# Глава 1.9. Практика. API пиццерии

В этом модуле мы разобрали основы Axum по отдельности. Теперь соберём их вместе и напишем первую версию API пиццерии. Проект будет расти от модуля к модулю: в конце каждого модуля мы будем дорабатывать его с помощью новых тем.

## Что должно уметь API

Пиццерия продаёт пиццы из меню и принимает заказы на доставку. Клиент выбирает пиццы, указывает имя и адрес, а сотрудники меняют статус заказа по мере его выполнения.

| Метод и путь                | Что делает                                |
| --------------------------- | ----------------------------------------- |
| `GET /menu`                 | Возвращает меню                           |
| `GET /menu/{id}`            | Возвращает одну пиццу                     |
| `POST /orders`              | Создаёт заказ                             |
| `GET /orders`               | Возвращает список заказов                 |
| `GET /orders?status=new`    | Возвращает заказы с указанным статусом    |
| `GET /orders/{id}`          | Возвращает заказ целиком                  |
| `PATCH /orders/{id}/status` | Меняет статус заказа                      |

Заказ проходит путь от нового до доставленного:

```text
new → cooking → delivering → delivered
 ↓
cancelled
```

Отменить можно только новый заказ: пиццу, которая уже в печи, отменить нельзя. Любой другой переход, например из `delivered` обратно в `cooking`, сервер должен отклонить. Цену заказа считает сервер по меню, а не клиент. Если доверять цене из запроса, клиент сможет заказать пиццу за один рубль.

Создадим проект и добавим зависимости:

```bash
cargo new pizzeria
cd pizzeria
cargo add axum
cargo add tokio --features macros,rt-multi-thread
cargo add serde --features derive
cargo add serde_json anyhow thiserror dotenvy
```

Код разложим по модулям:

```text
src/
├── main.rs       — настройки и запуск сервера
├── error.rs      — тип ошибки AppError
├── models.rs     — пиццы, заказы и их проверка
├── state.rs      — состояние приложения
└── handlers.rs   — хендлеры
```

## Модели

Начнём с типов данных. Пицца в меню содержит идентификатор, название и цену в рублях:

```rust
// src/models.rs
use serde::{Deserialize, Serialize};

use crate::error::AppError;

#[derive(Clone, Serialize)]
pub struct Pizza {
    pub id: u32,
    pub name: String,
    pub price: u32,
}
```

Статус заказа — `enum`. Атрибут `rename_all` превращает варианты в строки `new`, `cooking` и так далее, а метод `can_change_to` описывает разрешённые переходы:

```rust
#[derive(Clone, Copy, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum OrderStatus {
    New,
    Cooking,
    Delivering,
    Delivered,
    Cancelled,
}

impl OrderStatus {
    pub fn can_change_to(self, next: OrderStatus) -> bool {
        use OrderStatus::*;
        matches!(
            (self, next),
            (New, Cooking)
                | (New, Cancelled)
                | (Cooking, Delivering)
                | (Delivering, Delivered)
        )
    }

    pub fn as_str(self) -> &'static str {
        match self {
            OrderStatus::New => "new",
            OrderStatus::Cooking => "cooking",
            OrderStatus::Delivering => "delivering",
            OrderStatus::Delivered => "delivered",
            OrderStatus::Cancelled => "cancelled",
        }
    }
}
```

Макрос `matches!` сравнивает пару из текущего и нового статуса с перечисленными шаблонами. Все переходы описаны в одном месте, поэтому правила легко прочитать и поменять. Метод `as_str` понадобится для сообщений об ошибках.

Данные нового заказа приходят в теле запроса. Клиент передаёт только идентификаторы пицц и их количество, а названия и цены сервер возьмёт из меню:

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub struct NewOrder {
    pub customer: String,
    pub address: String,
    pub items: Vec<NewOrderItem>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)] // отклонит запрос с лишним полем
pub struct NewOrderItem {
    pub pizza_id: u32,
    pub quantity: u32,
}

impl NewOrder {
    pub fn validate(&self) -> Result<(), AppError> {
        if self.customer.trim().is_empty() {
            return Err(validation("не указано имя клиента"));
        }
        if self.address.trim().is_empty() {
            return Err(validation("не указан адрес доставки"));
        }
        if self.items.is_empty() {
            return Err(validation("в заказе нет ни одной пиццы"));
        }
        let quantity_ok = |item: &NewOrderItem| (1..=10).contains(&item.quantity);
        if !self.items.iter().all(quantity_ok) {
            let reason = "количество каждой пиццы — от 1 до 10";
            return Err(validation(reason));
        }
        Ok(())
    }
}

fn validation(reason: &str) -> AppError {
    AppError::Validation(reason.to_string())
}
```

Созданный заказ хранит позиции вместе с названиями и ценами на момент заказа. Если позже цена в меню изменится, сумма старых заказов останется прежней:

```rust
#[derive(Clone, Serialize)]
pub struct OrderItem {
    pub pizza_id: u32,
    pub name: String,
    pub price: u32,
    pub quantity: u32,
}

#[derive(Clone, Serialize)]
pub struct Order {
    pub id: u64,
    pub customer: String,
    pub address: String,
    pub status: OrderStatus,
    pub items: Vec<OrderItem>,
    pub total: u32,
}
```

В списке заказов позиции не нужны, поэтому для него заведём отдельный, более короткий тип. Осталось описать параметр запроса для фильтра по статусу и тело запроса для смены статуса:

```rust
#[derive(Serialize)]
pub struct OrderSummary {
    pub id: u64,
    pub customer: String,
    pub status: OrderStatus,
    pub total: u32,
}

#[derive(Deserialize)]
pub struct OrderFilter {
    pub status: Option<OrderStatus>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub struct StatusUpdate {
    pub status: OrderStatus,
}
```

Поле `status` в `OrderFilter` необязательное: без параметра `?status=` вернутся все заказы.

## Ошибки

Напишим свой тип для описания возможных ошибок в API. Отсутствующей может оказаться и пицца, и заказ, поэтому вариант `NotFound` теперь хранит текст сообщения. А для запрещённой смены статуса добавим вариант `Conflict`, которому соответствует ответ `409 Conflict`:

```rust
// src/error.rs
use axum::{
    http::StatusCode,
    response::{IntoResponse, Response},
    Json,
};
use serde_json::json;

#[derive(Debug, thiserror::Error)]
pub enum AppError {
    #[error("{0}")]
    NotFound(&'static str),
    #[error("{0}")]
    Conflict(String),
    #[error("некорректные данные: {0}")]
    Validation(String),
    #[error(transparent)]
    Internal(#[from] anyhow::Error),
}

impl IntoResponse for AppError {
    fn into_response(self) -> Response {
        let status = match &self {
            AppError::NotFound(_) => StatusCode::NOT_FOUND,
            AppError::Conflict(_) => StatusCode::CONFLICT,
            AppError::Validation(_) => StatusCode::BAD_REQUEST,
            AppError::Internal(_) => {
                StatusCode::INTERNAL_SERVER_ERROR
            }
        };
        let message = match &self {
            AppError::Internal(err) => {
                eprintln!("внутренняя ошибка: {err:#}");
                "внутренняя ошибка сервера".to_string()
            }
            other => other.to_string(),
        };
        let body = Json(json!({ "error": message }));
        (status, body).into_response()
    }
}
```

## Состояние

Меню не меняется во время работы, поэтому его достаточно обернуть в `Arc`. Заказы создаются и меняются из разных запросов, поэтому они лежат под `Mutex`. Вместе с заказами под мьютексом хранится счётчик для выдачи идентификаторов:

```rust
// src/state.rs
use std::{
    collections::BTreeMap,
    sync::{Arc, Mutex, MutexGuard},
};

use anyhow::anyhow;

use crate::error::AppError;
use crate::models::{Order, Pizza};

#[derive(Default)]
pub struct Orders {
    pub next_id: u64,
    pub by_id: BTreeMap<u64, Order>,
}

#[derive(Clone)]
pub struct AppState {
    pub menu: Arc<Vec<Pizza>>,
    pub orders: Arc<Mutex<Orders>>,
}

impl AppState {
    pub fn new() -> Self {
        Self {
            menu: Arc::new(menu()),
            orders: Arc::new(Mutex::new(Orders::default())),
        }
    }

    pub fn orders(&self) -> Result<MutexGuard<'_, Orders>, AppError> {
        let orders = self
            .orders
            .lock()
            .map_err(|_| anyhow!("мьютекс отравлен"))?;
        Ok(orders)
    }
}

fn menu() -> Vec<Pizza> {
    let pizza = |id, name: &str, price| Pizza {
        id,
        name: name.to_string(),
        price,
    };
    vec![
        pizza(1, "Маргарита", 490),
        pizza(2, "Пепперони", 590),
        pizza(3, "Четыре сыра", 650),
        pizza(4, "Гавайская", 550),
    ]
}
```

`BTreeMap` хранит заказы отсортированными по идентификатору, поэтому список заказов всегда выводится по порядку. Метод `orders` захватывает мьютекс и превращает ошибку отравленного мьютекса во внутреннюю ошибку сервера, чтобы не повторять это в каждом хендлере.

## Хендлеры

Хендлеры меню просто отдают данные из состояния:

```rust
// src/handlers.rs
use axum::{
    extract::{Path, Query, State},
    http::StatusCode,
    Json,
};

use crate::error::AppError;
use crate::models::{
    NewOrder, Order, OrderFilter, OrderItem, OrderStatus,
    OrderSummary, Pizza, StatusUpdate,
};
use crate::state::AppState;

pub async fn list_menu(
    State(state): State<AppState>,
) -> Json<Vec<Pizza>> {
    Json(state.menu.to_vec())
}

pub async fn get_pizza(
    State(state): State<AppState>,
    Path(id): Path<u32>,
) -> Result<Json<Pizza>, AppError> {
    let pizza = state
        .menu
        .iter()
        .find(|pizza| pizza.id == id)
        .ok_or(AppError::NotFound("пицца не найдена"))?;
    Ok(Json(pizza.clone()))
}
```

Создание заказа самый большой хендлер. Он проверяет данные, находит каждую пиццу в меню, считает сумму и только потом сохраняет заказ:

```rust
pub async fn create_order(
    State(state): State<AppState>,
    Json(new): Json<NewOrder>,
) -> Result<(StatusCode, Json<Order>), AppError> {
    new.validate()?;

    let mut items = Vec::new();
    for item in &new.items {
        let pizza = state
            .menu
            .iter()
            .find(|pizza| pizza.id == item.pizza_id)
            .ok_or_else(|| {
                AppError::Validation(format!(
                    "пиццы с id {} нет в меню",
                    item.pizza_id
                ))
            })?;
        items.push(OrderItem {
            pizza_id: pizza.id,
            name: pizza.name.clone(),
            price: pizza.price,
            quantity: item.quantity,
        });
    }
    let total = items.iter().map(|i| i.price * i.quantity).sum();

    let mut orders = state.orders()?;
    orders.next_id += 1;
    let order = Order {
        id: orders.next_id,
        customer: new.customer,
        address: new.address,
        status: OrderStatus::New,
        items,
        total,
    };
    orders.by_id.insert(order.id, order.clone());
    Ok((StatusCode::CREATED, Json(order)))
}
```

Неизвестная пицца в заказе — ошибка в данных клиента, поэтому здесь ответ `400 Bad Request`, а не `404 Not Found`. Мьютекс захватывается только в конце, когда заказ уже собран, и освобождается при выходе из функции. Чем меньше кода выполняется под мьютексом, тем меньше запросы ждут друг друга.

Получение заказа и список заказов только читают данные:

```rust
pub async fn get_order(
    State(state): State<AppState>,
    Path(id): Path<u64>,
) -> Result<Json<Order>, AppError> {
    let orders = state.orders()?;
    let order = orders
        .by_id
        .get(&id)
        .ok_or(AppError::NotFound("заказ не найден"))?;
    Ok(Json(order.clone()))
}

pub async fn list_orders(
    State(state): State<AppState>,
    Query(filter): Query<OrderFilter>,
) -> Result<Json<Vec<OrderSummary>>, AppError> {
    let orders = state.orders()?;
    let summaries = orders
        .by_id
        .values()
        .filter(|o| filter.status.is_none_or(|s| o.status == s))
        .map(|o| OrderSummary {
            id: o.id,
            customer: o.customer.clone(),
            status: o.status,
            total: o.total,
        })
        .collect();
    Ok(Json(summaries))
}
```

Метод `is_none_or` возвращает `true`, если фильтр не задан, а иначе сравнивает статус заказа с указанным.

Смена статуса проверяет переход и меняет заказ прямо в хранилище через `get_mut`:

```rust
pub async fn update_status(
    State(state): State<AppState>,
    Path(id): Path<u64>,
    Json(update): Json<StatusUpdate>,
) -> Result<Json<Order>, AppError> {
    let mut orders = state.orders()?;
    let order = orders
        .by_id
        .get_mut(&id)
        .ok_or(AppError::NotFound("заказ не найден"))?;
    if !order.status.can_change_to(update.status) {
        return Err(AppError::Conflict(format!(
            "нельзя перевести заказ из статуса {} в {}",
            order.status.as_str(),
            update.status.as_str()
        )));
    }
    order.status = update.status;
    Ok(Json(order.clone()))
}
```

Проверка и изменение статуса происходят под одним захватом мьютекса. Поэтому два одновременных запроса не смогут, например, одновременно отменить заказ и отправить его готовиться.

## Запуск

В `main` читаем настройки из переменных окружения и собираем роутер:

```rust
// src/main.rs
mod error;
mod handlers;
mod models;
mod state;

use anyhow::Context;
use axum::{
    routing::{get, patch},
    Router,
};
use tokio::net::TcpListener;

use crate::state::AppState;

struct Config {
    host: String,
    port: u16,
}

impl Config {
    fn from_env() -> anyhow::Result<Self> {
        let host = std::env::var("HOST")
            .unwrap_or_else(|_| "127.0.0.1".to_string());
        let port = std::env::var("PORT")
            .unwrap_or_else(|_| "3000".to_string())
            .parse()
            .context("PORT должен быть числом от 0 до 65535")?;
        Ok(Self { host, port })
    }
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    dotenvy::dotenv().ok();
    let config = Config::from_env()?;

    let router = Router::new()
        .route("/menu", get(handlers::list_menu))
        .route("/menu/{id}", get(handlers::get_pizza))
        .route(
            "/orders",
            get(handlers::list_orders).post(handlers::create_order),
        )
        .route("/orders/{id}", get(handlers::get_order))
        .route("/orders/{id}/status", patch(handlers::update_status))
        .with_state(AppState::new());

    let addr = format!("{}:{}", config.host, config.port);
    let listener = TcpListener::bind(&addr).await?;
    println!("пиццерия работает на {addr}");
    axum::serve(listener, router).await?;
    Ok(())
}
```

## Проверка

Запустим сервер через `cargo run` и сделаем заказ:

```bash
curl -i -H 'content-type: application/json' \
    -d '{"customer":"Анна","address":"ул. Ленина, 1",
         "items":[{"pizza_id":1,"quantity":2},
                  {"pizza_id":3,"quantity":1}]}' \
    http://127.0.0.1:3000/orders
```

```text
HTTP/1.1 201 Created
{"id":1,"customer":"Анна","address":"ул. Ленина, 1","status":"new",
"items":[{"pizza_id":1,"name":"Маргарита","price":490,"quantity":2},
{"pizza_id":3,"name":"Четыре сыра","price":650,"quantity":1}],
"total":1630}
```

Сервер сам подставил названия и цены и посчитал сумму: две «Маргариты» по 490 рублей и одна «Четыре сыра» за 650. Отправим заказ готовиться:

```bash
curl -i -X PATCH -H 'content-type: application/json' \
    -d '{"status":"cooking"}' \
    http://127.0.0.1:3000/orders/1/status
```

```text
HTTP/1.1 200 OK
{"id":1,"customer":"Анна","address":"ул. Ленина, 1",
"status":"cooking",...}
```

Отменить его теперь не получится:

```bash
curl -i -X PATCH -H 'content-type: application/json' \
    -d '{"status":"cancelled"}' \
    http://127.0.0.1:3000/orders/1/status
```

```text
HTTP/1.1 409 Conflict
{"error":"нельзя перевести заказ из статуса cooking в cancelled"}
```

Заказ с пиццей, которой нет в меню, сервер тоже отклонит:

```bash
curl -i -H 'content-type: application/json' \
    -d '{"customer":"Анна","address":"ул. Ленина, 1",
         "items":[{"pizza_id":7,"quantity":1}]}' \
    http://127.0.0.1:3000/orders
```

```text
HTTP/1.1 400 Bad Request
{"error":"некорректные данные: пиццы с id 7 нет в меню"}
```

Список заказов можно отфильтровать по статусу:

```bash
curl 'http://127.0.0.1:3000/orders?status=cooking'
```

```text
[{"id":1,"customer":"Анна","status":"cooking","total":1630}]
```

Полный код этой главы можно посмотреть в [ветке `v1-api`](https://github.com/Jadedboat780/Pizzeria-backend/tree/v1-api) репозитория.

Пиццерия уже принимает заказы, но все они пропадут при перезапуске сервера. В следующем модуле разберёмся как подключить к пиццерии базу данных.
