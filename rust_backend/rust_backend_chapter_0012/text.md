# Глава 1.2. Сериализация и десериализация JSON

Практически каждый хендлер в API разбирает JSON на входе и собирает его на выходе. В экосистеме Rust стандартом для сериализации и десериализации является `serde`.

Сериализация — это преобразование данных из Rust во внешний формат, например в JSON. Десериализация — обратный процесс, из JSON в Rust.

Сам `serde` не умеет работать ни с JSON, ни с другими форматами. Он задаёт общий интерфейс, а конкретные форматы реализуются отдельными крейтами: `serde_json`, `toml`, `rmp-serde` и другими. Благодаря этому одну структуру можно использовать с разными форматами.

Добавьте новые зависимости в `Cargo.toml`:

```bash
cargo add serde --features derive
cargo add serde_json
```

Фича `derive` позволяет указывать трейты serde в атрибуте `#[derive]`, чтобы компилятор сам генерировал их реализацию для наших типов.

## Основные трейты

У serde два трейта: `Serialize` для сериализации и `Deserialize` для десериализации. Вручную их обычно не реализуют, а используют макрос `derive`:

```rust
use serde::{Deserialize, Serialize};

// генерирует реализацию трейтов на этапе компиляции
#[derive(Serialize, Deserialize)]
struct User {
    id: i64,
    name: String,
}

// Rust-значение в JSON
let json = serde_json::to_string(&user)?;
// JSON в Rust-значение
let user: User = serde_json::from_str(&json)?;
```

## Основные атрибуты

По умолчанию serde работает по двум правилам: все поля обязательные (если поля нет, будет ошибка), а лишние поля молча игнорируются. Большинство атрибутов нужны, чтобы изменить это поведение.

### Переименование полей

Атрибут `rename_all` переименовывает все поля в нужный формат, а `rename` — одно поле:

```rust
#[derive(Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct User {
    first_name: String,
    #[serde(rename = "type")]
    kind: String,
}
// {"firstName":"...","type":"..."}
```

Кроме `camelCase`, есть `PascalCase`, `kebab-case`, `SCREAMING_SNAKE_CASE` и другие.

### Необязательные поля

Если поле может отсутствовать, проще всего сделать его типом `Option<T>`: отсутствующее поле и `null` одинаково дают `None`. Атрибут `default` вместо этого подставляет значение по умолчанию из трейта `Default` или из вашей функции. А атрибут `skip_serializing_if` не выводит поле, если выполняется условие. Без него поле со значением `None` попадёт в JSON как `"email": null`.

```rust
#[derive(Serialize, Deserialize)]
struct User {
    name: String,
    #[serde(default)]
    is_admin: bool, // false
    #[serde(default = "default_role")]
    role: String,   // "user"
    #[serde(skip_serializing_if = "Option::is_none")]
    email: Option<String>,
}

fn default_role() -> String {
    "user".to_string()
}
// на входе достаточно {"name":"Alice"}
// на выходе {"name":"Alice","is_admin":false,"role":"user"}
```

### Пропуск полей

Атрибут `skip` полностью исключает поле из сериализации и десериализации. При десериализации вместо значения используется `Default::default()`. Это удобно для внутренних полей, которые не должны передаваться через API:

```rust
#[derive(Serialize, Deserialize)]
struct User {
    name: String,
    #[serde(skip)]
    password_hash: String,
}
```

### Строгая проверка

Из-за того, что лишние поля игнорируются, опечатка клиента проходит незамеченной. Атрибут `deny_unknown_fields` превращает неизвестное поле в ошибку:

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct UpdateUser {
    name: Option<String>,
}
// {"nmae":"Bob"}
// без атрибута: name = None,
//     обновление ничего не сделало, ответ 200 OK
// с атрибутом: ошибка unknown field `nmae`
```

### Вложенные структуры

Атрибут `flatten` помещает поля вложенной структуры на один уровень с остальными:

```rust
#[derive(Serialize, Deserialize)]
struct User {
    name: String,
    #[serde(flatten)]
    metadata: Metadata,
}

#[derive(Serialize, Deserialize)]
struct Metadata {
    created_at: String,
}
// {"name":"Alice","created_at":"2026-09-24"}
```

### Перечисления

Простые перечисления, то есть перечисления без полей, сериализуются в строку с названием варианта. Это удобно, например, для статусов заказа. Чтобы название было в привычном для JSON виде, добавьте `rename_all`:

```rust
#[derive(Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
enum OrderStatus {
    New,        // "new"
    InProgress, // "in_progress"
    Delivered,  // "delivered"
}
```

С перечислениями, у вариантов которых есть поля, всё сложнее. Например, событие в интерфейсе может быть кликом или нажатием клавиши:

```rust
#[derive(Serialize, Deserialize)]
enum Event {
    Click { x: i32, y: i32 },
    KeyPress { key: String },
}
// {"Click":{"x":10,"y":20}}
// {"KeyPress":{"key":"Enter"}}
```

По умолчанию название варианта становится ключом объекта, а поля оказываются внутри. Клиенту такой формат разбирать неудобно: чтобы понять, что за событие пришло, приходится проверять, какой из возможных ключей есть в объекте.

Обычно в API тип передают в отдельном поле, например `type`. Для этого служит атрибут `tag`:

```rust
#[derive(Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
enum Event {
    Click { x: i32, y: i32 },
    KeyPress { key: String },
}
// {"type":"click","x":10,"y":20}
// {"type":"key_press","key":"Enter"}
```

Теперь у всех событий есть поле `type`, по которому клиент сразу понимает, как разбирать остальные поля. Десериализация работает так же: serde выбирает вариант перечисления по значению поля `type`.

## `serde_json::Value` и `json!`

Когда структура JSON заранее неизвестна, используют `serde_json::Value` — тип, представляющий произвольное JSON-значение:

```rust
let value: Value = serde_json::from_str(r#"{"name": "Alice"}"#)?;

println!("{}", value["name"]); // "Alice" — вместе с кавычками
value["name"].as_str();        // Some("Alice")
```

При обращении по несуществующему ключу `Value` вернёт `null`:

```rust
println!("{}", value["age"]); // null
```

Макрос `json!` позволяет создавать `Value` из JSON-подобного литерала. Внутри него можно использовать не только литералы, но и переменные: их значения сериализуются автоматически. Например, так можно собрать ответ с данными о работе сервера:

```rust
use serde_json::json;

fn count_tasks_and_workers() -> (usize, usize) {
    // дескриптор текущего рантайма Tokio
    let handle = tokio::runtime::Handle::current();
    // количество рабочих потоков
    let workers = handle.metrics().num_workers();
    // количество выполняемых задач на текущий момент
    let tasks = handle.metrics().num_alive_tasks();
    (tasks, workers)
}

// количество выполняемых задач и рабочих потоков
let (tasks, workers) = count_tasks_and_workers();

// формирование результирующего статуса
let status = json!({
    "status": "ok",
    "workers": workers,
    "tasks": tasks
});
// {"status":"ok","tasks":1,"workers":12}
```

Обратите внимание, что ключи в результате отсортированы по алфавиту: по умолчанию `Value` хранит поля объекта в отсортированном порядке.

Переменная внутри `json!` может быть любого типа, реализующего трейт `Serialize`: числом, строкой, вектором или вашей структурой с `#[derive(Serialize)]`.

Похожую строку можно собрать и через `format!`, но `json!` надёжнее. Во-первых, ошибка в структуре JSON внутри `json!`, например пропущенное значение, обнаружится ещё при компиляции, а строку из `format!` никто не проверяет. Во-вторых, `json!` правильно экранирует значения. Если в строке окажутся кавычки, `format!` соберёт некорректный JSON, и ошибка проявится только у клиента, когда тот попытается его разобрать:

```rust
let name = r#"Иван "Грозный""#;

// {"name": "Иван "Грозный""}
format!(r#"{{"name": "{}"}}"#, name);
// {"name":"Иван \"Грозный\""}
json!({ "name": name });
```

Злоупотреблять `Value` не стоит. Если структура JSON известна заранее, лучше описать её отдельным типом. Так вы получите проверку структуры на этапе компиляции и удобное автодополнение, а не ошибки доступа к полям во время выполнения.

## JSON в Axum

Функции `serde_json::to_string` и `serde_json::from_str` в хендлерах Axum вручную вызывать не нужно: Axum делает это сам с помощью обёртки `Json<T>`.

Напомним, что хендлер — это асинхронная функция, которая обрабатывает запрос. Данные запроса она получает через аргументы, а ответ формирует из возвращаемого значения. Аргументы, которые Axum заполняет данными из запроса, называются экстракторами. Одни экстракторы достают данные из адреса запроса, другие — из заголовков, третьи — из тела. Все они подробно разобраны в главе 1.5, а здесь рассмотрим простой случай: данные приходят в теле запроса в формате JSON.

### Тело запроса

Если указать `Json<T>` в аргументах хендлера, Axum прочитает тело запроса и десериализует его в тип `T`:

```rust
use axum::Json;
use serde::Deserialize;

#[derive(Deserialize)]
struct CreateUser {
    name: String,
}

async fn create_user(Json(user): Json<CreateUser>) -> String {
    format!("Получен пользователь с именем: {}", user.name)
}
```

Запись `Json(user)` в аргументах — это деструктуризация: она сразу достаёт из обёртки значение типа `CreateUser`.

Если тело не удалось разобрать, хендлер не вызывается, и Axum сам вернёт ошибку:

- `415` — нет заголовка `content-type: application/json`.
- `400` — синтаксическая ошибка в JSON.
- `422` — JSON корректный, но поля не те или не того типа.

В структурах запроса используйте `String`, а не `&str`, так как `Json<T>` требует тип, который ничего не заимствует из тела запроса.

### Тело ответа

Если вернуть `Json<T>` из хендлера, Axum сериализует значение в JSON и выставит заголовок `content-type: application/json`:

```rust
use axum::Json;
use serde::Serialize;

#[derive(Serialize)]
struct UserResponse {
    id: i64,
    name: String,
}

async fn get_user() -> Json<UserResponse> {
    let user = UserResponse {
        id: 1,
        name: "Alice".to_string(),
    };
    Json(user)
}
// {"id":1,"name":"Alice"}
```

Теперь мы умеем принимать и возвращать JSON. В следующей главе подробно разберём маршрутизацию: как описывать маршруты и как Axum выбирает хендлер для запроса.
