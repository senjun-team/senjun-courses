# Глава 1.6. Состояние приложения

Хендлерам часто нужны общие данные: подключение к базе данных, конфигурация приложения, клиент для внешнего API, кеш. Такие данные называют состоянием приложения. Создавать их заново на каждый запрос дорого, а глобальные переменные неудобны: их сложно подменить в тестах, и по сигнатуре хендлера не видно, от чего он зависит. В Axum состояние передаётся в хендлеры явно, через экстрактор `State`.

## Передача состояния в хендлер

Состояние описывается обычной структурой. Её передают роутеру методом `with_state`, а хендлер получает её через экстрактор `State<T>`:

```rust
use axum::{extract::State, routing::get, Router};

#[derive(Clone)]
struct AppState {
    greeting: String,
}

async fn hello(State(state): State<AppState>) -> String {
    state.greeting
}

#[tokio::main]
async fn main() {
    let state = AppState {
        greeting: "Привет!".to_string(),
    };

    let router = Router::new()
        .route("/", get(hello))
        .with_state(state);
    // запуск сервера
    // ...
}
```

Запрос `GET /` вернёт строку `Привет!`. Тип состояния указан прямо в сигнатуре хендлера, поэтому сразу видно, какие данные ему нужны.

## Проверка состояния при компиляции

Axum проверяет, что состояние передано роутеру, ещё на этапе компиляции. Пока `with_state` не вызван, роутер имеет тип `Router<AppState>`, то есть «роутер, которому ещё нужно состояние типа `AppState`».

Метод `with_state` превращает его в `Router<()>`, и только такой роутер можно передать в `axum::serve`. Если забыть вызвать `with_state`, программа не скомпилируется:

```text
error[E0277]: the trait bound `for<'a> Router<AppState>:
Service<IncomingStream<'a, TcpListener>>` is not satisfied
```

Сообщение выглядит пугающе, но главное в нём — тип `Router<AppState>`: он означает, что роутер всё ещё ждёт состояние.

## Клонирование состояния

Тип состояния обязан реализовывать трейт `Clone`: Axum клонирует состояние для каждого запроса, чтобы у каждого хендлера была своя копия. Без `Clone` компилятор выдаст ошибку:

```text
error[E0277]: the trait bound `AppState: Clone` is not satisfied
```

Клонировать строку на каждый запрос недорого, но у клонирования есть две проблемы. Во-первых, большие данные, например загруженный в память справочник, копировать на каждый запрос дорого. Во-вторых, у каждого запроса оказывается своя копия, и изменения, сделанные в одном хендлере, другие не увидят.

Эти проблемы решает [`Arc`](https://doc.rust-lang.org/std/sync/struct.Arc.html). При клонировании `Arc` копируется только указатель и увеличивается счётчик ссылок, а сами данные остаются в одном экземпляре:

```rust
use std::sync::Arc;

#[derive(Clone)]
struct AppState {
    catalog: Arc<Vec<String>>,
}
```

## Изменяемое состояние

`Arc` даёт общий доступ к данным только на чтение. Если хендлеры должны изменять общие данные, их нужно защитить от одновременного изменения из нескольких потоков. Для этого используют [`Mutex`](https://doc.rust-lang.org/std/sync/struct.Mutex.html): он пропускает к данным только один поток за раз, а остальные ждут своей очереди. Например, так выглядит список пользователей в памяти:

```rust
use std::sync::{Arc, Mutex};

use axum::{extract::State, http::StatusCode, Json};

#[derive(Clone)]
struct AppState {
    users: Arc<Mutex<Vec<String>>>,
}

async fn list_users(
    State(state): State<AppState>,
) -> Result<Json<Vec<String>>, StatusCode> {
    let users = state
        .users
        .lock()
        .unwrap();
    Ok(Json(users.clone()))
}

async fn create_user(
    State(state): State<AppState>,
    Json(user): Json<CreateUser>,
) -> Result<StatusCode, StatusCode> {
    let mut users = state
        .users
        .lock()
        .unwrap();
    users.push(user.name);
    Ok(StatusCode::CREATED)
}
```

Метод `lock` захватывает мьютекс и возвращает защитный объект, через который можно работать с данными. Мьютекс освобождается автоматически, когда этот объект выходит из области видимости.

Проверим:

```bash
curl -X POST -H 'content-type: application/json' \
    -d '{"name":"Alice"}' http://127.0.0.1:3000/users
curl http://127.0.0.1:3000/users
```

```text
["Alice"]
```

Такое хранилище живёт только в памяти процесса и пропадает при перезапуске сервера. Для учебных примеров этого достаточно, а настоящие данные мы будем хранить в базе данных.

## Часть состояния

По мере роста приложения у состояния появляется всё больше полей, а конкретному хендлеру обычно нужна только их часть. Чтобы хендлер зависел только от того, что ему действительно нужно, Axum позволяет извлекать из состояния отдельные поля. Для этого нужен макрос `FromRef`, который подключается фичей `macros`:

```bash
cargo add axum --features macros
```

```rust
use axum::extract::{FromRef, State};

#[derive(Clone)]
struct Config {
    greeting: String,
}

#[derive(Clone, FromRef)]
struct AppState {
    config: Config,
    users: Arc<Mutex<Vec<String>>>,
}

async fn hello(State(config): State<Config>) -> String {
    config.greeting
}
```

Роутеру по-прежнему передаётся всё состояние `AppState`:

```rust
let state = AppState {
    config: Config {
        greeting: "Привет!".to_string(),
    },
    users: Arc::new(Mutex::new(Vec::new())),
};

let router = Router::new()
    .route("/", get(hello))
    .with_state(state);
```

А хендлер `hello` при этом получает только `Config`. `FromRef` генерирует для каждого поля способ достать его из `AppState`, поэтому типы полей должны реализовывать `Clone`.

В следующей главе разберём, как описать собственный тип ошибки и превращать его в понятные клиенту HTTP-ответы.
