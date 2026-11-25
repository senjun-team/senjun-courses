# Глава 1.3. Роутинг

Маршрутизация (роутинг) — это сопоставление запроса по его HTTP-методу и пути с конкретным хендлером, который этот запрос обработает. Axum использует декларативный подход к маршрутизации: вы не пишете `if path == "/users"`, а описываете таблицу соответствий «путь + метод → функция». `Router` — обычный тип, поэтому маршруты можно собирать из нескольких функций или модулей.

## Маршруты

Базовый синтаксис выглядит так:

```rust
use axum::{routing::get, Router};

let app = Router::new()
    .route("/", get(root))
    .route("/users", get(list_users).post(create_user))
    .route("/users/{id}", get(get_user).delete(delete_user));
```

Функция `route` принимает путь и структуру `MethodRouter` — таблицу соответствий между HTTP-методами и хендлерами. Например, `.route("/users", get(list_users).post(create_user))` означает:

```text
GET  /users -> list_users
POST /users -> create_user
```

Модуль `axum::routing` предоставляет функции для каждого HTTP-метода: `get`, `post`, `put`, `patch`, `delete` и т.д. Отдельно объявлять `head` не нужно: `get` обслуживает и его. Для любого метода есть функция `any`, а для нескольких методов сразу — `on`.

Один путь нельзя добавить в роутер дважды:

```rust
Router::new()
    .route("/users", get(list_users))
    .route("/users", post(create_user)); // panic
```

## Параметры пути

Часть пути можно сделать параметром, указав его имя в фигурных скобках. Значение извлекается экстрактором `Path`:

```rust
use axum::extract::Path;

async fn get_user(Path(id): Path<u64>) -> String {
    format!("user {id}")
}

let app = Router::new().route("/users/{id}", get(get_user));
```

Если параметров несколько, их извлекают кортежем в том порядке, в котором они идут в пути:

```rust
// маршрут /users/{user_id}/posts/{post_id}
async fn get_post(
    Path((user_id, post_id)): Path<(u64, u64)>,
) {}
```

Весь оставшийся путь целиком захватывает параметр со звёздочкой:

```rust
.route("/assets/{*path}", get(serve_asset))
```

Например, для запроса `/assets/css/main.css` в `path` попадёт значение `css/main.css`.

Вся особенность такого параметра — в звёздочке перед именем: она означает, что параметр захватывает весь оставшийся путь вместе со слэшами. Само имя может быть любым: `path` здесь не ключевое слово, а просто название параметра, как `id` в примерах выше.

Подробнее про экстракторы поговорим в главе 1.5.

### Приоритет маршрутов

```rust
.route("/users/me", get(current_user))
.route("/users/{id}", get(get_user))
```

Запрос `GET /users/me` попадёт в `current_user`. Причём порядок объявления маршрутов не важен, поэтому `/users/{id}` и `/users/me` можно поменять местами без изменения поведения. При этом маршруты, совпадающие по структуре, запрещены, даже если у параметров разные имена: `/users/{id}` и `/users/{user_id}` в одном роутере вызовут панику при запуске.

## 404, 405 и fallback

Axum различает два случая:

- `404 Not Found` — такого пути нет.
- `405 Method Not Allowed` — путь существует, но для него не зарегистрирован данный HTTP-метод.

Например, если зарегистрирован только `GET /users`, то `POST /users` получит `405` и заголовок `Allow` со списком доступных методов, а `GET /unknown` получит `404`. Полный список HTTP-методов и кодов ответа можно посмотреть на [MDN](https://developer.mozilla.org/ru/docs/Web/HTTP).

Учтите, что `/users` и `/users/` — это разные пути. По сложившемуся соглашению путь со слэшем на конце обозначает каталог, а без слэша — отдельный ресурс, как файл в файловой системе. Поэтому, если зарегистрирован только `/users`, запрос к `/users/` получит `404`: автоматического перенаправления между ними Axum не делает.

Для API часто полезно настроить собственный `fallback` — хендлер, который вызывается, когда ни один маршрут не подошёл:

```rust
use axum::{http::StatusCode, response::IntoResponse, Json};
use serde_json::json;

async fn not_found() -> impl IntoResponse {
    let body = Json(json!({ "error": "not found" }));
    (StatusCode::NOT_FOUND, body)
}

let app = Router::new()
    .route("/", get(root))
    .fallback(not_found);
```

## Основные методы `Router`

Помимо `route`, у `Router` есть несколько методов, которые часто используются при построении приложения.

### `merge`

Метод `merge` объединяет два роутера, сохраняя их пути:

```rust
let users = Router::new().route("/users", get(list_users));
let posts = Router::new().route("/posts", get(list_posts));

let app = Router::new().merge(users).merge(posts);
```

Если в объединяемых роутерах окажутся одинаковые пути, при запуске произойдёт паника.

### `nest`

Метод `nest` монтирует роутер под определённым префиксом:

```rust
let users = Router::new()
    .route("/", get(list_users))
    .route("/{id}", get(get_user));

let app = Router::new()
    .nest("/users", users);
```

В результате получатся маршруты:

```text
GET /users
GET /users/{id}
```

Вложенный роутер не знает о своём префиксе, поэтому его можно смонтировать и в другое место, например под `/api/users`. Благодаря `merge` и `nest` маршруты удобно раскладывать по модулям: каждый модуль описывает свой роутер, а в `main` они собираются вместе.

## Практика

Опишем маршруты API блога в отдельном роутере и смонтируем его под префиксом `/api`. Маршрут `/api/posts/{id}` возвращает пост по идентификатору, а `/api/users/{user_id}/posts/{post_id}` — пост конкретного пользователя. На запросы к остальным путям сервер отвечает ошибкой в формате JSON.

```rust
use axum::{
    extract::Path, http::StatusCode, response::IntoResponse,
    routing::get, Json, Router,
};
use serde_json::json;

...
    let api = Router::new()
        .route("/posts/{id}", get(post))
        .route("/users/{user_id}/posts/{post_id}", get(user_post));

    let router = Router::new()
        .route("/", get(hello))
        .nest("/api", api)
        .fallback(not_found);
...

async fn post(Path(id): Path<u32>) -> String {
    format!("пост {id}")
}

// несколько параметров пути извлекаются кортежем
async fn user_post(
    Path((user_id, post_id)): Path<(u32, u32)>,
) -> String {
    format!("пост {post_id} пользователя {user_id}")
}

async fn not_found() -> impl IntoResponse {
    let body = Json(json!({ "error": "такой страницы нет" }));
    (StatusCode::NOT_FOUND, body)
}
```

И проверка:

```bash
curl -i http://127.0.0.1:3000/api/users/7/posts/42
```

```text
HTTP/1.1 200 OK
content-type: text/plain; charset=utf-8
content-length: 38
date: Wed, 25 Nov 2026 16:53:44 GMT

пост 42 пользователя 7
```

Если идентификатор не число, Axum сам ответит ошибкой:

```bash
curl -i http://127.0.0.1:3000/api/posts/abc
```

```text
HTTP/1.1 400 Bad Request
content-type: text/plain; charset=utf-8
content-length: 42
date: Wed, 25 Nov 2026 16:53:44 GMT

Invalid URL: Cannot parse `abc` to a `u32`
```

А запрос к несуществующему пути попадёт в `fallback`:

```bash
curl -i http://127.0.0.1:3000/feed
```

```text
HTTP/1.1 404 Not Found
content-type: application/json
content-length: 46
date: Wed, 25 Nov 2026 16:53:44 GMT

{"error":"такой страницы нет"}
```

Теперь мы знаем, как запрос попадает в нужный хендлер. В следующей главе разберёмся, что хендлер может вернуть и как Axum превращает это в HTTP-ответ.
