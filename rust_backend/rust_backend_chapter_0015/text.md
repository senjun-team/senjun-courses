# Глава 1.5. Экстракторы

Экстракторы — это типы, которые извлекают данные из HTTP-запроса и передают их в хендлер в качестве аргументов. Хендлер просто объявляет в сигнатуре, что ему нужно, а Axum сам разбирает запрос. Если извлечь данные не удалось, хендлер не вызывается, а клиент получает ошибку.

## Встроенные экстракторы

| Экстрактор        | Что извлекает                            |
| ----------------- | ---------------------------------------- |
| `Path<T>`         | параметры пути `/users/{id}` (глава 1.3) |
| `Query<T>`        | параметры строки запроса `/users?page=2` |
| `Json<T>`         | тело запроса в формате JSON (глава 1.2)  |
| `Form<T>`         | тело HTML-формы                          |
| `HeaderMap`       | все заголовки запроса                    |
| `Method`, `Uri`   | HTTP-метод и адрес запроса               |
| `String`, `Bytes` | тело запроса как есть                    |
| `State<T>`        | общее состояние приложения (глава 1.6)   |

Помимо встроенных экстракторов, есть дополнительные в крейте `axum-extra`, например для типизированных заголовков и cookies.

## Параметры строки запроса

`Query<T>` разбирает параметры после `?` в структуру. Например, так выглядит пагинация:

```rust
use axum::extract::Query;
use serde::Deserialize;

#[derive(Deserialize)]
struct Pagination {
    page: Option<u32>,
    limit: Option<u32>,
}

async fn list_users(
    Query(pagination): Query<Pagination>,
) -> String {
    let page = pagination.page.unwrap_or(1);
    let limit = pagination.limit.unwrap_or(20);
    format!("page {page}, limit {limit}")
}
// /users              -> page 1, limit 20
// /users?page=3       -> page 3, limit 20
// /users?page=abc     -> 400 Bad Request
```

Если параметр необязательный, сделайте поле типом `Option` или добавьте к нему атрибут `#[serde(default)]`.

## Заголовки

`HeaderMap` даёт доступ ко всем заголовкам запроса:

```rust
use axum::http::{header, HeaderMap};

async fn user_agent(headers: HeaderMap) -> String {
    headers
        .get(header::USER_AGENT)
        .and_then(|value| value.to_str().ok())
        .unwrap_or("unknown")
        .to_string()
}
```

Значение заголовка — это `HeaderValue`, а не строка, потому что HTTP допускает в заголовках байты, которые не являются корректным UTF-8. Поэтому перед использованием его нужно преобразовать в строку.

## Несколько экстракторов

Хендлер может принимать сразу несколько экстракторов:

```rust
async fn update_user(
    Path(id): Path<u64>,
    Query(params): Query<Pagination>,
    headers: HeaderMap,
    Json(user): Json<UpdateUser>, // тело всегда последним
) -> StatusCode {
    StatusCode::NO_CONTENT
}
```

Экстракторы делятся на два вида:

1) `FromRequestParts` — читают только части запроса: метод, адрес, заголовки и состояние. Например, `Path`, `Query`, `HeaderMap`, `State`.
2) `FromRequest` — могут читать тело запроса. Например, `Json`, `Form`, `String`, `Bytes`.

## Свой экстрактор

Экстрактор — это любой тип, реализующий `FromRequestParts` или `FromRequest`. Например, вот экстрактор, который достаёт заголовок `User-Agent` и возвращает `400`, если заголовка нет:

```rust
use axum::{
    extract::FromRequestParts,
    http::{header, request::Parts, StatusCode},
};

struct UserAgent(String);

impl<S: Send + Sync> FromRequestParts<S> for UserAgent {
    type Rejection = (StatusCode, &'static str);

    async fn from_request_parts(
        parts: &mut Parts,
        _state: &S,
    ) -> Result<Self, Self::Rejection> {
        parts
            .headers
            .get(header::USER_AGENT)
            .and_then(|value| value.to_str().ok())
            .map(|value| UserAgent(value.to_string()))
            .ok_or((StatusCode::BAD_REQUEST, "missing user-agent"))
    }
}

async fn handler(UserAgent(agent): UserAgent) -> String {
    agent
}
```

`Rejection` — это тип ошибки, которую получит клиент, поэтому он должен реализовывать `IntoResponse`.

Свои экстракторы позволяют убрать повторяющийся код из хендлеров. Например, в модуле про аутентификацию мы напишем экстрактор `AuthUser`, который проверяет токен, и хендлер сможет получить текущего пользователя просто как аргумент.

Теперь у нас есть всё, чтобы принять запрос и вернуть ответ. В следующей главе разберём состояние приложения: как передать хендлерам общие данные, например подключение к базе данных.
