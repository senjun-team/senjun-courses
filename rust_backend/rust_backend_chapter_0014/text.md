# Глава 1.4. Хендлеры и ответы

Хендлер в Axum — это обычная асинхронная функция:

```rust
async fn hello() -> &'static str {
    "Hello, World!"
}
```

Фреймворк принимает такую функцию, если выполняются два условия: все её аргументы можно извлечь из запроса, а возвращаемое значение можно превратить в HTTP-ответ. Первое обеспечивают трейты `FromRequest` и `FromRequestParts` (глава 1.5), а второе — трейт `IntoResponse`.

## Что можно вернуть из хендлера

Трейт `IntoResponse` описывает, как значение превращается в HTTP-ответ. Он уже реализован для большинства типов, которые могут понадобиться:

| Возвращаем               | Статус                   | `content-type`                  |
| ------------------------ | ------------------------ | ------------------------------- |
| `&'static str`, `String` | 200                      | `text/plain; charset=utf-8`     |
| `Json<T>`                | 200                      | `application/json`              |
| `Html<T>`                | 200                      | `text/html; charset=utf-8`      |
| `Vec<u8>`, `Bytes`       | 200                      | `application/octet-stream`      |
| `()`                     | 200                      | тела нет                        |
| `StatusCode`             | переданный код           | тела нет                        |
| `Redirect`               | 303, 307 или 308         | тела нет, есть заголовок `Location` |
| `Response`               | задаётся при создании    | задаётся при создании           |

Заголовок `content-type` выставляется в зависимости от типа возвращаемого значения, поэтому отдельно указывать его обычно не нужно. Именно поэтому в главе 1.1 в ответе `curl` появились заголовки, которые мы нигде не писали.

## Статус и заголовки

Если нужно задать статус или заголовки ответа, их возвращают вместе с телом в кортеже. Последний элемент кортежа — тело ответа, а все элементы перед ним — статус и заголовки:

```rust
use axum::{
    http::{header, StatusCode},
    response::IntoResponse,
    Json,
};

async fn create_user() -> (StatusCode, Json<UserResponse>) {
    (StatusCode::CREATED, Json(user))
}

async fn delete_user() -> StatusCode {
    StatusCode::NO_CONTENT
}

async fn export() -> impl IntoResponse {
    (
        [(header::CONTENT_TYPE, "text/csv")],
        "id,name\n1,Alice",
    )
}
```

Статус и заголовки необязательны, но порядок фиксированный: сначала статус, потом заголовки, последним тело. Заголовки из кортежа заменяют те, что тип тела выставил сам, поэтому в `export` вместо `text/plain` уйдёт `text/csv`.

## Трейт `IntoResponse` 

Иногда хендлер может вернуть несколько разных вариантов ответа. В таком случае удобно указать в качестве возвращаемого типа `impl IntoResponse`:

```rust
async fn get_user() -> impl IntoResponse {
    if let Some(user) = find_user() {
        Json(user).into_response()
    } else {
        StatusCode::NOT_FOUND.into_response()
    }
}
```

Здесь ветки возвращают значения разных типов, поэтому каждое приводится к общему типу `Response` через `.into_response()`. Этот вызов обязателен: `impl IntoResponse` означает один конкретный тип, и без приведения код бы не скомпилировался.

## Свой тип ответа

Трейт `IntoResponse` можно реализовать для собственного типа. Для этого нужна одна функция, которая превращает значение в `Response`:

```rust
struct Created<T>(T);

impl<T: Serialize> IntoResponse for Created<T> {
    fn into_response(self) -> Response {
        (StatusCode::CREATED, Json(self.0)).into_response()
    }
}

async fn create_user() -> Created<UserResponse> {
    Created(user)
}
```

Такой подход полезен, когда один и тот же формат ответа используется в нескольких хендлерах.

Мы разобрались, что хендлер может вернуть. В следующей главе посмотрим на другую сторону: как хендлер получает данные из запроса.
