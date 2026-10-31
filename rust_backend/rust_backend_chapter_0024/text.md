# Глава 2.4. База данных в Axum и слои приложения

Мы умеем запускать базу данных, создавать таблицы и писать запросы, которые проверяются при компиляции. Осталось соединить это с сервером на Axum. В этой главе сделаем API для каталога книг и разберём, как разделить код на слои, чтобы он оставался понятным, когда приложение вырастет.

## Зачем делить код на слои

Проще всего писать SQL-запросы прямо в хендлерах: хендлер получает пул из состояния, выполняет запрос и возвращает ответ. Для пары маршрутов это работает, но с ростом приложения в одной функции смешиваются разные задачи: разбор HTTP-запроса и работа с базой данных. Такой код трудно читать, а одни и те же запросы приходится повторять в нескольких хендлерах.

Поэтому код бэкенда делят на слои, у каждого из которых своя ответственность:

```text
HTTP-запрос
    ↓
хендлер        — разбирает запрос и формирует HTTP-ответ
    ↓
репозиторий    — выполняет SQL-запросы
    ↓
PostgreSQL
```

Хендлер ничего не знает об SQL, а репозиторий — об HTTP. Если понадобится поменять запрос или структуру таблицы, изменения затронут только репозиторий, а хендлеры останутся прежними.

В больших проектах между хендлером и репозиторием добавляют ещё один слой — сервис. В нём живут бизнес-правила, которые затрагивают несколько репозиториев, например проверка, что книга есть в наличии, перед добавлением её в заказ. Для учебного проекта такой слой избыточен, поэтому хендлеры будут обращаться к репозиторию напрямую.

Разложим код по модулям:

```text
src/
├── main.rs         — запуск сервера и сборка приложения
├── error.rs        — тип ошибки AppError
├── models.rs       — типы данных приложения
├── repository.rs   — запросы к базе данных
└── handlers.rs     — HTTP-хендлеры
```

Таблицу `books` мы уже создали, поэтому менять базу данных не придётся.

## Модели

В модуле `models.rs` описываются типы данных, с которыми работает приложение:

```rust
// src/models.rs
use serde::{Deserialize, Serialize};

use crate::error::AppError;

#[derive(Debug, Serialize)]
pub struct Book {
    pub id: i32,
    pub isbn: String,
    pub title: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub author: Option<String>,
    pub pages: i32,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub struct NewBook {
    pub isbn: String,
    pub title: String,
    pub author: Option<String>,
    pub pages: i32,
}

impl NewBook {
    pub fn validate(&self) -> Result<(), AppError> {
        if self.title.trim().is_empty() {
            let reason = "название не может быть пустым";
            return Err(AppError::Validation(reason.to_string()));
        }
        if self.pages <= 0 {
            let reason = "число страниц должно быть больше нуля";
            return Err(AppError::Validation(reason.to_string()));
        }
        Ok(())
    }
}
```

Тип `Book` описывает книгу, которая уже есть в базе: репозиторий возвращает книги этого типа, а хендлеры отправляют их клиенту в ответе. Тип `NewBook` описывает данные для создания новой книги, которые приходят в теле запроса. В нём нет идентификатора: его присвоит база данных.

Метод `validate` проверяет данные до того, как они попадут в базу. Число страниц проверяет и база данных через ограничение `CHECK`. Это не лишнее дублирование: ограничение в базе — последняя линия защиты, которая не даст записать некорректные данные, даже если в коде есть ошибка. А проверка в коде позволяет ответить клиенту понятным сообщением, а не внутренней ошибкой сервера.

Сейчас один и тот же тип `Book` служит и для чтения из базы, и для ответа клиенту. Если позже в таблицу добавится служебная колонка, которую нельзя показывать клиенту, для ответа заводят отдельный тип или пропускают поле при сериализации.

## Ошибки

Возьмём тип `AppError` и добавим в него вариант `Conflict` для случаев, когда запрос противоречит уже существующим данным:

```rust
// src/error.rs
#[derive(Debug, thiserror::Error)]
pub enum AppError {
    #[error("книга не найдена")]
    NotFound,
    #[error("{0}")]
    Conflict(String),
    #[error("некорректные данные: {0}")]
    Validation(String),
    #[error(transparent)]
    Internal(#[from] anyhow::Error),
}

impl From<sqlx::Error> for AppError {
    fn from(err: sqlx::Error) -> Self {
        AppError::Internal(err.into())
    }
}
```

Реализация `From<sqlx::Error>` позволяет применять оператор `?` к результату запроса: любая ошибка базы данных станет внутренней ошибкой сервера. Поэтому для `AppError` нужно добавить только статус для нового варианта: `AppError::Conflict(_) => StatusCode::CONFLICT`.

## Репозиторий

Репозиторий — единственное место, где приложение обращается к базе данных. Он хранит пул соединений и предоставляет методы для работы с таблицей:

```rust
// src/repository.rs
use sqlx::PgPool;

use crate::error::AppError;
use crate::models::{Book, NewBook};

#[derive(Clone)]
pub struct BookRepository {
    pool: PgPool,
}

impl BookRepository {
    pub fn new(pool: PgPool) -> Self {
        Self { pool }
    }

    pub async fn list(&self) -> Result<Vec<Book>, AppError> {
        let books = sqlx::query_as!(
            Book,
            "SELECT id, isbn, title, author, pages
             FROM books ORDER BY id"
        )
        .fetch_all(&self.pool)
        .await?;
        Ok(books)
    }

    pub async fn find(
        &self,
        id: i32,
    ) -> Result<Option<Book>, AppError> {
        let book = sqlx::query_as!(
            Book,
            "SELECT id, isbn, title, author, pages
             FROM books WHERE id = $1",
            id
        )
        .fetch_optional(&self.pool)
        .await?;
        Ok(book)
    }

    pub async fn create(
        &self,
        new: &NewBook,
    ) -> Result<Book, AppError> {
        sqlx::query_as!(
            Book,
            "INSERT INTO books (isbn, title, author, pages)
             VALUES ($1, $2, $3, $4)
             RETURNING id, isbn, title, author, pages",
            new.isbn,
            new.title,
            new.author,
            new.pages
        )
        .fetch_one(&self.pool)
        .await
        .map_err(|err| match err.as_database_error() {
            Some(db) if db.is_unique_violation() => {
                AppError::Conflict(format!(
                    "книга с ISBN {} уже есть в каталоге",
                    new.isbn
                ))
            }
            _ => AppError::from(err),
        })
    }

    pub async fn delete(&self, id: i32) -> Result<bool, AppError> {
        let result =
            sqlx::query!("DELETE FROM books WHERE id = $1", id)
                .execute(&self.pool)
                .await?;
        Ok(result.rows_affected() > 0)
    }
}
```

Метод `find` возвращает `Option`, а не ошибку, если книги нет: для репозитория отсутствие строки — обычный результат запроса. Метод `delete` сообщает, была ли удалена хотя бы одна строка. А решать, считать ли это ошибкой, будет хендлер.

В методе `create` нарушение уникальности ISBN превращается в `AppError::Conflict` с понятным сообщением. Только репозиторий знает, что за этой ошибкой стоит ограничение `UNIQUE` в базе данных, поэтому преобразование выполняется именно здесь. Остальные ошибки базы становятся внутренними.

Обратите внимание на конструкцию `RETURNING`: она возвращает созданную строку целиком, поэтому не нужен второй запрос, чтобы получить книгу вместе с присвоенным идентификатором.

## Хендлеры

Хендлеры отвечают за HTTP: разбирают запрос, вызывают репозиторий и формируют ответ. Репозиторий они получают из состояния приложения:

```rust
// src/handlers.rs
use axum::{
    extract::{Path, State},
    http::StatusCode,
    Json,
};

use crate::error::AppError;
use crate::models::{Book, NewBook};
use crate::repository::BookRepository;

pub async fn list_books(
    State(repo): State<BookRepository>,
) -> Result<Json<Vec<Book>>, AppError> {
    Ok(Json(repo.list().await?))
}

pub async fn get_book(
    State(repo): State<BookRepository>,
    Path(id): Path<i32>,
) -> Result<Json<Book>, AppError> {
    let book = repo.find(id).await?.ok_or(AppError::NotFound)?;
    Ok(Json(book))
}

pub async fn create_book(
    State(repo): State<BookRepository>,
    Json(new): Json<NewBook>,
) -> Result<(StatusCode, Json<Book>), AppError> {
    new.validate()?;
    let book = repo.create(&new).await?;
    Ok((StatusCode::CREATED, Json(book)))
}

pub async fn delete_book(
    State(repo): State<BookRepository>,
    Path(id): Path<i32>,
) -> Result<StatusCode, AppError> {
    if repo.delete(id).await? {
        Ok(StatusCode::NO_CONTENT)
    } else {
        Err(AppError::NotFound)
    }
}
```

Хендлер `get_book` превращает отсутствие книги в ошибку `AppError::NotFound` методом `ok_or`, а `delete_book` делает то же самое, если ни одна строка не была удалена. Каждый хендлер описывает только успешный путь, а ошибки уходят наружу через оператор `?`.

## Сборка приложения

В `main` все части собираются вместе: пул передаётся в репозиторий, а репозиторий становится состоянием приложения:

```rust
// src/main.rs
mod error;
mod handlers;
mod models;
mod repository;

use std::time::Duration;

use anyhow::Context;
use axum::{routing::get, Router};
use sqlx::postgres::PgPoolOptions;
use tokio::net::TcpListener;

use crate::repository::BookRepository;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    dotenvy::dotenv().ok();
    let database_url = std::env::var("DATABASE_URL")
        .context("не задана переменная DATABASE_URL")?;

    let pool = PgPoolOptions::new()
        .max_connections(5)
        .acquire_timeout(Duration::from_secs(3))
        .connect(&database_url)
        .await
        .context("не удалось подключиться к базе данных")?;

    sqlx::migrate!()
        .run(&pool)
        .await
        .context("не удалось применить миграции")?;

    let repo = BookRepository::new(pool);

    let router = Router::new()
        .route(
            "/books",
            get(handlers::list_books).post(handlers::create_book),
        )
        .route(
            "/books/{id}",
            get(handlers::get_book).delete(handlers::delete_book),
        )
        .with_state(repo);

    let listener = TcpListener::bind("127.0.0.1:3000").await?;
    axum::serve(listener, router).await?;
    Ok(())
}
```

Состояние должно реализовывать трейт `Clone`. Репозиторий клонировать дёшево: внутри у него только `PgPool`, который сам устроен как `Arc`. Поэтому все запросы работают с одним и тем же пулом соединений.

## Проверка

Запустим сервер и проверим API. Сначала добавим книгу:

```bash
curl -i -H 'content-type: application/json' \
    -d '{"isbn":"978-5-17-118366-2",
         "title":"Мастер и Маргарита",
         "author":"Михаил Булгаков","pages":480}' \
    http://127.0.0.1:3000/books
```

```text
HTTP/1.1 201 Created
{"id":1,"isbn":"978-5-17-118366-2","title":"Мастер и Маргарита",
"author":"Михаил Булгаков","pages":480}
```

Запросим книгу, которой нет в каталоге:

```bash
curl -i http://127.0.0.1:3000/books/42
```

```text
HTTP/1.1 404 Not Found
{"error":"книга не найдена"}
```

Попробуем добавить книгу с тем же ISBN ещё раз:

```bash
curl -i -H 'content-type: application/json' \
    -d '{"isbn":"978-5-17-118366-2",
         "title":"Мастер и Маргарита","pages":480}' \
    http://127.0.0.1:3000/books
```

```text
HTTP/1.1 409 Conflict
{"error":"книга с ISBN 978-5-17-118366-2 уже есть в каталоге"}
```

И, наконец, книгу без страниц:

```bash
curl -i -H 'content-type: application/json' \
    -d '{"isbn":"978-0","title":"Пустая","pages":0}' \
    http://127.0.0.1:3000/books
```

```text
HTTP/1.1 400 Bad Request
{"error":"некорректные данные: число страниц должно быть больше нуля"}
```

Каждая ошибка возникает в своём месте: `404` — в хендлере, когда репозиторий не нашёл книгу, `409` — в репозитории, когда база сообщила о нарушении уникальности, а `400` — при проверке данных в `NewBook::validate`.

Если остановить базу данных, клиент через три секунды получит ответ `500 Internal Server Error`, а подробности попадут в лог сервера:

```text
внутренняя ошибка: pool timed out while waiting for an open connection
```

Теперь данные сохраняются между перезапусками сервера, а работа с базой данных собрана в одном месте. В следующем модуле вернёмся к Axum и разберём его возможности, которые понадобятся в реальном приложении.
