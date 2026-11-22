# Глава 2.4. База данных в Axum и слои приложения

Мы умеем запускать базу данных, создавать таблицы и писать запросы, которые проверяются при компиляции. Осталось соединить это с сервером на Axum. В этой главе сделаем API сервиса фиче-флагов и разберём, как разделить код на слои, чтобы он оставался понятным, когда приложение вырастет.

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

В больших проектах между хендлером и репозиторием добавляют ещё один слой — сервис. В нём живут бизнес-правила, например проверка, включена ли фича для конкретного пользователя: сначала проверить общий выключатель `is_active`, а затем решить, попадает ли пользователь в процент раскатки. Пока наше API только хранит фичи, поэтому такой слой избыточен, поэтому хендлеры будут обращаться к репозиторию напрямую.

Разложим код по модулям:

```text
src/
├── main.rs         — запуск сервера и сборка приложения
├── error.rs        — тип ошибки AppError
├── models.rs       — типы данных приложения
├── repository.rs   — запросы к базе данных
└── handlers.rs     — HTTP-хендлеры
```

Таблицу `features` мы уже создали, поэтому менять базу данных не придётся.

## Модели

В модуле `models.rs` описываются типы данных, с которыми работает приложение:

```rust
// src/models.rs
use serde::{Deserialize, Serialize};

use crate::error::AppError;

#[derive(Debug, Serialize)]
pub struct Feature {
    pub id: i32,
    pub key: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    pub is_active: bool,
    pub rollout_percentage: i32,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub struct NewFeature {
    pub key: String,
    pub description: Option<String>,
    pub rollout_percentage: i32,
}

impl NewFeature {
    pub fn validate(&self) -> Result<(), AppError> {
        let key_ok =
            !self.key.is_empty() && self.key.chars().all(is_key_char);
        if !key_ok {
            let reason = "ключ может содержать только строчные \
                          латинские буквы, цифры и дефис";
            return Err(AppError::Validation(reason.to_string()));
        }
        if !(0..=100).contains(&self.rollout_percentage) {
            let reason = "процент раскатки должен быть от 0 до 100";
            return Err(AppError::Validation(reason.to_string()));
        }
        Ok(())
    }
}

fn is_key_char(c: char) -> bool {
    c.is_ascii_lowercase() || c.is_ascii_digit() || c == '-'
}
```

Тип `Feature` описывает фичу, которая уже есть в базе: репозиторий возвращает фичи этого типа, а хендлеры отправляют их клиенту в ответе. Тип `NewFeature` описывает данные для создания новой фичи, которые приходят в теле запроса. В нём нет идентификатора, который присвоит база данных, и поля `is_active`: новая фича включена по умолчанию.

Метод `validate` проверяет данные до того, как они попадут в базу. Ключ используется в коде приложений, поэтому разрешены только строчные латинские буквы, цифры и дефис: так ключи выглядят одинаково и их легко искать. Функция `is_key_char` проверяет один символ, а метод `all` — что ей соответствуют все символы ключа. Обратная косая черта в конце строки продолжает строковый литерал на следующей строке и убирает пробелы в её начале.

Процент раскатки проверяет и база данных через ограничение `CHECK`. Это не лишнее дублирование: ограничение в базе — последняя линия защиты, которая не даст записать некорректные данные, даже если в коде есть ошибка. А проверка в коде позволяет ответить клиенту понятным сообщением, а не внутренней ошибкой сервера.

Сейчас один и тот же тип `Feature` служит и для чтения из базы, и для ответа клиенту. Если позже в таблицу добавится служебная колонка, которую нельзя показывать клиенту, для ответа заводят отдельный тип или пропускают поле при сериализации.

## Ошибки

Возьмём тип `AppError` и добавим в него вариант `Conflict` для случаев, когда запрос противоречит уже существующим данным:

```rust
// src/error.rs
#[derive(Debug, thiserror::Error)]
pub enum AppError {
    #[error("фича не найдена")]
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
use crate::models::{Feature, NewFeature};

#[derive(Clone)]
pub struct FeatureRepository {
    pool: PgPool,
}

impl FeatureRepository {
    pub fn new(pool: PgPool) -> Self {
        Self { pool }
    }

    pub async fn list(&self) -> Result<Vec<Feature>, AppError> {
        let features = sqlx::query_as!(
            Feature,
            "SELECT id, key, description, is_active,
                    rollout_percentage
             FROM features ORDER BY id"
        )
        .fetch_all(&self.pool)
        .await?;
        Ok(features)
    }

    pub async fn find(
        &self,
        key: &str,
    ) -> Result<Option<Feature>, AppError> {
        let feature = sqlx::query_as!(
            Feature,
            "SELECT id, key, description, is_active,
                    rollout_percentage
             FROM features WHERE key = $1",
            key
        )
        .fetch_optional(&self.pool)
        .await?;
        Ok(feature)
    }

    pub async fn create(
        &self,
        new: &NewFeature,
    ) -> Result<Feature, AppError> {
        sqlx::query_as!(
            Feature,
            "INSERT INTO features
                 (key, description, rollout_percentage)
             VALUES ($1, $2, $3)
             RETURNING id, key, description, is_active,
                       rollout_percentage",
            new.key,
            new.description,
            new.rollout_percentage
        )
        .fetch_one(&self.pool)
        .await
        .map_err(|err| match err.as_database_error() {
            Some(db) if db.is_unique_violation() => {
                AppError::Conflict(format!(
                    "фича с ключом {} уже существует",
                    new.key
                ))
            }
            _ => AppError::from(err),
        })
    }

    pub async fn delete(&self, key: &str) -> Result<bool, AppError> {
        let result =
            sqlx::query!("DELETE FROM features WHERE key = $1", key)
                .execute(&self.pool)
                .await?;
        Ok(result.rows_affected() > 0)
    }
}
```

Метод `find` возвращает `Option`, а не ошибку, если фичи нет: для репозитория отсутствие строки — обычный результат запроса. Метод `delete` сообщает, была ли удалена хотя бы одна строка. А решать, считать ли это ошибкой, будет хендлер.

В методе `create` нарушение уникальности ключа превращается в `AppError::Conflict` с понятным сообщением. Только репозиторий знает, что за этой ошибкой стоит ограничение `UNIQUE` в базе данных, поэтому преобразование выполняется именно здесь. Остальные ошибки базы становятся внутренними.

Обратите внимание на конструкцию `RETURNING`: она возвращает созданную строку целиком, поэтому не нужен второй запрос, чтобы получить фичу вместе с присвоенным идентификатором и значениями по умолчанию.

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
use crate::models::{Feature, NewFeature};
use crate::repository::FeatureRepository;

pub async fn list_features(
    State(repo): State<FeatureRepository>,
) -> Result<Json<Vec<Feature>>, AppError> {
    Ok(Json(repo.list().await?))
}

pub async fn get_feature(
    State(repo): State<FeatureRepository>,
    Path(key): Path<String>,
) -> Result<Json<Feature>, AppError> {
    let feature = repo.find(&key).await?.ok_or(AppError::NotFound)?;
    Ok(Json(feature))
}

pub async fn create_feature(
    State(repo): State<FeatureRepository>,
    Json(new): Json<NewFeature>,
) -> Result<(StatusCode, Json<Feature>), AppError> {
    new.validate()?;
    let feature = repo.create(&new).await?;
    Ok((StatusCode::CREATED, Json(feature)))
}

pub async fn delete_feature(
    State(repo): State<FeatureRepository>,
    Path(key): Path<String>,
) -> Result<StatusCode, AppError> {
    if repo.delete(&key).await? {
        Ok(StatusCode::NO_CONTENT)
    } else {
        Err(AppError::NotFound)
    }
}
```

Фичу хендлеры находят по ключу из пути, например `/features/new-dashboard`: клиентам удобнее обращаться к фиче по понятному ключу, чем по числовому идентификатору. Хендлер `get_feature` превращает отсутствие фичи в ошибку `AppError::NotFound` методом `ok_or`, а `delete_feature` делает то же самое, если ни одна строка не была удалена. Каждый хендлер описывает только успешный путь, а ошибки уходят наружу через оператор `?`.

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

use crate::repository::FeatureRepository;

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

    let repo = FeatureRepository::new(pool);

    let router = Router::new()
        .route(
            "/features",
            get(handlers::list_features)
                .post(handlers::create_feature),
        )
        .route(
            "/features/{key}",
            get(handlers::get_feature)
                .delete(handlers::delete_feature),
        )
        .with_state(repo);

    let listener = TcpListener::bind("127.0.0.1:3000").await?;
    axum::serve(listener, router).await?;
    Ok(())
}
```

Состояние должно реализовывать трейт `Clone`. Репозиторий клонировать дёшево: внутри у него только `PgPool`, который сам устроен как `Arc`. Поэтому все запросы работают с одним и тем же пулом соединений.

## Проверка

Запустим сервер и проверим API. Сначала добавим фичу:

```bash
curl -i -H 'content-type: application/json' \
    -d '{"key":"new-dashboard",
         "description":"Новый интерфейс личного кабинета",
         "rollout_percentage":25}' \
    http://127.0.0.1:3000/features
```

```text
HTTP/1.1 201 Created
{"id":1,"key":"new-dashboard",
"description":"Новый интерфейс личного кабинета",
"is_active":true,"rollout_percentage":25}
```

Запросим фичу, которой нет:

```bash
curl -i http://127.0.0.1:3000/features/dark-theme
```

```text
HTTP/1.1 404 Not Found
{"error":"фича не найдена"}
```

Попробуем добавить фичу с тем же ключом ещё раз:

```bash
curl -i -H 'content-type: application/json' \
    -d '{"key":"new-dashboard","rollout_percentage":100}' \
    http://127.0.0.1:3000/features
```

```text
HTTP/1.1 409 Conflict
{"error":"фича с ключом new-dashboard уже существует"}
```

И, наконец, фичу с некорректным процентом раскатки:

```bash
curl -i -H 'content-type: application/json' \
    -d '{"key":"dark-theme","rollout_percentage":150}' \
    http://127.0.0.1:3000/features
```

```text
HTTP/1.1 400 Bad Request
{"error":"некорректные данные: процент раскатки должен быть
от 0 до 100"}
```

Каждая ошибка возникает в своём месте: `404` — в хендлере, когда репозиторий не нашёл фичу, `409` — в репозитории, когда база сообщила о нарушении уникальности, а `400` — при проверке данных в `NewFeature::validate`.

Если остановить базу данных, клиент через три секунды получит ответ `500 Internal Server Error`, а подробности попадут в лог сервера:

```text
внутренняя ошибка: pool timed out while waiting for an open connection
```

Теперь данные сохраняются между перезапусками сервера, а работа с базой данных собрана в одном месте. В следующем модуле вернёмся к Axum и разберём его возможности, которые понадобятся в реальном приложении.
