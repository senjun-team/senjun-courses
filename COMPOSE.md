# Локальный запуск проекта SenJun

Чтобы локально поднять сайт и необходимые для его работы микросервисы, в у вас должны быть установлены:
- `docker-compose`
- `make`

## Перед запуском

Склонируйте к себе два проекта: 
- [senjun-courses](https://github.com/senjun-team/senjun-courses) - тексты курсов и обвязка для запуска инфраструктуры.
- [senjun-images](https://github.com/senjun-team/senjun-images) - docker-образы для запуска задач.

Оба проекта должны находиться _на одном уровне_ в вашей иерархии директорий:

```bash
git clone https://github.com/senjun-team/senjun-courses.git
git clone https://github.com/senjun-team/senjun-images.git
```

Подгрузите самбодули проекта `senjun-courses`:

```bash
cd senjun-courses
git submodule update --init --recursive
```

## Запуск

Вся инфраструктура запускается одной командой из корня проекта `senjun-courses`:

```bash
make start_services -j2
```

Число после опции `-j` задает количество потоков, в которые собираются docker-образы. При первом запуске команда должна собрать все образы, накатить миграции и подготовить к старту микросервисы. Поэтому _первый_ запуск может быть долгим. Повторные - быстрее.

Если команда завершилась успехом, вы можете открыть в браузере локальный адрес и начать работать с сайтом:

```
http://127.0.0.1:8001/
```

На сайте уже создан пользователь. Логин `admin`, пароль `admin`.

Для остановки инфраструктуры выполните `make clean`.

# Доступы

pgadmin:
- login: admin@senjun.ru
- password: admin

scene:
- login: admin
- password: admin

