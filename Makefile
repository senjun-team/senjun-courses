.PHONY: infra/handyman infra/scene infra/watchman_cpp
# Определение DOCKER_GID необходимо для доступа watchman_cpp к Docker socket
export DOCKER_GID := $(shell getent group docker | cut -d: -f3)


all: start_services

IMAGES = infra/handyman infra/scene infra/watchman_cpp


$(IMAGES):
	$(MAKE) -C $@ image

start_services: build init_db $(IMAGES) 
	docker compose up -d --remove-orphans
	docker compose exec -e DJANGO_SUPERUSER_PASSWORD=admin scene ./manage.py createsuperuser --username admin --email admin@senjun.ru --noinput

init_db:
	docker compose --profile init kill --remove-orphans init_postgres init_handyman_db init_scene_db postgres
	docker compose --profile init rm -f init_postgres init_handyman_db init_scene_db postgres
	docker volume rm -f sj_postgres
	docker compose up -d postgres --remove-orphans
	docker compose --profile init up init_postgres init_handyman_db init_scene_db

retry: clean start_services

build:
	 docker compose --profile images build

clean:
	docker compose --profile init,images kill --remove-orphans
	docker compose --profile init,images rm -f
	docker compose volumes --format '{{.Name}}' | xargs -r docker volume rm

# only built by us
clean_images:
	docker compose --profile images config --images | grep -v postgres | xargs docker rmi -f
