# Определение DOCKER_GID необходимо для доступа watchman_cpp к Docker socket
export DOCKER_GID := $(shell getent group docker | cut -d: -f3)

all: .tmp/.init_db.done start_services

start_services: build
	docker compose up -d --remove-orphans

.tmp/.init_db.done: .tmp
	docker compose up -d postgres
	docker compose --profile init up init_handyman_db
	touch $@


retry: clean start_services init_db

build:
	docker compose build 

.tmp:
	mkdir -p .tmp

clean:
	docker compose kill 
	docker compose rm -f
	docker compose volumes --format '{{.Name}}' | xargs -r docker volume rm
	rm -rf .tmp
