all: .tmp/.init_db.done start_services

.tmp:
	mkdir -p .tmp

start_services: build
	docker compose up -d --remove-orphans

.tmp/.init_db.done: .tmp
	docker compose up -d postgres
	docker compose --profile init up init_handyman_db
	touch $@

clean:
	docker compose kill 
	docker compose rm -f
	docker compose volumes --format '{{.Name}}' | xargs -r docker volume rm
	rm -rf .tmp


retry: clean start_services init_db

build:
	docker compose build 

