all: start_services init_db

start_services: build
	docker compose up -d --remove-orphans

init_db:
	docker compose --profile init up init_handyman_db

clean:
	docker compose kill 
	docker compose rm -f
	docker compose volumes --format '{{.Name}}' | xargs -r docker volume rm


retry: clean start_services init_db

build:
	docker compose build 
