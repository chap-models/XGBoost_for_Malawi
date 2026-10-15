.PHONY: help install run test test-remote docker-build docker-run clean

IMAGE ?= xgboost-for-malawi
PORT ?= 9090
PLATFORM_FLAG :=

help:
	@echo "Targets:"
	@echo "  install       Install deps into .venv (uv sync)"
	@echo "  run           Serve the chapkit app locally (python main.py)"
	@echo "  test          Build the image, start the container, drive a full chapkit"
	@echo "                test against it, then tear down"
	@echo "  test-remote   Drive chapkit test against an already-running service on \$$PORT"
	@echo "  docker-build  Build the container image (\$$IMAGE=$(IMAGE))"
	@echo "  docker-run    Run the container, publishing \$$PORT=$(PORT)"
	@echo "  clean         Remove .venv and common caches"

install:
	uv sync

run:
	uv run python main.py

# Build + run the image, then drive `chapkit test` against the running container.
# Required for R-based models (the host doesn't have R + the renv packages),
# useful for Python ones too because it exercises the same image that ships
# downstream. `chapkit test` fails on its first request if the server is not
# listening yet, so wait for /health before driving it.
test: docker-build
	@docker rm -f $(IMAGE)-test 2>/dev/null || true
	docker run -d --name $(IMAGE)-test -p $(PORT):8000 $(PLATFORM_FLAG) $(IMAGE)
	@for i in $$(seq 1 60); do \
		curl -fsS http://localhost:$(PORT)/health >/dev/null 2>&1 && break; \
		if [ $$i -eq 60 ]; then echo "service did not become healthy in time"; docker logs $(IMAGE)-test; docker rm -f $(IMAGE)-test >/dev/null; exit 1; fi; \
		sleep 2; \
	done
	@uv run chapkit test --url http://localhost:$(PORT); status=$$?; \
		docker stop $(IMAGE)-test >/dev/null; \
		docker rm $(IMAGE)-test >/dev/null; \
		exit $$status

test-remote:
	uv run chapkit test --url http://localhost:$(PORT)

docker-build:
	docker build $(PLATFORM_FLAG) --build-arg GIT_REVISION=$(shell git rev-parse HEAD 2>/dev/null) -t $(IMAGE) .

docker-run:
	docker run --rm -p $(PORT):8000 $(PLATFORM_FLAG) $(IMAGE)

clean:
	rm -rf .venv .ruff_cache .pytest_cache .mypy_cache
	find . -type d -name '__pycache__' -exec rm -rf {} + 2>/dev/null || true