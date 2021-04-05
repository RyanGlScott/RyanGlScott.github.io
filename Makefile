all: docker-serve

BUNDLE_ARGS=exec jekyll serve --no-watch --config _config.yml,_config-dev.yml
IMAGE_NAME=ryanglscott/website

.PHONY: serve
serve:
	bundle ${BUNDLE_ARGS}

docker-build:
	docker build -t ${IMAGE_NAME} .

.PHONY: docker-serve
docker-serve: docker-build
	docker run --rm --net=host ${IMAGE_NAME} ${BUNDLE_ARGS}
