.PHONY: serve

serve:
	bundle exec jekyll serve --no-watch --config _config.yml,_config-dev.yml
