.PHONY: serve

serve:
	bundle exec jekyll serve -w --config _config.yml,_config-dev.yml
