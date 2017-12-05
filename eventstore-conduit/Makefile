.PHONY: help run-eventstore

APP_NAME := eventstore-conduit

help: ## Show this help
	@echo "---\nMakefile for $(APP_NAME)\n---"
	@fgrep -h "##" $(MAKEFILE_LIST) | \
	fgrep -v fgrep | sed -e 's/## */##/' | column -t -s##

run-eventstore: ## Run eventstore in docker
	mkdir -p data/
	docker run --rm -ti \
		-p 1113:1113 \
		-p 2113:2113 \
		-v $(shell pwd)/data:/var/lib/eventstore \
		--name eventstore-conduit \
		eventstore/eventstore:release-4.0.2

