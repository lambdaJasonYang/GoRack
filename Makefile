GO ?= go
RACO ?= raco
EXPECTED_GO_VERSION := go1.26.5
GO_BRIDGE_DIR := go-bridge
BRIDGE := $(GO_BRIDGE_DIR)/bin/gorack-go-bridge
RACKET_MODULES := $(shell find gorack -type f -name '*.rkt' -print | sort)

.PHONY: all check-go bridge go-test racket-test test clean

all: test

check-go:
	@test "$$($(GO) env GOVERSION)" = "$(EXPECTED_GO_VERSION)" || { \
		echo "This distribution requires $(EXPECTED_GO_VERSION); found $$($(GO) env GOVERSION)" >&2; \
		exit 1; \
	}

bridge: check-go
	mkdir -p $(dir $(BRIDGE))
	GOTOOLCHAIN=local $(GO) -C $(GO_BRIDGE_DIR) build -o bin/gorack-go-bridge ./cmd/gorack-go-bridge

go-test: check-go
	GOTOOLCHAIN=local $(GO) -C $(GO_BRIDGE_DIR) test ./...

racket-test:
	PLTCOLLECTS="$(CURDIR):" $(RACO) make $(RACKET_MODULES)
	PLTCOLLECTS="$(CURDIR):" $(RACO) test $(RACKET_MODULES)

test: go-test racket-test

clean:
	rm -rf $(GO_BRIDGE_DIR)/bin
	find gorack -type d -name compiled -prune -exec rm -rf {} +
