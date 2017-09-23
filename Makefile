EMACS?=emacs
VERSION="$(shell sed -nre '/^;; Version:/ { s/^;; Version:[ \t]+//; p }' helm-locations.el)"

default:
	@echo "Version: helm-locations-$(VERSION)"

test: test-all

test-all:
	cask exec buttercup -L . test
