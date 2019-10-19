## Makefile

# Copyright (C) 2019  Naoya Yamashita

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

all:

VERSIONS     := 26.1 26.2

EMACS        ?= emacs
LOADPATH     := $$(cask load-path)
BATCH        := EMACSLOADPATH=$(LOADPATH) $(EMACS) -Q --batch

PACKAGE_NAME := flylint
TESTFILE     := $(PACKAGE_NAME)-tests.el
ELS          := $(shell cask files)

GIT_HOOKS    := pre-commit

MAKEARGS     := --no-print-directory

export PACKAGE_NAME
export ELS
export TESTFILE

##################################################

.PHONY: all git-hook init build test down clean
.PRECIOUS: $(VERSIONS:%=.docker/emacs-%/Makefile)

all: git-hook localbuild

git-hook: $(GIT_HOOKS:%=.git/hooks/%)
localbuild: $(ELS:%.el=%.elc)
localtest: localbuild
	$(BATCH) -l $(TESTFILE) -f cort-test-run

##############################

.git/hooks/%: git-hooks/%
	cp -a $< $@

%.elc: %.el .cask
	$(BATCH) -f batch-byte-compile $<

##############################
#
#  dynamic resources
#

# listed below will remove via `make clean`
GABAGES := .cask .env .docker.env .docker

.cask: Cask
	cask install
	find $@ -name "*.elc" | xargs rm -f
	touch $@

.docker/emacs-%/Makefile: Makefile-docker.mk
	mkdir -p $(@D)
	cp $< $@

.env:
	echo "PACKAGE_NAME=$(PACKAGE_NAME)" > $@

.docker.env: .cask
	echo "EMACSLOADPATH=$$(cask load-path | tr ':' '\n' | \
grep -o '\.cask.*' | sed 's|^|/$(PACKAGE_NAME)/|g' | \
tr '\n' ':' | sed 's|^|/$(PACKAGE_NAME)/:|g')" > $@

##############################
#
#  docker managemant
#

.docker/up: up
	touch .docker/up

up: init .cask .env .docker.env
	docker-compose up -d --no-recreate

down:
	docker-compose down
	rm -rf .docker/up

init: $(VERSIONS:%=.make-init-%)
build: $(VERSIONS:%=.make-build-%)
test:  $(VERSIONS:%=.make-test-%)

.make-init-%: $(VERSIONS:%=.docker/emacs-%/Makefile) .cask
	$(MAKE) $(MAKEARGS) init -C .docker/emacs-$* VERSION=$*

.make-build-%: init .docker/up
	$(MAKE) $(MAKEARGS) build -C .docker/emacs-$* VERSION=$* 

.make-test-%: init .docker/up
	$(MAKE) $(MAKEARGS) test -C .docker/emacs-$* VERSION=$*

##############################

clean: down
	rm -rf $(ELS:%.el=%.elc) $(GABAGES)
