LUA_PATH_MAKE = $(shell luarocks path --lr-path | sed -e "s/?.*//")
LUA_BIN_MAKE = $(shell luarocks path --lr-bin | sed -e "s/:.*//")
LLIX = llix

.PHONY: install compile

install: compile
	@echo '--install'
	cp $(LLIX)/*.lua $(LUA_PATH_MAKE)/$(LLIX)/
	cp bin/llix  $(LUA_BIN_MAKE)/

compile:
	@echo '--compile'
	moonc $(LLIX)/

