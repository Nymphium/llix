LLIX_DIR = llix

.PHONY: install compile

install: compile
	luarocks --local make llix-build-1.rockspec

compile:
	moonc $(LLIX_DIR)

