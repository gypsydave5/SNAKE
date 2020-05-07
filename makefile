test: test-utils

utils.lua: utils.fnl
	@fennel --compile $< > $@

test-utils: utils.lua
	@fennel utils_test.fnl

watch:
	ls | entr make
