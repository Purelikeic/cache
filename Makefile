BUILD_DIR = ./build

PRJ = cache

test:
	./mill -i $(PRJ).test

verilog:
	mkdir -p $(BUILD_DIR)
	./mill -i $(PRJ).runMain Elaborate --target-dir $(BUILD_DIR)

help:
	./mill -i $(PRJ).runMain Elaborate --help

clean:
	rm -rf $(BUILD_DIR)

.PHONY: test verilog help clean
