OUT_DIR := out

%.run: $(OUT_DIR)/%.run
	$^

$(OUT_DIR)/%.run: %.int | $(OUT_DIR)
	dune exec hugorm $^ > $(OUT_DIR)/$*.s
	nasm -f macho64 -o $(OUT_DIR)/$*.o $(OUT_DIR)/$*.s
	clang -g -arch x86_64 -o $@ main.c $(OUT_DIR)/$*.o

$(OUT_DIR):
	mkdir -p $@

.PHONY: clean
clean:
	rm -f *.o *.s *.run
