%.run: %.o
	clang -g -arch x86_64 -o $@ main.c $<

%.o: %.s
	nasm -f macho64 -o $@ $<

%.s: %.int
	dune exec hugorm $< > $@

%PHONY: clean
clean:
	rm -f *.o *.s *.run 
