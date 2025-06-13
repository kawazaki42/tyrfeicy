PC = fpc

all: main

run: main
	LD_LIBRARY_PATH=".:$$LD_LIBRARY_PATH" ./$<

%: %.c libmy.so
	$(CC) -L. -lmy $< -o $@

lib%.so: %.pas
	$(PC) $<
