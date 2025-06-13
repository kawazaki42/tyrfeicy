PC = fpc

all: main

run: main
	LD_LIBRARY_PATH=".:$$LD_LIBRARY_PATH" ./$<

%: %.c libmy.so
	$(CC) -L. -lmy $< -o $@

libmy.so: my.pas

lib%.so: %.pas
	$(PC) $<
