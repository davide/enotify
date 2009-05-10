APP = enotify

EBINDIR = ebin

all: $(EBINDIR) dependencies
	erl -noshell -s make all -s init stop

$(EBINDIR):
	mkdir -p $(EBINDIR)

dependencies:
	cd priv/build/mingw; make

run:
	werl -pa ebin -eval "$(APP):start(\"priv/win32/enotify/mingw/enotify.exe\")" &

clean:
	rm -rf $(EBINDIR)/*.beam erl_crash.dump
	cd priv/build/mingw; make clean
