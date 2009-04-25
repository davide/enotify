APP = enotify

EBINDIR = ebin

all: $(EBINDIR) dependencies
	erl -noshell -s make all -s init stop

$(EBINDIR):
	mkdir -p $(EBINDIR)

dependencies:
	cd priv; cd win32; make

run:
	werl -pa ebin -s $(APP) start &

clean:
	rm -rf $(EBINDIR)/*.beam erl_crash.dump