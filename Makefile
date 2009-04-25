
all: dependencies
	erl -noshell -s make all -s init stop

dependencies:
	cd priv; cd win32; make

run:
	werl -pa ebin -s enotify start &
