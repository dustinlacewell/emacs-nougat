all: clean init

clean:
	-rm -f ./user-outlines/*.e*

init:
	./bin/build-elisp.sh $(user)
