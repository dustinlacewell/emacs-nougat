clean:
	-rm -f ./user-outlines/$(user).e*

init:
	./bin/build-elisp.sh $(user)
