.PHONY: clean all

all:
	./cargo-build.sh
	cp target/debug/jlc jlc

clean:
	rm -f jlc
	rm -rf target .rustup .cargo
