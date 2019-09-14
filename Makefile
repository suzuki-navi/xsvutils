
# ここを修正する場合は src/install.sh の修正も必要
# それよりは etc/build-makefile.sh の修正のほうが望ましい
build: var/makefile-legacy var/xsvutils-scala
	make --question -f var/makefile-legacy build || make -f var/makefile-legacy build

gobuild: var/makefile
	make -f var/makefile gobuild

var/xsvutils-scala: var/mulang/mulang FORCE
	@ mkdir -p var/build-by-mulang
	@ if [ ! -e var/build-by-mulang/src ]; then ln -s ../../src var/build-by-mulang/src; fi
	@ cd var/build-by-mulang; ../mulang/mulang
	@ if [ ! -e $@ ] || ! cmp -s var/build-by-mulang/var/out.sh $@; then cp var/build-by-mulang/var/out.sh $@; fi

var/mulang/mulang:
	mkdir -p var/mulang
	cd var/mulang; git clone 'https://github.com/xsvutils/xsv-mulang.git' ./
	cd var/mulang; make

test: build
	bash test/test.sh

var/makefile-legacy: FORCE
	bash etc/build-makefile.sh > var/makefile-legacy.tmp
	mv var/makefile-legacy.tmp var/makefile-legacy

FORCE:

.PHONY: rust
rust:
	cd etc && cargo update
	cargo check --manifest-path=etc/Cargo.toml --target-dir=var/rust-target
	cargo fix --allow-dirty --allow-staged --manifest-path=etc/Cargo.toml --target-dir=var/rust-target
	cargo clippy --all --manifest-path=etc/Cargo.toml --target-dir=var/rust-target
	cd etc && cargo fmt
	cargo test --manifest-path=etc/Cargo.toml --target-dir=var/rust-target
	cargo build --manifest-path=etc/Cargo.toml --target-dir=var/rust-target
