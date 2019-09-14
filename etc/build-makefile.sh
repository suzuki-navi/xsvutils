#!/bin/bash
uname=$(uname)

if [ "$uname" = "Darwin" ]; then
    platform_name='mac'
elif [ "$uname" = "Linux" ]; then
    platform_name='linux'
else
    echo "Unknown OS: $uname" >&2
    exit 1
fi

TARGET_SOURCES1=$(echo $((
            echo target/xsvutils-go;
            # echo target/xsvutils-ml;
            echo target/xsvutils-rs;
            echo target/xsvutils-scala
            echo target/mcut;
            perl etc/list-sources.pl legacy | grep -v -E -e '(boot\.sh)' | grep -v '\.(java|scala)$' | sed 's/^/target\//g';
            bash etc/list-help.sh all | sed 's/^/target\/help-/g';
            echo target/help-guide-version.txt;
            echo target/help-guide-changelog.txt) | LC_ALL=C sort))
TARGET_SOURCES2=$(echo $((
            echo target/xsvutils-go;
            # echo target/xsvutils-ml;
            echo target/xsvutils-rs;
            echo target/xsvutils-scala
            echo target/mcut;
            perl etc/list-sources.pl legacy | grep -v -E -e '(boot\.sh)' | grep -v '\.(java|scala)$' | sed 's/^/target\//g';
            bash etc/list-help.sh all | sed 's/^/target\/help-/g';
            echo target/help-guide-version.txt;
            echo target/help-guide-changelog.txt) | LC_ALL=C sort))

RM_TARGET=$(diff -u <(ls -d $TARGET_SOURCES1 2>/dev/null) <(ls -d target/* 2>/dev/null) | grep -E '^\+target' | cut -b2-)
if [ -n "$RM_TARGET" ]; then
    echo rm -r $RM_TARGET >&2
    rm -r $RM_TARGET >&2
fi

gopath_rel=var/golang_packages
GOPATH=$PWD/$gopath_rel
JAVA_HOME=$HOME/.xsvutils/var/openjdk

cat <<EOF
export GOPATH=$GOPATH
export JAVA_HOME=$JAVA_HOME
export PATH=$HOME/.xsvutils/var/openjdk/bin:$PWD/var/golang_packages/bin:$PATH

GO       := $PWD/etc/anybuild --prefix=$PWD/var/anybuild --go=1.12.6 go
RUSTUP   := $PWD/etc/anybuild --prefix=$PWD/var/anybuild --rust=1.35.0 rustup
CARGO    := $PWD/etc/anybuild --prefix=$PWD/var/anybuild --rust=1.35.0 cargo
OCAMLOPT := $PWD/etc/anybuild --prefix=$PWD/var/anybuild --ocaml=4.07.0 ocamlopt

EOF

cat <<\EOF
build: xsvutils

target: var/TARGET_VERSION_HASH

EOF

cat <<\EOF
xsvutils: src/boot.sh var/TARGET_VERSION_HASH
	cat src/boot.sh | sed "s/XXXX_VERSION_HASH_XXXX/$$(cat var/TARGET_VERSION_HASH)/g" > var/xsvutils
	(cd target; tar cz *) >> var/xsvutils
	chmod 755 var/xsvutils
	mv var/xsvutils xsvutils

EOF

cat <<EOF
var/TARGET_VERSION_HASH: $TARGET_SOURCES2
	(find $TARGET_SOURCES1 -type f; cat \$\$(find $TARGET_SOURCES1 -type f)) | shasum | cut -b1-40 > var/TARGET_VERSION_HASH.tmp
	mv var/TARGET_VERSION_HASH.tmp var/TARGET_VERSION_HASH

EOF

for f in $(perl etc/list-sources.pl legacy | grep -v -E -e '(boot\.sh)'); do
cat <<EOF
target/$f: src/$f
	perl etc/preprocess.pl $platform_name < src/$f > target/$f.tmp
	mv target/$f.tmp target/$f

EOF
done

cat <<EOF
target/help-guide-version.txt: version.txt
	cp version.txt target/help-guide-version.txt

target/help-guide-changelog.txt: CHANGELOG.md
	cp CHANGELOG.md target/help-guide-changelog.txt

EOF

(
    bash etc/list-help.sh cmd | sed -E 's/^help\/cmd-([^.]+)\.txt$/\1/g'
) | sort | column -c 80 > var/help-cmd-list.txt.tmp
if [ ! -e var/help-cmd-list.txt ] || ! diff -q var/help-cmd-list.txt var/help-cmd-list.txt.tmp >/dev/null; then
    mv var/help-cmd-list.txt.tmp var/help-cmd-list.txt
fi

(
    bash etc/list-help.sh guide | sed -E 's/^help\/guide-([^.]+)\.txt$/\1/g'
    echo version
    echo changelog
) | sort | column -c 80 > var/help-guide-list.txt.tmp
if [ ! -e var/help-guide-list.txt ] || ! diff -q var/help-guide-list.txt var/help-guide-list.txt.tmp >/dev/null; then
    mv var/help-guide-list.txt.tmp var/help-guide-list.txt
fi

cat <<EOF
target/help-main.txt: etc/build-help-main-1.sh etc/build-help-main-2.sh src/help-main.txt var/help-cmd-list.txt var/help-guide-list.txt
	bash etc/build-help-main-1.sh src/help-main.txt > var/help-main.txt.tmp.1
	bash etc/build-help-main-2.sh var/help-main.txt.tmp.1 > var/help-main.txt.tmp.2
	cp var/help-main.txt.tmp.2 target/help-main.txt

target/help-notfound.txt: etc/build-help-main-1.sh etc/build-help-main-2.sh src/help-notfound.txt var/help-cmd-list.txt var/help-guide-list.txt
	bash etc/build-help-main-1.sh src/help-notfound.txt > var/help-notfound.txt.tmp.1
	bash etc/build-help-main-2.sh var/help-notfound.txt.tmp.1 > var/help-notfound.txt.tmp.2
	cp var/help-notfound.txt.tmp.2 target/help-notfound.txt

EOF

help_rm_target=$(diff -u <(bash etc/list-help.sh all 2>/dev/null) <(ls target/help-* 2>/dev/null | sed 's/^target\/help-//g') | tail -n+4 | grep -E '^\+' | cut -b2- | grep -v -E -e 'guide-(changelog|version)\.txt')
if [ -n "$help_rm_target" ]; then
    echo rm $help_rm_target >&2
    rm $help_rm_target >&2
fi

for f in $(ls src/*-help.txt | sed -E 's#^src/(.+)-help\.txt$#\1#g'); do
cat <<EOF
target/help-cmd-$f.txt: src/$f-help.txt
	cp src/$f-help.txt target/help-cmd-$f.txt

EOF
done

for f in $(ls src/help-*.txt | grep -v -E '(help-main|help-notfound)' | sed -E 's#^src/help-(.+)\.txt$#\1#g'); do
cat <<EOF
target/help-guide-$f.txt: src/help-$f.txt
	cp src/help-$f.txt target/help-guide-$f.txt

EOF
done

perl etc/build-makefile-golang.pl $(find src -name '*.go')

go_target=github.com/xsvutils/xsvutils
cat <<EOF
gobuild: target/xsvutils-go

target/xsvutils-go: var/GOLANG_VERSION_HASH $gopath_rel/src/$go_target/go.mod $gopath_rel/src/$go_target/go.sum
	cd $gopath_rel/src/$go_target; GO111MODULE=on \$(GO) vet ./...
	cd $gopath_rel/src/$go_target; GO111MODULE=on \$(GO) build
	cp var/GOLANG_VERSION_HASH var/GOLANG_VERSION_HASH-build
	cp $gopath_rel/src/$go_target/xsvutils target/xsvutils-go

$gopath_rel/src/$go_target/go.mod: etc/go.mod
	cp -p etc/go.mod $gopath_rel/src/$go_target/go.mod

$gopath_rel/src/$go_target/go.sum: etc/go.sum
	cp -p etc/go.sum $gopath_rel/src/$go_target/go.sum

EOF

if [ -e var/sbt ]; then
    echo rm -rf var/sbt >&2
    rm -rf var/sbt >&2
fi
if [ -e target/java ]; then
    echo rm -rf target/java >&2
    rm -rf target/java >&2
fi

if [ "$uname" = "Linux" ]; then

mtools_hash=488b652e5c530c1f100e7f431f37ea78fe23913b

cargo_target=x86_64-unknown-linux-musl
cat <<EOF
target/xsvutils-rs: cargo-build
	cp -p var/rust-target/$cargo_target/release/xsvutils-rs target/xsvutils-rs

.PHONY: cargo-build
cargo-build:
	\$(RUSTUP) target add $cargo_target
	\$(CARGO) build --release --manifest-path=etc/Cargo.toml --target-dir=var/rust-target --target $cargo_target

target/mcut: var/mtools/target/$cargo_target/release/mcut
	cp -p var/mtools/target/$cargo_target/release/mcut target/mcut

var/mtools/target/$cargo_target/release/mcut: var/mtools/hash-$mtools_hash
	cd var/mtools && \$(RUSTUP) target add $cargo_target
	cd var/mtools && \$(CARGO) build --release --target $cargo_target

var/mtools/hash-$mtools_hash: var/mtools/.gitignore
	rm -f var/mtools/hash-*
	touch var/mtools/hash-$mtools_hash

var/mtools/.gitignore:
	git clone https://github.com/ng3rdstmadgke/mtools.git var/mtools
	cd var/mtools && git checkout $mtools_hash

EOF

else

    # TODO このelse節(Linux以外)のコードは以下の2チケットで修正漏れ
    # https://github.com/xsvutils/xsvutils/pull/12
    # https://github.com/xsvutils/xsvutils/pull/15

cat <<EOF
target/xsvutils-rs: cargo-build
	cp -p var/rust-target/release/xsvutils-rs target/xsvutils-rs

.PHONY: cargo-build
cargo-build:
	\$(CARGO) build --release --manifest-path=etc/Cargo.toml --target-dir=var/rust-target

target/mcut: build-mcut
	cp -p ext/mtools/target/release/mcut target/mcut

.PHONY: build-mcut
build-mcut:
	cd ext/mtools && \$(CARGO) build --release

EOF

fi

cat <<'EOF'
ML_SRC = $(wildcard src/*.ml)
ML_TGT = $(patsubst src/%,var/ocaml-target/%,$(ML_SRC))

target/xsvutils-ml: $(ML_TGT)
	$(OCAMLOPT) $(ML_TGT) -o $@

var/ocaml-target/%.ml: src/*.ml
	@mkdir -p var/ocaml-target
	cp $< $@

EOF

cat <<'EOF'
target/xsvutils-scala: var/xsvutils-scala
	cp var/xsvutils-scala target/xsvutils-scala

EOF

