
build: var/mulang/mulang
	./var/mulang/mulang
	cp var/out.sh scala-parser

var/mulang/mulang:
	mkdir -p var/mulang
	cd var/mulang; git clone 'https://github.com/xsvutils/xsv-mulang.git' ./
	cd var/mulang; make

var/class_file/Main.class: src/*.scala
	scalac -deprecation -d var/class_file $<
