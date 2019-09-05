
build: scala-parser

scala-parser: FORCE var/mulang/mulang
	@ ./var/mulang/mulang --devel
	@ if [ ! -e $@ ] || ! cmp -s var/out.sh $@; then cp var/out.sh $@; fi

var/mulang/mulang:
	mkdir -p var/mulang
	cd var/mulang; git clone 'https://github.com/xsvutils/xsv-mulang.git' ./
	cd var/mulang; make

#var/anylang.sh:
#	curl -fsS 'https://raw.githubusercontent.com/xsvutils/xsv-anylang/master/anylang.sh' > var/anylang.sh
#
#var/class_file/Main.class: src/*.scala var/anylang.sh
#	bash ./var/anylang.sh --jdk=11 --scala=2.13.0 scalac -deprecation -d var/class_file $<

FORCE:

