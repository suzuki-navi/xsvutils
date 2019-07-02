
build: var/mulang
	./var/mulang
	cp var/out.sh scala-parser

var/mulang:
	mkdir -p var
	curl -Ssf 'https://raw.githubusercontent.com/xsvutils/xsv-mulang/master/etc/mulang-last' > var/mulang.tmp
	chmod +x var/mulang.tmp
	mv var/mulang.tmp var/mulang

