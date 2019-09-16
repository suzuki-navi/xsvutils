#!/bin/bash

action=$1

if [ "$action" = "all" ]; then
    ls src/*-help.txt | sed -E 's#^src/(.+)-help\.txt$#cmd-\1.txt#g'
    ls src/help-*.txt | sed -E 's#^src/help-(.+)\.txt$#guide-\1.txt#g' | grep -v -E '(guide-main|guide-notfound)'
    echo main.txt
    echo notfound.txt
fi

if [ "$action" = "cmd" ]; then
    ls src/*-help.txt | sed -E 's#^src/(.+)-help\.txt$#help/cmd-\1.txt#g'
fi

if [ "$action" = "guide" ]; then
    ls src/help-*.txt | sed -E 's#^src/help-(.+)$#help/guide-\1#g' | grep -v -E '(guide-main|guide-notfound)'
fi

