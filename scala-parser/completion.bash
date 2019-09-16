
_xsvutils() {
    (
        echo $COMP_CWORD
        for ((i = 1; i <= $COMP_CWORD; i++)) {
            echo "${COMP_WORDS[i]}"
        }
    ) > var/debug.txt
    local cur=${COMP_WORDS[COMP_CWORD]}
    cat <<\EOF > var/comp.txt
COMPREPLY=( $(compgen -W "one two $(ls 2>/dev/null)" -- $cur) )
EOF
    . ./var/comp.txt
}

complete -F _xsvutils run.sh

