
function _xsvutils() {
    (
        echo $CURRENT
        for i in $(seq 2 $CURRENT); do
            echo "${words[i]}"
        done
    ) > var/debug.txt
    cat <<EOF > var/comp.txt
_values 'options' 'one[111]' 'two'
_files .
EOF
    . ./var/comp.txt
}

compdef _xsvutils run.sh

