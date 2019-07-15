
function _xsvutils() {
    (
        local -a args
        for i in $(seq 2 $CURRENT); do
            args=($args "${words[i]}")
        done
        ./scala-parser complete zsh "${args[@]}"
    ) > ./var/completion.sh
    . ./var/completion.sh
}

compdef _xsvutils run.sh

