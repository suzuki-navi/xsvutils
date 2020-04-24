
cd $XSVUTILS_HOME/etc

if type pipenv >/dev/null 2>&1; then
    #pipenv sync >&2
    exec pipenv run python "$@"
elif type python >/dev/null 2>&1; then
    exec python "$@"
else
    echo "Python not installed." >&2
    echo "See \`xsvutils help install\`" >&2
    exit 1
fi

