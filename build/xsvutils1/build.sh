
set -Ceu

if [ ! -e var/xsvutils1 ]; then
    git clone https://github.com/xsvutils/xsvutils.git var/xsvutils1
    (
        cd var/xsvutils1
        git checkout refs/tags/0.13
    )
fi

cd var/xsvutils1
make

