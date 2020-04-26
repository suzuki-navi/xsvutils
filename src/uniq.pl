use strict;
use warnings;
use utf8;

# 1行目を読み込んでから別プロセスをexecする点で
# sort.pl, to-postgresql.pl とプログラムの構造がよく似ている

my $in = *STDIN;

my $head_unit_size = 4096;
my $head_buf = "";

while () {
    my $head_buf2;
    my $l = sysread($in, $head_buf2, $head_unit_size);
    if ($l == 0) {
        last;
    }
    $head_buf .= $head_buf2;
    if ($head_buf2 =~ /\n/) {
        last;
    }
}

my $header;
my $body;
if ($head_buf =~ /\A([^\n]*)\n(.*)\z/s) {
    $header = $1;
    $body = $2;
} else {
    $header = $head_buf;
    $body = '';
}

syswrite(STDOUT, $header . "\n");

my $READER1;
my $WRITER1;
pipe($READER1, $WRITER1);
my $pid1 = fork;
die unless defined $pid1;

if (!$pid1) {
    # child process
    close($READER1);
    open(STDOUT, '>&=', fileno($WRITER1));
    syswrite(STDOUT, $body);
    exec("cat");
}

close $WRITER1;

my $READER2;
my $WRITER2;
pipe($READER2, $WRITER2);
my $pid2 = fork;
die unless defined $pid2;

if (!$pid2) {
    # child process
    close($READER2);
    open(STDIN, '<&=', fileno($READER1));
    open(STDOUT, '>&=', fileno($WRITER2));
    exec("sort");
}

close $READER1;
close $WRITER2;

open(STDIN, '<&=', fileno($READER2));
exec("uniq");

