use strict;
use warnings;
use utf8;

# 1行目を読み込んでから別プロセスをexecする点で
# uniq.pl, to-postgresql.pl とプログラムの構造がよく似ている

my @sort_keys = ();

while (@ARGV) {
    my $a = shift(@ARGV);
    if ($a eq "--col") {
        my $b = shift(@ARGV);
        if ($b =~ /\A([nr]*):(.+)\z/) {
            push(@sort_keys, [$2, $1]);
        } else {
            die "Illegal parameter: $b";
        }
    } else {
        die "Unknown argument: $a";
    }
}

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

my @sort_options = ();

{
    my $line = $header;
    my @cols = split(/\t/, $line, -1);

    foreach my $k (@sort_keys) {
        for (my $i = 0; $i < @cols; $i++) {
            if ($k->[0] eq $cols[$i]) {
                my $num = $i + 1;
                my $flag = $k->[1];
                push(@sort_options, "-k");
                push(@sort_options, "$num$flag,$num");
                last;
            }
        }
    }

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
open(STDIN, '<&=', fileno($READER1));
exec("sort", "-t", "\t", "-s", @sort_options);

