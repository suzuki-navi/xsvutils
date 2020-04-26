use strict;
use warnings;
use utf8;

# 1行目を読み込んでから別プロセスをexecする点で
# sort.pl, uniq.pl とプログラムの構造がよく似ている

my @args = ();
my $passwd = undef;
my $table = undef;
my $action = undef;

while (@ARGV) {
    my $a = shift(@ARGV);
    if ($a eq "-h") {
        push(@args, $a, shift(@ARGV));
    } elsif ($a eq "-p") {
        push(@args, $a, shift(@ARGV));
    } elsif ($a eq "-U") {
        push(@args, $a, shift(@ARGV));
    } elsif ($a eq "-P") {
        $passwd = shift(@ARGV);
    } elsif ($a eq "-d") {
        push(@args, $a, shift(@ARGV));
    } elsif ($a eq "-t") {
        $table = shift(@ARGV);
    } elsif ($a eq "--append") {
        $action = "append";
    } elsif ($a eq "--overwrite") {
        $action = "overwrite";
    } else {
        die "Unknown argument: $a";
    }
}

die "subcommand `to-postgresql` requires option -t" unless defined $table;
die "subcommand `to-postgresql` requires option --overwrite or --append" unless defined $action;

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

my $query = "";

{
    my $line = $header;
    my @cols = split(/\t/, $line, -1);

    my $colsStr = "\"" . join("\", \"", @cols) . "\"";
    $query = "COPY \"$table\" ($colsStr) FROM STDIN (DELIMITER '\t', FORMAT CSV, HEADER FALSE);";
}

if ($action eq "overwrite") {
    $query = "TRUNCATE \"$table\"; " . $query;
}

push(@args, "-c", $query);
unshift(@args, "psql", "-X");

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

if (defined($passwd)) {
    $ENV{"PGPASSWORD"} = $passwd;
}
exec(@args);

