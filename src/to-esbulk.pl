use strict;
use warnings;
use utf8;

use Encode qw/decode_utf8/;
use JSON qw/encode_json/;

# sudo apt install libjson-perl

# curl -XPOST -H 'Content-Type: application/json' @- "https://AAA.ap-northeast-1.es.amazonaws.com/BBB/_bulk"

my $headers = undef;
my $headerCount = 0;

{
    my $line = <STDIN>;
    exit(1) unless defined($line);

    $line =~ s/\r?\n\z//g;
    $line = decode_utf8($line);
    my @cols = split(/\t/, $line, -1);

    $headers = \@cols;
    $headerCount = @cols;
    for (my $i = 0; $i < $headerCount; $i++) {
        $cols[$i] = encode_json($cols[$i]);
    }
}

my $action_json = "{ \"index\" : { } }\n";

while (my $line = <STDIN>) {
    $line =~ s/\r?\n\z//g;
    $line = decode_utf8($line);
    my @cols = split(/\t/, $line, -1);

    # 行にタブの数が少ない場合に列を付け足す
    for (my $i = $headerCount - @cols; $i > 0; $i--) {
        push(@cols, "");
    }
    my $buf = "{ ";
    for (my $i = 0; $i < $headerCount; $i++) {
        if ($i > 0) {
            $buf .= ", ";
        }
        my $h = $headers->[$i];
        my $v = encode_json($cols[$i]);
        $buf .= "$h: $v";
    }
    $buf .= " }\n";
    print $action_json;
    print $buf;
}

