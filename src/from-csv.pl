use strict;
use warnings;
use utf8;

my $buf = "";
my $output = "";
while (my $line = <STDIN>) {
    $line =~ s/\n\z//g;
    $buf = $buf . $line;

    while () {
        if ($buf =~ /\A([^,\n]*),(.*)\z/s) {
            my $v = $1;
            $buf = $2;
            $v =~ s/\t/ /g;
            $output .= $v . "\t";
        } elsif ($buf =~ /\A([^\n]*)\z/s) {
            my $v = $1;
            $buf = "";
            $v =~ s/\t/ /g;
            $output .= $v . "\n";
            print($output);
            $output = "";
            last;
        } else {
            die;
        }
    }
}

