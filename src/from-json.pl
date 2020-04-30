use strict;
use warnings;
use utf8;

use Encode qw/encode_utf8 decode_utf8/;
use JSON qw/from_json/;
use Data::Dumper;

my @column_names = ();

while (@ARGV) {
    my $a = shift(@ARGV);
    if ($a eq "--col") {
        push(@column_names, shift(@ARGV));
    } else {
        die "Unknown argument: $a";
    }
}

if (@column_names) {
    print encode_utf8(join("\t", @column_names)) . "\n";
}

my $json = JSON->new->canonical(1);

sub printRecord {
    my ($obj) = @_;
    my @cols = ();
    foreach my $c (@column_names) {
        my $v = $obj->{$c};
        if (!defined($v)) {
            $v = "";
        } elsif ((ref $v) eq "HASH" || (ref $v) eq "ARRAY") {
            $v = $json->encode($v);
        }
        $v =~ s/[\x00-\x1F]/ /g;
        push(@cols, $v);
    }
    print encode_utf8(join("\t", @cols)) . "\n";
}

sub getColumnNames {
    my ($records) = @_;
    my @column_names = ();
    foreach my $r (@$records) {
        for my $c (keys %$r) {
            if (!(grep { $_ eq $c } @column_names)) {
                push(@column_names, $c);
            }
        }
    }
    @column_names = sort @column_names;
    return \@column_names;
}

my @records = ();

my $buf = "";
while (my $line = <STDIN>) {
    $line = decode_utf8($line);
    $buf .= $line;

    my $obj;
    eval {
        $obj = from_json($buf);
    };
    if ($@) {
        next;
    }

    $buf = "";

    if (@column_names) {
        printRecord($obj);
    } else {
        push(@records, $obj);
    }
}

if (!@column_names) {
    @column_names = @{getColumnNames(\@records)};
    print encode_utf8(join("\t", @column_names)) . "\n";
    foreach my $r (@records) {
        printRecord($r);
    }
}

