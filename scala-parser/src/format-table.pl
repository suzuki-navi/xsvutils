use strict;
use warnings;
use utf8;

my $flag_path = undef;
my $flag_append = '';

while (@ARGV) {
    my $a = shift(@ARGV);
    if ($a eq "-f") {
        die "option $a needs an argument" unless (@ARGV);
        $flag_path = shift(@ARGV);
    } elsif ($a eq "--flag-append") {
        $flag_append = 1;
    } else {
        die "Unknown argument: $a";
    }
}

################################################################################

sub write_flag {
    my @flags = @_;
    if (defined($flag_path)) {
        my $flag_fh;
        if ($flag_append) {
            open($flag_fh, '>>', $flag_path) or die $!;
        } else {
            open($flag_fh, '>', $flag_path) or die $!;
        }
        foreach my $r (@flags) {
            print $flag_fh "$r\n";
        }
        close($flag_fh);
        $flag_append = 1;
    }
}

################################################################################

my $head_buf = "";

while () {
    my $head_buf2;
    my $l = sysread(STDIN, $head_buf2, 4096);
    if ($l == 0) {
        last;
    }
    $head_buf .= $head_buf2;
}

################################################################################

my $newline;
if ($head_buf =~ /\r\n/) {
    $newline = 'dos';
} elsif ($head_buf =~ /\r/) {
    $newline = 'mac';
} else {
    $newline = 'unix';
}
write_flag("newline:$newline");

my $head_buf2 = $head_buf;
if ($newline eq 'mac') {
    $head_buf2 =~ s/\r/\n/g;
}

my $table_format;
if ($head_buf2 =~ /\A[^\n]*\t/) {
    $table_format = 'tsv';
} elsif ($head_buf2 =~ /\A[^\n]*,/) {
    $table_format = 'csv';
} else {
    $table_format = 'tsv';
}
write_flag("$table_format");

# 文字コードを自動判別する。
# いまのところ、 UTF-8 / SHIFT-JIS のみ
my $charencoding;
my $utf8bom;
{
    my $len = length($head_buf2);
    my $utf8_multi = 0;
    my $utf8_flag = 1;
    my $sjis_multi = 0;
    my $sjis_flag = 1;
    for (my $i = 0; $i < $len; $i++) {
        my $b = ord(substr($head_buf2, $i, 1));
        if ($utf8_multi > 0) {
            if    ($b >= 0x80 && $b < 0xC0)  { $utf8_multi--; }
            else                             { $utf8_multi = 0; $utf8_flag = ''; }
        } else {
            if    ($b < 0x80)                { ; }
            elsif ($b >= 0xC2 && $b < 0xE0)  { $utf8_multi = 1; }
            elsif ($b >= 0xE0 && $b < 0xF0)  { $utf8_multi = 2; }
            elsif ($b >= 0xF0 && $b < 0xF8)  { $utf8_multi = 3; }
            else                             { $utf8_flag = ''; }
        }
        if ($sjis_multi > 0) {
            if    ($b >= 0x40 && $b <= 0x7E) { $sjis_multi = 0; }
            elsif ($b >= 0x80 && $b <= 0xFC) { $sjis_multi = 0; }
            else                             { $sjis_multi = 0; $sjis_flag = ''; }
        } else {
            if    ($b <= 0x7F)               { ; }
            elsif ($b >= 0x81 && $b <= 0x9F) { $sjis_multi = 1; }
            elsif ($b >= 0xA0 && $b <= 0xDF) { ; }
            elsif ($b >= 0xE0 && $b <= 0xFC) { $sjis_multi = 1; }
            elsif ($b >= 0xFD && $b <= 0xFF) { ; }
            else                             { $sjis_flag = ''; }
        }
    }
    $charencoding = "UTF-8";
    $utf8bom = '';
    if (!$utf8_flag && $sjis_flag) {
        $charencoding = "SHIFT-JIS";
    } else {
        if ($len >= 3) {
            if (substr($head_buf2, 0, 3) eq "\xEF\xBB\xBF") {
                # BOM in UTF-8
                # require `tail -c+4`
                $utf8bom = '1';
            }
        }
    }
}
write_flag("$charencoding");
if ($utf8bom) {
    write_flag("utf8bom");
}

################################################################################

