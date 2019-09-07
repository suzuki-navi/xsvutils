use strict;
use warnings;
use utf8;

################################################################################

# ファイルまたはパイプからの入力をそのまま標準出力するが、
# ファイルの先頭部分だけteeコマンドのように別の出力先にも出力する。
# 入力が圧縮されている場合は、どちらの出力先にも解凍後のデータを出力する。
# 
# 別の出力先は -h で指定する。
# -h を指定すると、指定したファイルパスに入力ファイルの先頭部分を出力する。
# 入力が圧縮されていた場合は解凍後のデータを出力する。
# 
# -f を指定すると、指定したファイルパスに入力ファイルのフォーマットに関する情報を出力する。
# -f での出力例
# gz    gzipでの圧縮(-h での出力は解凍後のファイルになる)
# xz    xzでの圧縮(-h での出力は解凍後のファイルになる)
# file  fileモード(それ以外はpipeモード)
# 
# fileモードでは標準出力しない。
# 
# 例
# 
# $ perl compression-wrapper.pl -f flags.txt             -c 1024  < input.txt > output.txt
# $ perl compression-wrapper.pl -f flags.txt             -c 1024 -i input.txt > output.txt
# $ perl compression-wrapper.pl -f flags.txt -h head.txt -c 1024 -i input.txt > output.txt
# 
# 外部コマンド gunzip, xz に依存する。

################################################################################

my $self_script = $0;

# デフォルトはfileモードまたはpipeモードいずれか
# --pipe をつけると、pipeモードに強制する
my $pipe_mode = '';

my $input_path = undef;
my $flag_path = undef;
my $flag_append = '';
my $head_path = undef;
my $head_size = 4096;

while (@ARGV) {
    my $a = shift(@ARGV);
    if ($a eq "--pipe") {
        # pipeモードに強制する
        $pipe_mode = 1;
    } elsif ($a eq "-i") {
        # 入力ファイルを指定
        die "option $a needs an argument" unless (@ARGV);
        $input_path = shift(@ARGV);
    } elsif ($a eq "-h") {
        # 先頭部分の出力先
        die "option $a needs an argument" unless (@ARGV);
        $head_path = shift(@ARGV);
    } elsif ($a eq "-c") {
        # 先頭部分の出力サイズ
        die "option $a needs an argument" unless (@ARGV);
        $head_size = shift(@ARGV);
    } elsif ($a eq "-f") {
        # フォーマットに関する情報の出力先
        die "option $a needs an argument" unless (@ARGV);
        $flag_path = shift(@ARGV);
    } elsif ($a eq "--flag-append") {
        # 内部から呼び出されるとき専用のオプション
        $flag_append = 1;
    } else {
        die "Unknown argument: $a";
    }
}

if (defined($input_path)) {
    if (! -f $input_path) {
        # 入力がパイプの場合はpipeモードを強制する
        # シンボリックリンクの場合はリンク先で判断
        $pipe_mode = 1;
    }
} else {
    # 入力が標準入力の場合はpipeモードを強制する
    $pipe_mode = 1;
}

if (defined($input_path)) {
    open(my $in, '<', $input_path) or die $!;
    open(STDIN, '<&=', fileno($in)) or die $!;
}

################################################################################

sub detectFormat {
    my ($head_buf) = @_;
    my @result = ();

    my $newline;
    if ($head_buf =~ /\r\n/) {
        $newline = 'dos';
    } elsif ($head_buf =~ /\r/) {
        $newline = 'mac';
    } else {
        $newline = 'unix';
    }
    push(@result, $newline);

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
    push(@result, $table_format);

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
        $charencoding = "utf8";
        $utf8bom = '';
        if (!$utf8_flag && $sjis_flag) {
            $charencoding = "sjis";
        } else {
            if ($len >= 3) {
                if (substr($head_buf2, 0, 3) eq "\xEF\xBB\xBF") {
                    # BOM in UTF-8
                    # require `tail -c+4`
                    $charencoding = 'utf8-bom';
                }
            }
        }
    }
    push(@result, $charencoding);
    \@result;
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

# 読み込み済みの入力を標準出力し、残りはcatする
sub cat {
    my ($head_buf) = @_;
    # 先読みした内容を出力
    syswrite(STDOUT, $head_buf);
    # 残りの入力をそのまま出力
    exec("cat");
}

# 読み込み済みの入力と残りの入力を結合して
# proc2の標準入力に出力し、
# proc2の標準出力を
# proc3の標準入力に接続し、
# proc3の標準出力を現在のプロセスの出力とする。
sub fork_processes {
    my ($head_buf, $proc2, $proc3) = @_;

    my $PROCESS2_READER;
    my $PROCESS1_WRITER;
    pipe($PROCESS2_READER, $PROCESS1_WRITER);

    my $pid1 = fork;
    if (!defined($pid1)) {
        die $!;
    } elsif ($pid1 == 0) {
        # process 1
        close($PROCESS2_READER);
        open(STDOUT, '>&=', fileno($PROCESS1_WRITER)) or die $!;

        # 読み込み済みの入力を標準出力し、残りはcatする
        cat($head_buf);
        exit(0);
    }
    close($PROCESS1_WRITER);

    # process 2 or 3

    my $PROCESS3_READER;
    my $PROCESS2_WRITER;
    pipe($PROCESS3_READER, $PROCESS2_WRITER);

    my $pid2 = fork;
    if (!defined($pid2)) {
        die $!;
    } elsif ($pid2 == 0) {
        # process 2

        close($PROCESS3_READER);
        open(STDIN, '<&=', fileno($PROCESS2_READER)) or die $!;
        open(STDOUT, '>&=', fileno($PROCESS2_WRITER)) or die $!;
        # $proc2 のエラーは無視する
        close(STDERR);

        $proc2->();
        exit(0);
    }
    close($PROCESS2_WRITER);

    my $pid3 = fork;
    if (!defined($pid3)) {
        die $!;
    } elsif ($pid3 == 0) {
        # process 3
        open(STDIN, '<&=', fileno($PROCESS3_READER)) or die $!;

        $proc3->();
        exit(0);
    }

    my $result_code = 0;
    waitpid($pid3, 0);
    if (${^CHILD_ERROR_NATIVE} != 0) {
        $result_code = 1;
    }
    # $proc2 のエラーは無視する

    exit($result_code);
}

################################################################################
# 先頭部分を読み込む
################################################################################

my $head_buf = "";
my $read_size = 0;
my $gzip_flag = '';
my $xz_flag = '';

while () {
    if ($read_size >= $head_size) {
        last;
    }
    my $head_buf2;
    my $l = sysread(STDIN, $head_buf2, $head_size - $read_size);
    if ($l == 0) {
        last;
    }
    $head_buf .= $head_buf2;
    $read_size += $l;
    if ($read_size >= 2) {
        if ($head_buf =~ /\A\x1F\x8B/) {
            $gzip_flag = 1;
            last;
        }
    }
    if ($read_size >= 6) {
        if ($head_buf =~ /\A\xFD\x37\x7A\x58\x5A\x00/) {
            $xz_flag = 1;
            last;
        }
    }
}

################################################################################
# 残りを処理
################################################################################

if ($gzip_flag || $xz_flag) {
    # 圧縮ファイルの場合の処理

    if ($xz_flag) {
        write_flag("xz");
    } else {
        write_flag("gz");
    }

    my $proc2 = sub {
        # gunzip or xz のプロセスをexecする
        if ($xz_flag) {
            exec("xz", "-c", "-d");
        } else {
            exec("gunzip", "-c");
        }
    };
    my $proc3 = sub {
        my @options = ();
        if (defined($head_path)) {
            push(@options, "-h", $head_path, "-c", $head_size);
        }
        if (defined($flag_path)) {
            push(@options, "-f", $flag_path, "--flag-append");
        }
        exec("perl", $self_script, @options);
    };
    fork_processes($head_buf, $proc2, $proc3);
    exit(0);
}

# 非圧縮ファイルの場合の処理

if ($pipe_mode) {
    write_flag("pipe");
} else {
    write_flag("file");
}

if (defined($head_path)) {
    open(my $head_fh, '>', $head_path) or die $!;
    syswrite($head_fh, $head_buf);
    close($head_path);
}

my $formatFlags = detectFormat($head_buf);
for my $f (@$formatFlags) {
    write_flag($f);
}

# 読み込み済みの入力を標準出力し、残りはcatする
if ($pipe_mode) {
    cat($head_buf);
}

################################################################################

