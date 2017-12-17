use strict;
use warnings;
use utf8;

use POSIX qw/mkfifo/;

my $TOOL_DIR = $ENV{"TOOL_DIR"};
my $WORKING_DIR = $ENV{"WORKING_DIR"};
my $isInputTty = undef;
if (-t STDIN) {
    $isInputTty = 1;
}
my $isOutputTty = undef;
if (-t STDOUT) {
    $isOutputTty = 1;
}

################################################################################
# parse command line options
################################################################################

sub escape_for_bash {
    my ($str) = @_;
    if ($str =~ /\A[-_.=\/0-9a-zA-Z]+\z/) {
        return $str;
    }
    $str =~ s/'/'"'"'/g;
    return "'" . $str . "'";
}

my $option_help = undef;
my $option_input = undef;
my $option_output = ""; # 空文字列は標準出力の意味

my $option_explain = undef;

my $option_format = undef;
my $option_input_headers = undef;
my $option_output_headers_flag = 1;
my $option_output_format = undef;

my $subcommands = [];
my $subcommand = undef;
my $subcommand_args = [];

my $addcol_name = undef;
my $addcol_value = undef;

my $exists_args = '';
$exists_args = 1 if (@ARGV);

while (@ARGV) {
    my $a = shift(@ARGV);
    if ($a eq "--help") {
        $option_help = 1;
    } elsif ($a eq "--explain") {
        $option_explain = 1;
    } elsif ($a eq "--tsv") {
        $option_format = "tsv";
    } elsif ($a eq "--csv") {
        $option_format = "csv";
    } elsif ($a eq "cat") {
        push(@$subcommands, [$subcommand, @$subcommand_args]) if (defined($subcommand));
        $subcommand = $a;
        $subcommand_args = [];
    } elsif ($a eq "take" || $a eq "head") {
        $a = "take";
        push(@$subcommands, [$subcommand, @$subcommand_args]) if (defined($subcommand));
        $subcommand = $a;
        $subcommand_args = ['10'];
    } elsif ($a eq "drop") {
        push(@$subcommands, [$subcommand, @$subcommand_args]) if (defined($subcommand));
        $subcommand = $a;
        $subcommand_args = ['10'];
    } elsif ($a eq "cut") {
        push(@$subcommands, [$subcommand, @$subcommand_args]) if (defined($subcommand));
        $subcommand = $a;
        $subcommand_args = [];
    } elsif ($a eq "wcl") {
        push(@$subcommands, [$subcommand, @$subcommand_args]) if (defined($subcommand));
        $subcommand = $a;
        $subcommand_args = [];
    } elsif ($a eq "summary") {
        push(@$subcommands, [$subcommand, @$subcommand_args]) if (defined($subcommand));
        $subcommand = $a;
        $subcommand_args = [];
    } elsif ($a eq "countcols") {
        push(@$subcommands, [$subcommand, @$subcommand_args]) if (defined($subcommand));
        $subcommand = $a;
        $subcommand_args = [];
    } elsif ($a eq "addcol") {
        push(@$subcommands, [$subcommand, @$subcommand_args]) if (defined($subcommand));
        $subcommand = $a;
        $subcommand_args = [];
        $addcol_name = undef;
        $addcol_value = undef;
    } elsif ($a eq "-i") {
        die "option -i needs an argument" unless (@ARGV);
        $option_input = shift(@ARGV);
    } elsif ($a eq "-o") {
        die "option -o needs an argument" unless (@ARGV);
        $option_output = shift(@ARGV);
    } elsif ($a eq "--i-header") {
        die "option --i-header needs an argument" unless (@ARGV);
        $option_input_headers = shift(@ARGV);
    } elsif ($a eq "--o-no-header") {
        $option_output_headers_flag = '';
    } elsif (!defined($option_input) && -e $a) {
        $option_input = $a;
    } elsif (defined($subcommand)) {
        if ($subcommand eq "take" || $subcommand eq "drop") {
            my $num = undef;
            if ($a eq "-n") {
                die "option -n needs an argument" unless (@ARGV);
                $num = shift(@ARGV);
                die "option -n needs a number argument" unless ($num =~ /\A(0|[1-9][0-9]*)\z/);
            } elsif ($a =~ /\A-n(0|[1-9][0-9]*)\z/) {
                $num = $1;
            } elsif ($a =~ /\A(0|[1-9][0-9]*)\z/) {
                $num = $a;
            } else {
                die "Unknown argument: $a";
            }
            if (defined($num)) {
                $subcommand_args = [$num];
            }
        } elsif ($subcommand eq "cut") {
            if ($a eq "--col" || $a eq "--cols" || $a eq "--columns") {
                die "option $a needs an argument" unless (@ARGV);
                push(@$subcommand_args, '--col', shift(@ARGV));
            } else {
                push(@$subcommand_args, '--col', $a);
            }
        } elsif ($subcommand eq "addcol") {
            if ($a eq "--name") {
                die "option $a needs an argument" unless (@ARGV);
                $addcol_name = shift(@ARGV);
                push(@$subcommand_args, '--name', $addcol_name);
            } elsif ($a eq "--value") {
                die "option $a needs an argument" unless (@ARGV);
                $addcol_value = shift(@ARGV);
                push(@$subcommand_args, '--value', $addcol_value);
            } elsif (!defined($addcol_name)) {
                $addcol_name = $a;
                push(@$subcommand_args, '--name', escape_for_bash($addcol_name));
            } elsif (!defined($addcol_value)) {
                $addcol_value = $a;
                push(@$subcommand_args, '--value', escape_for_bash($addcol_value));
            } else {
                die "Unknown argument: $a";
            }
        } else {
            die "Unknown argument: $a";
        }
    } else {
        die "Unknown argument: $a";
    }
}

push(@$subcommands, [$subcommand, @$subcommand_args]) if (defined($subcommand));

if (!$isInputTty && !defined($option_input) && !$option_help) {
    # 入力がパイプにつながっていて
    # 入力ファイル名が指定されていなくて
    # ヘルプオプションも指定されていない場合は、
    # 標準入力を入力とする。
    $option_input = ""; # stdin
}

if (defined($option_input) && !@$subcommands) {
    # 入力があり、サブコマンドが指定されていない場合は、
    # サブコマンドを cat とする。
    push(@$subcommands, ["cat"]);
}

if ($isOutputTty && $option_output eq "") {
    # 出力が端末の場合
    # TODO オプションで出力フォーマットが指定されていない場合に限定
    $option_output_format = "tty";
} else {
    $option_output_format = "";
}

################################################################################
# help
################################################################################

my $help_stdout = undef;
my $help_stderr = undef;
if ($option_help) {
    $help_stdout = 1;
} elsif (!defined($option_input)) {
    if ($exists_args) {
        # 入力がない場合は、
        # ヘルプをエラー出力する。
        $help_stderr = 1;
    } else {
        # なにもパラメータがない場合は、 --help を付けたのと同じ扱いとする
        $help_stdout = 1;
    }
}

if ($help_stdout || $help_stderr) {
    my $help_filepath = $TOOL_DIR . "/help.txt";
    open(IN, '<', $help_filepath) or die $!;
    my @lines = <IN>;
    my $str = join('', @lines);
    close IN;
    if ($help_stderr) {
        open(STDOUT, '>&=', fileno(STDERR));
    }
    if ($isOutputTty) {
        exec("less", "-SRXF", "$TOOL_DIR/help.txt");
    } else {
        exec("cat", "$TOOL_DIR/help.txt");
    }
}

################################################################################
# 入出力を stdin, stdout に統一
################################################################################

if ($option_input ne "") {
    # 入力がファイルの場合
    my $data_in;
    open($data_in, '<', $option_input) or die "Cannot open file: $!";
    open(STDIN, '<&=', fileno($data_in));
}

if ($option_output ne "") {
    # 出力がファイルの場合
    my $data_out;
    open($data_out, '>', $option_output) or die "Cannot open file: $!";
    open(STDOUT, '>&=', fileno($data_out));
}

################################################################################
# guess format ...
################################################################################

sub guess_format {
    my ($head_buf) = @_;
    if ($head_buf =~ /\t/) {
        return "tsv";
    } elsif ($head_buf =~ /,/) {
        return "csv";
    } else {
        # failed to guess format
        return "tsv";
    }
}

sub guess_charencoding {
    my ($head_buf) = @_;
    my $len = length($head_buf);
    my $utf8_multi = 0;
    my $utf8_flag = 1;
    my $sjis_multi = 0;
    my $sjis_flag = 1;
    for (my $i = 0; $i < $len; $i++) {
        my $b = ord(substr($head_buf, $i, 1));
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
            elsif ($b >= 0xE0 && $b <= 0xFC) { $sjis_multi = 1; }
            else                             { $sjis_flag = ''; }
        }
    }
    if (!$utf8_flag && $sjis_flag) {
        return "SHIFT-JIS";
    } else {
        return "UTF-8";
    }
}

my $head_size = 100 * 4096;
my $head_buf;

sysread(STDIN, $head_buf, $head_size);

my $format;
if (defined($option_format)) {
    $format = $option_format;
} else {
    $format = guess_format($head_buf);
}
my $charencoding = guess_charencoding($head_buf);

################################################################################
# subcommand list to intermediate code
################################################################################

my $ircode = [["cmd", "cat"]];

if ($charencoding ne "UTF-8") {
    push(@$ircode, ["cmd", "iconv -f $charencoding -t UTF-8"]);
}

if ($format eq "csv") {
    push(@$ircode, ["cmd", "\$TOOL_DIR/golang.bin csv2tsv"]);
}

if (defined($option_input_headers)) {
    my @headers = split(/,/, $option_input_headers);
    for my $h (@headers) {
        unless ($h =~ /\A[_0-9a-zA-Z][-_0-9a-zA-Z]*\z/) {
            die "Illegal header: $h\n";
        }
    }
    my $headers = escape_for_bash(join("\t", @headers));
    $ircode = [["seq",
                [["cmd", "printf '%s' $headers"],
                 ["cmd", "echo"],
                 ["pipe", $ircode]]]];
}

my $last_subcommand = undef;
foreach my $t (@$subcommands) {
    $subcommand = shift(@$t);
    $subcommand_args = $t;
    if (defined($last_subcommand) && $last_subcommand eq "wcl") {
        die "subcommand `wcl` must be last`\n";
    }
    if ($subcommand eq "take") {
        my $num = $subcommand_args->[0];
        my $arg = escape_for_bash('-n' . ($num + 1));

        push(@$ircode, ["cmd", "head $arg"]);
    } elsif ($subcommand eq "drop") {
        my $num = $subcommand_args->[0];
        my $arg = escape_for_bash(($num + 2) . ',$p');

        push(@$ircode, ["cmd", "sed -n -e 1p -e $arg"]);
    } elsif ($subcommand eq "cut") {
        push(@$ircode, ["cmd", "perl \$TOOL_DIR/cut.pl @$subcommand_args"]);
    } elsif ($subcommand eq "wcl") {
        push(@$ircode, ["cmd", "\$TOOL_DIR/golang.bin wcl --header @$subcommand_args"]);
    } elsif ($subcommand eq "summary") {
        push(@$ircode, ["cmd", "perl \$TOOL_DIR/summary.pl @$subcommand_args"]);
    } elsif ($subcommand eq "countcols") {
        push(@$ircode, ["cmd", "perl \$TOOL_DIR/countcols.pl @$subcommand_args"]);
    } elsif ($subcommand eq "addcol") {
        push(@$ircode, ["cmd", "perl \$TOOL_DIR/addcol.pl @$subcommand_args"]);
    }
    $last_subcommand = $subcommand;
}

if ($last_subcommand ne "wcl" && $option_output_format eq "tty") {
    my $table_option = "";
    if ($last_subcommand ne "countcols") {
        $table_option .= " --col-number";
        $table_option .= " --record-number";
    }
    if ($last_subcommand eq "summary") {
        $table_option .= " --max-width 500";
    }
    push(@$ircode, ["cmd", "perl \$TOOL_DIR/table.pl$table_option"]);
    push(@$ircode, ["cmd", "less -SRX"]);
}

if ($last_subcommand ne "wcl" && !$option_output_headers_flag && $option_output_format ne "tty") {
    push(@$ircode, ["cmd", "tail -n+2"]);
}

$ircode = ["pipe", $ircode];

################################################################################
# intermediate code to shell script
################################################################################

sub irToShellscript {
    my ($code) = @_;
    my $type = $code->[0];
    if ($type eq "pipe") {
        my @cs = @{$code->[1]};
        if (!@cs) {
            [":"];
        } elsif ((scalar @cs) == 1) {
            irToShellscript($cs[0]);
        } else {
            joinShellscriptLines([map { irToShellscript($_) } @cs], "", "", " |", "    ", "    ", " |", "    ", "    ", "");
        }
    } elsif ($type eq "seq") {
        my @cs = @{$code->[1]};
        if (!@cs) {
            [":"];
        } elsif ((scalar @cs) == 1) {
            irToShellscript($cs[0]);
        } else {
            joinShellscriptLines([map { irToShellscript($_) } @cs], "( ", "  ", ";", "  ", "  ", ";", "  ", "  ", ")");
        }
    } elsif ($type eq "cmd") {
        my $script = $code->[1];
        [$script];
    }
}

sub joinShellscriptLines {
    my ($sources, $begin, $begin_b, $end0, $begin1, $begin1_b, $end1, $begin2, $begin2_b, $end) = @_;

    if (@$sources == 0) {
        die;
    } elsif (@$sources == 1) {
        die;
    }

    my $first = shift(@$sources);
    my $last  = pop(@$sources);

    [
     @{joinShellscriptLinesSub($first, $begin, $begin_b, $end0)},
     (map { @{joinShellscriptLinesSub($_, $begin1, $begin1_b, $end1)} } @$sources),
     @{joinShellscriptLinesSub($last,  $begin2, $begin2_b, $end)}
    ];
}

sub joinShellscriptLinesSub {
    my ($sources, $begin, $begin_b, $end) = @_;

    if (@$sources == 0) {
        die;
    }
    if (@$sources == 1) {
        return [$begin . $sources->[0] . $end];
    }

    my $first = shift(@$sources);
    my $last  = pop(@$sources);

    [
     $begin . $first,
     (map { $begin_b . $_ } @$sources),
     $begin_b . $last . $end,
    ];
}

my $main_1_source = join("\n", @{irToShellscript($ircode)}) . "\n";

open(my $main_1_out, '>', "$WORKING_DIR/main-1.sh") or die $!;
print $main_1_out $main_1_source;
close($main_1_out);

if ($option_explain) {
    my $view = $main_1_source;
    $view =~ s/^/> /gm;
    print STDERR $view;
}

################################################################################
# exec script
################################################################################

my $PARENT_READER;
my $CHILD_WRITER;
pipe($PARENT_READER, $CHILD_WRITER);

my $pid1 = fork;
if (!defined $pid1) {
    die;
} elsif ($pid1) {
    # parent process

    close $CHILD_WRITER;
    open(STDIN, '<&=', fileno($PARENT_READER));

    exec("bash", "$WORKING_DIR/main-1.sh");
} else {
    # child process

    close $PARENT_READER;
    open(STDOUT, '>&=', fileno($CHILD_WRITER));

    syswrite(STDOUT, $head_buf);
    exec("cat");
}

################################################################################
