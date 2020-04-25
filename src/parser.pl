use strict;
use warnings;
use utf8;

use File::Path qw/mkpath/;
use Data::Dumper;

use POSIX qw/mkfifo/;

our $true = 1;
our $false = "";

require "$ENV{XSVUTILS_HOME}/src/commands-info.pl";
our %command_options;

my @help_files = qw/
    format
    install
    list
    main
    options
    usage
/;
# 新バージョンに対応していない旧バージョン(-vo)のヘルプファイルをいったん削除するのは嫌なので、
# 当面の間は、ヘルプファイルの一覧を上記配列で管理することにする

################################################################################

my $isInputTty = $false;
if (-t STDIN) {
    $isInputTty = $true;
}
my $isOutputTty = $false;
if (-t STDOUT) {
    $isOutputTty = $true;
}

$ENV{"LC_ALL"} = "C";

################################################################################
# help
################################################################################

sub execHelp {
    my ($name) = @_;
    my $path;
    my $path1 = $ENV{"XSVUTILS_HOME"} . "/src/" . $name . ".help.txt";
    my $path2 = $ENV{"XSVUTILS_HOME"} . "/src/help-" . $name . ".txt";
    if (-e $path1 && $command_options{$name}->{"exists_help"}) {
        # 新バージョンに対応していない旧バージョン(-vo)のヘルプファイルをいったん削除するのは嫌なので、
        # ヘルプファイルが有効かどうかを exists_help というフラグで判断することにする
        $path = $path1;
    } elsif (-e $path2 && grep {$_ eq $name} @help_files) {
        $path = $path2;
    } else {
        print STDERR "Help document not found: $name\n";
        exit(1);
    }

    my $READER;
    my $WRITER;
    pipe($READER, $WRITER);
    my $pid1 = fork;
    if (!defined $pid1) {
        die $!;
    } elsif ($pid1) {
        # parent process
        close $WRITER;
        open(STDIN, '<&=', fileno($READER));
        if ($isOutputTty) {
            exec("less", "-SRXF");
        } else {
            exec("cat");
        }
    } else {
        # child process
        close($READER);
        open(STDOUT, '>&=', fileno($WRITER));
        open(my $help_fp, "<", $path);
        while (my $line = <$help_fp>) {
            if ($line !~ /\A#/) {
                print $line;
            }
        }
        close($help_fp);
        exit(0);
    }
}

sub existsHelp {
    my ($name) = @_;
    my $path1 = $ENV{"XSVUTILS_HOME"} . "/src/" . $name . ".help.txt";
    my $path2 = $ENV{"XSVUTILS_HOME"} . "/src/help-" . $name . ".txt";
    if (-e $path1) {
        return $true;
    } elsif (-e $path2) {
        return $true;
    } else {
        return $false;
    }
}

sub parseAndExecHelpQuery {
    my ($node, $arg) = @_;
    if (defined($node)) {
        execHelp($node->{"command_name"});
    } else {
        if (defined($arg)) {
            if (existsHelp($arg)) {
                execHelp($arg);
            }
        }
        execHelp("main");
    }
}

################################################################################

# --explainオプションが指定されているかどうか
my $is_explain = $false;
my $global_option_version = $false;

################################################################################

sub parseQuery {
    my ($argv, $subqueryCommandName, $is_global, $is_strict_mode, $inputMode, $outputMode) = @_;
    # $inputMode="must"
    # $inputMode="may"
    # $inputMode="mustNot"
    # $outputMode="must"
    # $outputMode="may"
    # $outputMode="mustNot"

    # 4値を返す関数。
    # 1つ目の返り値の例
    # 2つ目は閉じ括弧よりも後ろの残ったパラメータの配列
    # 3つ目はコマンド補完の情報 (未実装)
    # 4つ目はエラーの情報

    my @argv_orig = @$argv;
    my @argv = @argv_orig;

    my @nodes = ();
    my $input_node = undef;
    my $output_node = undef;
    my $completion = undef;

    my $curr_node = undef;
    my $curr_cmd_parameters = undef;
    my $curr_cmd_options = undef;
    my $curr_cmd_parameters_idx = undef;

    my $input_filepath = undef;
    my $output_filepath = undef;

    my $query_options = {};

    while () {
        my $a;
        if (@$argv) {
            $a = shift(@$argv);
        } else {
            last;
        }

        if ($a eq "]") {
            if ($is_global) {
                return (undef, undef, undef, "Unexpected ']'")
            }
            last;
        }

        # サブコマンドのオプション
        if (defined($curr_node)) {
            my $op = $curr_cmd_options->{$a};
            if (defined($op)) {
                if ($op eq "") {
                    $curr_node->{"options"}->{$a} = "";
                    next;
                }

                if (!@$argv) {
                    return (undef, undef, [$op], "Option $a needs parameter");
                }

                # サブコマンドのオプションパラメータ
                if ($op =~ /\AA:/) {
                    if (!defined($curr_node->{"options"}->{$a})) {
                        $curr_node->{"options"}->{$a} = [];
                    }
                    push(@{$curr_node->{"options"}->{$a}}, shift(@$argv));
                } else {
                    $curr_node->{"options"}->{$a} = shift(@$argv);
                }
                next;
            }
        }

        # グローバルオプション
        if ($true) {
            if ($a eq "--explain") {
                $is_explain = $true;
                next;
            }
            if ($a eq "--version") {
                $global_option_version = $true;
                next;
            }
            if ($a eq "--help") {
                my $helpArg = undef;
                $helpArg = $argv->[0] if (@$argv);
                parseAndExecHelpQuery($curr_node, $helpArg);
            }
        }

        # クエリオプション
        if ($true) {
            if ($a eq "--strict") {
                unless ($is_strict_mode) {
                    return (parseQuery(\@argv_orig, $subqueryCommandName, $is_global, $true, $inputMode, $outputMode));
                }
                next;
            }
            if ($a eq "--tsv") {
                unless ($inputMode eq "must" || $inputMode eq "may") {
                    return (undef, undef, undef, "--tsv not allowed");
                }
                $query_options->{"--tsv"} = "";
                delete($query_options->{"--csv"});
                next;
            }
            if ($a eq "--csv") {
                unless ($inputMode eq "must" || $inputMode eq "may") {
                    return (undef, undef, undef, "--csv not allowed");
                }
                $query_options->{"--csv"} = "";
                delete($query_options->{"--tsv"});
                next;
            }
            if ($a eq "--o-tsv") {
                unless ($outputMode eq "must" || $outputMode eq "may") {
                    return (undef, undef, undef, "--o-tsv not allowed");
                }
                $query_options->{"--o-tsv"} = "";
                delete($query_options->{"--o-csv"});
                next;
            }
            if ($a eq "--o-csv") {
                unless ($outputMode eq "must" || $outputMode eq "may") {
                    return (undef, undef, undef, "--o-csv not allowed");
                }
                $query_options->{"--o-csv"} = "";
                delete($query_options->{"--o-tsv"});
                next;
            }
            if ($a eq "--o-no-header") {
                unless ($outputMode eq "must" || $outputMode eq "may") {
                    return (undef, undef, undef, "--o-no-header not allowed");
                }
                $query_options->{"--o-no-header"} = "";
                next;
            }
            if ($a eq "-i") {
                unless ($inputMode eq "must" || $inputMode eq "may") {
                    return (undef, undef, undef, "File path of input not allowed");
                }
                if (defined($input_filepath)) {
                    return (undef, undef, undef, "Duplicated option: $a");
                }
                if (!@$argv) {
                    return (undef, undef, ["FILE"], "Option $a needs parameter");
                }
                $input_filepath = shift(@$argv);
                next;
            }
            if ($a eq "-o") {
                unless ($outputMode eq "must" || $outputMode eq "may") {
                    return (undef, undef, undef, "File path of output not allowed");
                }
                if (defined($output_filepath)) {
                    return (undef, undef, undef, "Duplicated option: $a");
                }
                if (!@$argv) {
                    return (undef, undef, ["FILE"], "Option $a needs parameter");
                }
                $output_filepath = shift(@$argv);
                next;
            }
        }

        if ($a =~ /\A-/) {
            return (undef, undef, undef, "Unknown option: $a");
        }

        if ($a eq "help") {
            my $helpArg = undef;
            $helpArg = $argv->[0] if (@$argv);
            parseAndExecHelpQuery(undef, $helpArg);
        }

        # サブコマンド名
        my $co = $command_options{$a};
        if (defined($co) && !$co->{"is_internal"}) {
            my $next_node = {};
            $next_node->{"command_name"} = $a;
            $next_node->{"options"} = {};
            $next_node->{"connections"} = {};

            if (defined($curr_node)) {
                $curr_node->{"connections"}->{"output"} = [$next_node, "input"];
                $next_node->{"connections"}->{"input"} = [$curr_node, "output"];
            } else {
                $input_node = $next_node;
            }
            $curr_node = $next_node;
            $output_node = $curr_node;

            $curr_cmd_parameters = [@{$co->{"parameters"}}];
            $curr_cmd_options = $co->{"options"};

            push(@nodes, $curr_node);

            next;
        }

        if ($is_strict_mode) {
            return (undef, undef,undef, "Parameter not allowed");
        }

        if (!defined($input_filepath)) {
            if ($inputMode eq "must" || $inputMode eq "may") {
                if (@nodes <= 1) {
                    if (-f $a || -p $a) {
                        $input_filepath = $a;
                        next;
                    }
                }
            }
        }

        # サブコマンドのパラメータ
        if (defined($curr_node)) {
            my $o = undef;
            while (@$curr_cmd_parameters) {
                $o = shift(@$curr_cmd_parameters);
                if (!defined($curr_node->{"options"}->{$o})) {
                    last;
                }
                if ($curr_cmd_options->{$o} =~ /\AA:/) {
                    unshift(@$curr_cmd_parameters, $o);
                }
                $o = undef;
            }
            if (defined($o)) {
                if ($curr_node->{"command_name"} eq "join" && $curr_cmd_options->{$o} eq "FILE") {
                    # SPECIAL IMPL FOR join
                    my $argv2 = [@$argv];
                    if ($a ne "[") {
                        unshift(@$argv2, "]");
                        unshift(@$argv2, $a);
                        unshift(@$argv2, "-i");
                    }
                    my ($subGraph, $subArgv, $subCompletion, $subError) =
                        parseQuery($argv2, $curr_node->{"command_name"},
                                   $false, $is_strict_mode, "must", "mustNot");
                    if (!defined($subArgv) && defined($subCompletion) && @$subCompletion) {
                        return (undef, undef, $subCompletion, $subError);
                    }
                    if (defined($subError)) {
                        return (undef, undef, undef, $subError);
                    }

                    $curr_node->{"options"}->{$o} = $subGraph;
                    $argv = $subArgv;

                    pop(@nodes);
                    push(@nodes, @{$subGraph->{"nodes"}});
                    push(@nodes, $curr_node);

                    $subGraph->{"output"}->{"connections"}->{"output"} = [$curr_node, $o];
                    $curr_node->{"connections"}->{$o} = [$subGraph->{"output"}, "output"];
                } elsif ($curr_cmd_options->{$o} =~ /\AA:/) {
                    push(@{$curr_node->{"options"}->{$o}}, $a);
                } else {
                    $curr_node->{"options"}->{$o} = $a;
                }
                next;
            }

            die "Unknown parameter: $a";
        }

        die "Unknown parameter: \"$a\"";
    }

    if (defined($input_filepath)) {
        my $input_node2 = createInputNode($input_filepath, $query_options);
        if (@nodes) {
            $input_node2->{"connections"}->{"output"} = [$input_node, "input"];
            $input_node->{"connections"}->{"input"} = [$input_node2, "output"];
        } else {
            $output_node = $input_node2;
        }
        unshift(@nodes, $input_node2);
        $input_node = $input_node2;
    }
    if (defined($output_filepath)) {
        my $output_node2 = createOutputNode($output_filepath, $query_options, "");
        if (@nodes) {
            $output_node2->{"connections"}->{"input"} = [$output_node, "output"];
            $output_node->{"connections"}->{"output"} = [$output_node2, "input"];
        } else {
            $input_node = $output_node2;
        }
        push(@nodes, $output_node2);
        $output_node = $output_node2;
    }

    if (!@nodes) {
        $input_node = {
            "command_name" => "cat",
            "parameters" => [],
            "options" => {},
            "connections" => {},
        };
        $output_node = $input_node;
        push(@nodes, $input_node);
    }

    my $input_node_input_format = $command_options{$input_node->{"command_name"}}->{"input"}->[0];
    if ($inputMode eq "must" && $input_node_input_format ne "deny") {
        return (undef, undef, undef, "sub query of '$subqueryCommandName' must have input");
    }
    if ($inputMode eq "mustNot" && $input_node_input_format eq "deny") {
        return (undef, undef, undef, "sub query of '$subqueryCommandName' must not have input");
    }

    my $output_node_output_format;
    if ((ref $command_options{$output_node->{"command_name"}}->{"output"}) eq "CODE") {
        $output_node_output_format = [""];
    } else {
        $output_node_output_format = $command_options{$output_node->{"command_name"}}->{"output"};
    }
    if ($outputMode eq "must" && $output_node_output_format->[0] ne "deny") {
        return (undef, undef, undef, "sub query of '$subqueryCommandName' must have output");
    }
    if ($outputMode eq "mustNot" && $output_node_output_format->[0] eq "deny") {
        return (undef, undef, undef, "sub query of '$subqueryCommandName' must not have output");
    }

    my $graph = {
        "input" => $input_node,
        "output" => $output_node,
        "nodes" => \@nodes,
        "options" => $query_options,
    };
    return ($graph, $argv, $completion, undef);
}

################################################################################

sub insertNode {
    my ($nodes, $index, $node) = @_;
    return [@$nodes[0..$index-1], $node, @$nodes[$index..(@$nodes-1)]];
}

sub removeNode {
    my ($nodes, $index) = @_;
    if ($index >= 1 && $index <= @$nodes - 2) {
        return [@$nodes[0..$index-1], @$nodes[($index+1)..(@$nodes-1)]];
    } elsif ($index >= 1) {
        return [@$nodes[0..$index-1]];
    } elsif ($index <= @$nodes - 2) {
        return [@$nodes[($index+1)..(@$nodes-1)]];
    } else {
        return [];
    }
}

################################################################################

sub createInputNode {
    my ($input_filepath, $options) = @_;
    my $input_node2 = {
        "command_name" => "read-file",
        "parameters" => [],
        "options" => {},
        "connections" => {},
    };
    if (defined($input_filepath)) {
        $input_node2->{"options"}->{"-i"} = $input_filepath;
    }
    if (defined($options->{"--tsv"})) {
        $input_node2->{"options"}->{"--tsv"} = "";
    } elsif (defined($options->{"--csv"})) {
        $input_node2->{"options"}->{"--csv"} = "";
    }
    return $input_node2;
}

sub connectStdin {
    my ($graph, $isInputTty) = @_;
    my $input_node = $graph->{"input"};
    my $command_name = $input_node->{"command_name"};
    my $coi = $command_options{$command_name};
    if ($coi->{"input"}->[0] eq "deny") {
        return;
    }
    if ($isInputTty) {
        parseAndExecHelpQuery(undef, undef);
    }
    my $input_node2 = createInputNode(undef, $graph->{"options"});
    $input_node2->{"connections"}->{"output"} = [$input_node, "input"];
    $input_node->{"connections"}->{"input"} = [$input_node2, "output"];
    $graph->{"input"} = $input_node2;
    unshift(@{$graph->{"nodes"}}, $input_node2);
}

################################################################################

sub createOutputNode {
    my ($output_filepath, $options, $isTerminal) = @_;
    my $output_node2 = {
        "command_name" => "write-file",
        "options" => {},
        "connections" => {},
        "internal" => {},
    };
    if (defined($output_filepath)) {
        $output_node2->{"options"}->{"-o"} = $output_filepath;
    }
    if ($isTerminal) {
        $output_node2->{"options"}->{"--terminal"} = "";
    }
    $output_node2->{"internal"}->{"format"} = "any";
    if (defined($options->{"--o-tsv"})) {
        $output_node2->{"internal"}->{"format"} = "tsv";
    } elsif (defined($options->{"--o-csv"})) {
        $output_node2->{"internal"}->{"format"} = "csv";
    }
    if (defined($options->{"--o-no-header"})) {
        $output_node2->{"internal"}->{"--o-no-header"} = "";
    }
    return $output_node2;
}

sub connectStdout {
    my ($graph, $isOutputTty) = @_;
    my $output_node = $graph->{"output"};
    my $command_name = $output_node->{"command_name"};
    my $coi = $command_options{$command_name};
    my $output_format;
    if ((ref $coi->{"output"}) eq "CODE") {
        $output_format = [""];
    } else {
        $output_format = $coi->{"output"};
    }
    if ($output_format->[0] eq "deny") {
        return;
    }
    my $output_node2;
    my $isTerminal = "";
    if ($isOutputTty) {
        # ターミナルへの標準出力は出力を整形する
        $isTerminal = 1;
    }
    $output_node2 = createOutputNode(undef, $graph->{"options"}, $isTerminal);
    $output_node2->{"connections"}->{"input"} = [$output_node, "output"];
    $output_node->{"connections"}->{"output"} = [$output_node2, "input"];
    $graph->{"output"} = $output_node2;
    push(@{$graph->{"nodes"}}, $output_node2);
}

################################################################################

sub forkFormatWrapper {
    my ($node) = @_;

    my $fifoIdx = $node->{"internal"}->{"fifo"};
    my $format_result_path = formatWrapperFormatPathRaw($fifoIdx);
    my $output_path = formatWrapperDataPathRaw($fifoIdx);

    mkfifo($format_result_path, 0600) or die $!;

    my @cmd = ();
    push(@cmd, "perl");
    push(@cmd, "$ENV{XSVUTILS_HOME}/src/format-wrapper.pl");

    if (defined($node->{"options"}->{"-i"})) {
        push(@cmd, "-i");
        push(@cmd, $node->{"options"}->{"-i"});
    }

    if (defined($node->{"options"}->{"--tsv"})) {
        push(@cmd, "--tsv");
        delete($node->{"options"}->{"--tsv"});
    }
    if (defined($node->{"options"}->{"--csv"})) {
        push(@cmd, "--csv");
        delete($node->{"options"}->{"--csv"});
    }

    push(@cmd, "-o");
    push(@cmd, $output_path);

    push(@cmd, $format_result_path);

    my $pid1 = fork;
    if (!defined $pid1) {
        die $!;
    } elsif (!$pid1) {
        # child process
        exec(@cmd);
    }
}

sub fetchFormatWrapperResult {
    my ($node) = @_;

    my $fifoIdx = $node->{"internal"}->{"fifo"};
    my $format_result_path = formatWrapperFormatPathRaw($fifoIdx);
    my $output_path = formatWrapperDataPathRaw($fifoIdx);

    open(my $format_fh, '<', $format_result_path) or die $!;
    my $format = <$format_fh>;
    close($format_fh);

    $format =~ s/\n\z//g;
    if ($format !~ /\Aformat:([^ ]+) charencoding:([^ ]+) utf8bom:([^ ]+) newline:([^ ]+) mode:([^ ]+)\z/) {
        die "failed to guess format";
    }
    $node->{"internal"}->{"format-result"} = $format;
    $node->{"internal"}->{"format"}       = $1; # tsv, csv, json, text
    $node->{"internal"}->{"charencoding"} = $2; # UTF-8, SHIFT-JIS
    $node->{"internal"}->{"utf8bom"}      = $3; # 0, 1
    $node->{"internal"}->{"newline"}      = $4; # unix, dos, mac
    $node->{"internal"}->{"mode"}         = $5; # pipe, file

    if (defined($node->{"options"}->{"-i"})) {
        $node->{"internal"}->{"input"} = $node->{"options"}->{"-i"};
    } else {
        $node->{"internal"}->{"input"} = "";
    }
    if ($node->{"internal"}->{"mode"} eq "pipe") {
        delete($node->{"options"}->{"-i"});
    }

    if ($node->{"internal"}->{"format"} eq "csv") {
        $node->{"connections"}->{"output"}->[2] = ["csv", "lf"];
    } elsif ($node->{"internal"}->{"format"} eq "json") {
        $node->{"connections"}->{"output"}->[2] = ["json", "lf"];
    } elsif ($node->{"internal"}->{"format"} eq "text") {
        $node->{"connections"}->{"output"}->[2] = ["text", "lf"];
    }
}

sub executeFormatWrapper {
    my ($graph) = @_;
    my $nodes = $graph->{"nodes"};

    # format-wrapper.pl を起動
    for (my $i = 0; $i < @$nodes; $i++) {
        my $node = $nodes->[$i];
        my $command_name = $node->{"command_name"};
        if ($command_name eq "read-file") {
            my $fifoIdx = fetchFifoIdx();
            # internalの利用はいまのところread-fileのみ
            $node->{"internal"} = {};
            $node->{"internal"}->{"fifo"} = $fifoIdx;
            forkFormatWrapper($node);
        }
    }

    # 起動した format-wrapper.pl からフォーマット推定情報を読み取り
    for (my $i = 0; $i < @$nodes; $i++) {
        my $node = $nodes->[$i];
        my $command_name = $node->{"command_name"};
        if ($command_name eq "read-file") {
            fetchFormatWrapperResult($node);
        }
    }
}

################################################################################

sub walkPhase0 {
    my ($graph) = @_;
    my $nodes = $graph->{"nodes"};
    for (my $i = 0; $i < @$nodes; $i++) {
        my $node = $nodes->[$i];
        my $command_name = $node->{"command_name"};
        if ($command_name eq "cat") {
            my $input = $node->{"connections"}->{"input"};
            my $output = $node->{"connections"}->{"output"};
            $input->[0]->{"connections"}->{$input->[1]}->[0] = $output->[0];
            $input->[0]->{"connections"}->{$input->[1]}->[1] = $output->[1];
            $output->[0]->{"connections"}->{$output->[1]}->[0] = $input->[0];
            $output->[0]->{"connections"}->{$output->[1]}->[1] = $input->[1];
            $nodes = [(@$nodes)[0..($i - 1)], (@$nodes)[($i + 1)..(@$nodes - 1)]];
            $i--;
        }
    }
    $graph->{"nodes"} = $nodes;
}

################################################################################

# 各ノードの入力と出力が必要な箇所で接続されているかどうか
# 各ノードの入力や出力のない箇所で接続されていないかどうか
# を検査する
sub walkPhase1 {
    my ($graph) = @_;
    my $nodes = $graph->{"nodes"};
    for (my $i = 0; $i < @$nodes; $i++) {
        my $node = $nodes->[$i];
        my $command_name = $node->{"command_name"};
        my $coi = $command_options{$command_name};
        if ($coi->{"input"}->[0] eq "deny") {
            if (defined($node->{"connections"}->{"input"})) {
                die "`$command_name` subcommand must not have input.";
            }
        } else {
            if (!defined($node->{"connections"}->{"input"})) {
                die "`$command_name` subcommand must have input.";
            }
        }
        my $output_format;
        if ((ref $coi->{"output"}) eq "CODE") {
            $output_format = [""];
        } else {
            $output_format = $coi->{"output"};
        }
        if ($output_format->[0] eq "deny") {
            if (defined($node->{"connections"}->{"output"})) {
                die "`$command_name` subcommand must not have output.";
            }
        } else {
            if (!defined($node->{"connections"}->{"output"})) {
                die "`$command_name` subcommand must have output.";
            }
        }
    }
}

################################################################################

sub insertCsvToTsvNode {
    my ($nodes, $index, $inputName) = @_;
    my $newNode = {
        "command_name" => "from-csv",
        "options" => {},
        "connections" => {},
    };
    my $node = $nodes->[$index];
    my $inputNodeInfo = $node->{"connections"}->{$inputName};
    $inputNodeInfo->[0]->{"connections"}->{$inputNodeInfo->[1]}->[0] = $newNode;
    $inputNodeInfo->[0]->{"connections"}->{$inputNodeInfo->[1]}->[1] = "input";
    $newNode->{"connections"}->{"input"} = [@$inputNodeInfo];
    $newNode->{"connections"}->{"output"} = [$node, $inputName, ["tsv", "lf"]];
    $node->{"connections"}->{$inputName} = [$newNode, "output", ["tsv", "lf"]];
    return insertNode($nodes, $index, $newNode);
}

sub insertTsvToCsvNode {
    my ($nodes, $index, $inputName) = @_;
    my $newNode = {
        "command_name" => "to-csv",
        "options" => {},
        "connections" => {},
    };
    my $node = $nodes->[$index];
    my $inputNodeInfo = $node->{"connections"}->{$inputName};
    $inputNodeInfo->[0]->{"connections"}->{$inputNodeInfo->[1]}->[0] = $newNode;
    $inputNodeInfo->[0]->{"connections"}->{$inputNodeInfo->[1]}->[1] = "input";
    $newNode->{"connections"}->{"input"} = [@$inputNodeInfo];
    $newNode->{"connections"}->{"output"} = [$node, $inputName, ["csv", "lf"]];
    $node->{"connections"}->{$inputName} = [$newNode, "output", ["csv", "lf"]];
    return insertNode($nodes, $index, $newNode);
}

sub insertJsonToTsvNode {
    my ($nodes, $index, $inputName) = @_;
    my $newNode = {
        "command_name" => "from-json",
        "options" => {},
        "connections" => {},
    };
    my $node = $nodes->[$index];
    my $inputNodeInfo = $node->{"connections"}->{$inputName};
    $inputNodeInfo->[0]->{"connections"}->{$inputNodeInfo->[1]}->[0] = $newNode;
    $inputNodeInfo->[0]->{"connections"}->{$inputNodeInfo->[1]}->[1] = "input";
    $newNode->{"connections"}->{"input"} = [@$inputNodeInfo];
    $newNode->{"connections"}->{"output"} = [$node, $inputName, ["tsv", "lf"]];
    $node->{"connections"}->{$inputName} = [$newNode, "output", ["tsv", "lf"]];
    return insertNode($nodes, $index, $newNode);
}

sub insertNoHeaderNode {
    my ($nodes, $index, $inputName, $fmt) = @_;
    my $newNode = {
        "command_name" => "no-header",
        "options" => {},
        "connections" => {},
    };
    my $node = $nodes->[$index];
    my $inputNodeInfo = $node->{"connections"}->{$inputName};
    $inputNodeInfo->[0]->{"connections"}->{$inputNodeInfo->[1]}->[0] = $newNode;
    $inputNodeInfo->[0]->{"connections"}->{$inputNodeInfo->[1]}->[1] = "input";
    $newNode->{"connections"}->{"input"} = [@$inputNodeInfo];
    $newNode->{"connections"}->{"output"} = [$node, $inputName, [$fmt, "lf"]];
    $node->{"connections"}->{$inputName} = [$newNode, "output", [$fmt, "lf"]];
    return insertNode($nodes, $index, $newNode);
}

sub walkPhase2_modifyNode_writeFile {
    my ($nodes, $i, $node, $connectionName, $format) = @_;
    if ($node->{"internal"}->{"format"} eq "tsv") {
        if ($format->[0] eq "tsv") {
            # nothing
        } elsif ($format->[0] eq "csv") {
            # CSV->TSV 変換ノードを挿入
            $nodes = insertCsvToTsvNode($nodes, $i, $connectionName);
            $i++;
        } elsif ($format->[0] eq "json") {
            # JSON->TSV 変換ノードを挿入
            $nodes = insertJsonToTsvNode($nodes, $i, $connectionName);
            $i++;
        } else {
            die "Cannot convert format from $format->[0] to tsv.";
        }
    } elsif ($node->{"internal"}->{"format"} eq "csv") {
        if ($format->[0] eq "tsv") {
            # TSV->CSV 変換ノードを挿入
            $nodes = insertTsvToCsvNode($nodes, $i, $connectionName);
            $i++;
        } elsif ($format->[0] eq "csv") {
            # nothing
        } elsif ($format->[0] eq "json") {
            # JSON->CSV 変換ノードを挿入
            $nodes = insertJsonToTsvNode($nodes, $i, $connectionName);
            $i++;
            $nodes = insertTsvToCsvNode($nodes, $i, $connectionName);
            $i++;
        } else {
            die "Cannot convert format from $format->[0] to csv.";
        }
    } else {
        if ($format->[0] eq "tsv") {
            # nothing
        } elsif ($format->[0] eq "csv") {
            # CSV->TSV 変換ノードを挿入
            $nodes = insertCsvToTsvNode($nodes, $i, $connectionName);
            $i++;
        } elsif ($format->[0] eq "json") {
            # nothing
        } elsif ($format->[0] eq "string") {
            # nothing
        } elsif ($format->[0] eq "textsimple") {
            # nothing
        } elsif ($format->[0] eq "text") {
            # nothing
        } elsif ($format->[0] eq "image-png") {
            if (defined($node->{"options"}->{"--terminal"})) {
                die "Cannot output image file to terminal";
            }
        } else {
            die;
        }
    }
    if (defined($node->{"internal"}->{"--o-no-header"})) {
        my $fmt = $node->{"connections"}->{$connectionName}->[2]->[0];
        if ($fmt eq "tsv" || $fmt eq "csv") {
            delete($node->{"options"}->{"--terminal"});

            # ヘッダ削除のノードを挿入
            $nodes = insertNoHeaderNode($nodes, $i, $connectionName, $fmt);
            $i++;
        }
    }
    return ($nodes, $i);
}

sub walkPhase2_modifyNode_other {
    my ($nodes, $i, $node, $connectionName, $format) = @_;
    my $command_name = $node->{"command_name"};
    my $coi = $command_options{$command_name};
    if ($coi->{"input"}->[0] eq "tsv") {
        if ($format->[0] eq "tsv") {
            # nothing
        } elsif ($format->[0] eq "csv") {
            # CSV->TSV 変換ノードを挿入
            $nodes = insertCsvToTsvNode($nodes, $i, "input");
            $i++;
        } elsif ($format->[0] eq "json") {
            # JSON->TSV 変換ノードを挿入
            $nodes = insertJsonToTsvNode($nodes, $i, "input");
            $i++;
        } elsif ($format->[0] ne "tsv") {
            die "`$command_name` subcommand input must be tsv.";
        }
    } elsif ($coi->{"input"}->[0] eq "csv") {
        if ($format->[0] ne "csv") {
            die "`$command_name` subcommand input must be csv.";
        }
    } elsif ($coi->{"input"}->[0] eq "json") {
        if ($format->[0] ne "json") {
            die "`$command_name` subcommand input must be json.";
        }
    } elsif ($coi->{"input"}->[0] eq "text") {
        if ($format->[0] !~ /\A(tsv|csv|json|text|textsimple|string)\z/) {
            die "`$command_name` subcommand input must be text.";
        }
    } elsif ($coi->{"input"}->[0] eq "any") {
        # nothing
    } else {
        die;
    }
    return ($nodes, $i);
}

# 各ノード間の入出力フォーマットがあっているかどうかを検査する
# 一致していなくて可能であれば変換処理のノードを挿入する
sub walkPhase2 {
    my ($graph) = @_;
    my $nodes = $graph->{"nodes"};
    for (my $i = 0; $i < @$nodes; $i++) {
        my $node = $nodes->[$i];
        my $command_name = $node->{"command_name"};
        my $coi = $command_options{$command_name};

        # 入力側を検査
        foreach my $key (sort keys %{$node->{"connections"}}) {
            my $other = $node->{"connections"}->{$key};
            my $otherIdx = searchNodeFromNodes($nodes, $other->[0]);
            if ($otherIdx < $i) {
                my $format = $nodes->[$otherIdx]->{"connections"}->{$other->[1]}->[2];
                $node->{"connections"}->{$key}->[2] = $format;
                if ($key eq "input") {
                    if ($command_name eq "write-file") {
                        # SPECIAL IMPL FOR write-file
                        ($nodes, $i) = walkPhase2_modifyNode_writeFile($nodes, $i, $node, $key, $format);
                        $graph->{"nodes"} = $nodes;
                    } else {
                        ($nodes, $i) = walkPhase2_modifyNode_other($nodes, $i, $node, $key, $format);
                        $graph->{"nodes"} = $nodes;
                    }
                } else {
                    # TODO
                }
            }
        }

        # 出力側を検査
        foreach my $key (sort keys %{$node->{"connections"}}) {
            my $other = $node->{"connections"}->{$key};
            my $otherIdx = searchNodeFromNodes($nodes, $other->[0]);
            if ($otherIdx > $i && !defined($node->{"connections"}->{$key}->[2])) {
                # $node->{"connections"}->{$key}->[2] がこの段階で定義済みのケースはいまのところ read-file のみ
                if ($key eq "output") {
                    my $format;
                    if ((ref $coi->{"output"}) eq "CODE") {
                        $format = $coi->{"output"}->($node);
                    } else {
                        $format = $coi->{"output"};
                    }
                    $node->{"connections"}->{$key}->[2] = $format;
                } else {
                    die # TODO
                }
            }
        }
    }
}

################################################################################

# ノード単位でパラメータを見て書き換え
sub walkPhase3 {
    my ($graph) = @_;
    my $nodes = $graph->{"nodes"};
    for (my $i = 0; $i < @$nodes; $i++) {
        my $node = $nodes->[$i];
        my $command_name = $node->{"command_name"};
        if ($command_name eq "cut") {
            # SPECIAL IMPL FOR cut, cols
            $node->{"command_name"} = "col-impl";
            #$node->{"command_name"} = "cols";
        } elsif ($command_name eq "filter") {
            # SPECIAL IMPL FOR filter, where
            $node->{"command_name"} = "where";
        } elsif ($command_name eq "head" || $command_name eq "limit") {
            # SPECIAL IMPL FOR head, limit
            my $n = $node->{"options"}->{"-n"};
            if (defined($n)) {
                delete($node->{"options"}->{"-n"});
            } else {
                $n = 10;
            }
            $node->{"command_name"} = "range";
            $node->{"options"}->{"--start"} = 0;
            $node->{"options"}->{"--end"} = $n;
        } elsif ($command_name eq "offset") {
            # SPECIAL IMPL FOR offset
            my $n = $node->{"options"}->{"-n"};
            if (defined($n)) {
                delete($node->{"options"}->{"-n"});
            } else {
                $n = 10;
            }
            $node->{"command_name"} = "range";
            $node->{"options"}->{"--start"} = $n;
            $node->{"options"}->{"--end"} = -1;
        } elsif ($command_name eq "offset-random") {
            # SPECIAL IMPL FOR offset-random
            my $n = $node->{"options"}->{"-n"};
            if (defined($n)) {
                delete($node->{"options"}->{"-n"});
            } else {
                $n = 10;
            }
            $node->{"command_name"} = "range";
            $node->{"options"}->{"--start"} = int(rand($n));
            $node->{"options"}->{"--end"} = -1;
        } elsif ($command_name eq "col") {
            # SPECIAL IMPL FOR col
            my $new_node = {};
            $node->{"command_name"} = "col-impl";
            my $node_options = $node->{"options"};
            $node->{"options"} = {};

            my $cols = $node_options->{"--cols"};
            my $col = $node_options->{"--col"};
            if (defined($cols)) {
                $node->{"options"}->{"--col"} = [];
                foreach my $c (split(/,/, $cols, -1)) {
                    push(@{$node->{"options"}->{"--col"}}, $c);
                }
            } else {
                $node->{"options"}->{"--col"} = [];
            }
            if (defined($col)) {
                push(@{$node->{"options"}->{"--col"}}, @$col);
            }
        } elsif ($command_name eq "sort") {
            # SPECIAL IMPL FOR sort
            my $new_node = {};
            $node->{"command_name"} = "sort-impl";
            my $node_options = $node->{"options"};
            $node->{"options"} = {};

            my $cols = $node_options->{"--cols"};
            my $col = $node_options->{"--col"};
            if (defined($cols)) {
                $node->{"options"}->{"--col"} = [];
                foreach my $c (split(/,/, $cols, -1)) {
                    push(@{$node->{"options"}->{"--col"}}, ":$c");
                }
            } else {
                $node->{"options"}->{"--col"} = [];
            }
            if (defined($col)) {
                my $flag = "";
                if (defined($node_options->{"--number"})) {
                    delete($node_options->{"--number"});
                    $flag .= "n";
                }
                if (defined($node_options->{"--reverse"})) {
                    delete($node_options->{"--reverse"});
                    $flag .= "r";
                }
                # sort の --cols には特別に配列を入れる
                push(@{$node->{"options"}->{"--col"}}, "$flag:$col");
            }
        } elsif ($command_name eq "write-file" && defined($node->{"options"}->{"--terminal"})) {
            # SPECIAL IMPL FOR write-file
            my $format = $node->{"connections"}->{"input"}->[2];
            if ($format->[0] eq "tsv") {
                # ターミナルへのテーブル形式の出力
                $node->{"options"}->{"--tsv"} = "";
            } elsif ($format->[0] eq "csv") {
                # CSV形式はそのまま表示
                $node->{"options"}->{"--text"} = "";
            } elsif ($format->[0] eq "json") {
                $node->{"options"}->{"--json"} = "";
            } elsif ($format->[0] eq "string") {
                $node->{"options"}->{"--string"} = "";
            } elsif ($format->[0] eq "textsimple") {
                $node->{"options"}->{"--textsimple"} = "";
            } else {
                $node->{"options"}->{"--text"} = "";
            }
        }
    }
}

################################################################################

sub unifyRange {
    my ($start1, $end1, $start2, $end2) = @_;
    my $start = $start1 + $start2;
    my $end;
    if ($end1 < 0) {
        if ($end2 < 0) {
            $end = -1;
        } else {
            $end = $start1 + $end2;
        }
    } else {
        if ($end2 < 0) {
            $end = $end1;
        } else {
            $end = $start1 + $end2;
            if ($end > $end1) {
                $end = $end1;
            }
        }
    }
    if ($end <= $start) {
        $start = 0;
        $end = 0;
    }
    return ($start, $end);
}

# 各ノード間単の関係を見て書き換え
sub walkPhase4 {
    my ($graph) = @_;
    my $nodes = $graph->{"nodes"};
    for (my $i = 0; $i < @$nodes; $i++) {
        my $node1 = $nodes->[$i];
        next if (!defined($node1->{"connections"}->{"output"}));
        my $node2 = $node1->{"connections"}->{"output"}->[0];
        my $command_name = $node1->{"command_name"};
        if ($command_name eq "range") {
            if ($node2->{"command_name"} eq "range") {
                # SPECIAL IMPL FOR head, offset
                my ($start, $end) = unifyRange(
                    $node1->{"options"}->{"--start"}, $node1->{"options"}->{"--end"},
                    $node2->{"options"}->{"--start"}, $node2->{"options"}->{"--end"});
                $node1->{"options"}->{"--start"} = $start;
                $node1->{"options"}->{"--end"} = $end;
                $node1->{"connections"}->{"output"} = $node2->{"connections"}->{"output"};
                $node1->{"connections"}->{"output"}->[0]->{"connections"}->{"input"} = [$node1, "output", ["tsv", "lf"]];
                $nodes = removeNode($nodes, $i + 1);
                $graph->{"nodes"} = $nodes;
                $i--;
            } elsif ($node2->{"command_name"} eq "write-file" &&
                     defined($node2->{"options"}->{"--terminal"}) &&
                     !defined($node2->{"options"}->{"--record-number-start"})) {
                # SPECIAL IMPL FOR head, offset
                $node2->{"options"}->{"--record-number-start"} = $node1->{"options"}->{"--start"} + 1;
            }
        } elsif ($command_name eq "col-impl" || $command_name eq "sort-impl") {
            if ($node2->{"command_name"} eq $command_name) {
                # SPECIAL IMPL FOR col, sort
                push(@{$node1->{"options"}->{"--col"}}, @{$node2->{"options"}->{"--col"}});
                $node1->{"connections"}->{"output"} = $node2->{"connections"}->{"output"};
                $node1->{"connections"}->{"output"}->[0]->{"connections"}->{"input"} = [$node1, "output", ["tsv", "lf"]];
                $nodes = removeNode($nodes, $i + 1);
                $graph->{"nodes"} = $nodes;
                $i--;
            }
        }
    }
}

################################################################################

my $fifoCount = 0;

sub fetchFifoIdx {
    $fifoCount++;
    return $fifoCount;
}

sub searchNodeFromNodes {
    my ($nodes, $node) = @_;
    for (my $i = 0; $i < @$nodes; $i++) {
        if ($nodes->[$i] eq $node) {
            return $i;
        }
    }
}

sub connectFifo {
    my ($graph) = @_;
    my $nodes = $graph->{"nodes"};

    for (my $i = 0; $i < @$nodes; $i++) {
        my $node = $nodes->[$i];
        $node->{"fifos"} = {};
    }

    for (my $i = 0; $i < @$nodes; $i++) {
        my $node = $nodes->[$i];
        my $command_name = $node->{"command_name"};
        foreach my $key (sort keys %{$node->{"connections"}}) {
            if (defined($node->{"fifos"}->{$key})) {
                next;
            }
            my $other = $node->{"connections"}->{$key};
            my $otherIdx = searchNodeFromNodes($nodes, $other->[0]);
            my $fifoIdx = fetchFifoIdx();
            mkfifo(fifoPathRaw($fifoIdx), 0600) or die $!;
            $node->{"fifos"}->{$key} = $fifoIdx;
            $other->[0]->{"fifos"}->{$other->[1]} = $fifoIdx;
        }
    }
}

################################################################################

sub escape_for_bash {
    my ($str) = @_;
    if ($str =~ /\A[-_.=\/0-9a-zA-Z]+\z/) {
        return $str;
    }
    $str =~ s/'/'"'"'/g;
    return "'" . $str . "'";
}

sub buildNodeCommandParametersForBash {
    my ($node) = @_;
    my $command_name = $node->{"command_name"};
    my $coi = $command_options{$command_name};

    my @args = ();

    if ($command_name eq "read-file") {
        push(@args, "cat");
        if ($node->{"internal"}->{"mode"} eq "pipe") {
            my $fifoIdx = $node->{"internal"}->{"fifo"};
            push(@args, [formatWrapperDataPathBash($fifoIdx)]);
        } else {
            my $path = $node->{"options"}->{"-i"};
            push(@args, $path);
        }
        return \@args;
    }

    push(@args, "bash");
    push(@args, ["\$XSVUTILS_HOME/src/" . $command_name . ".cmd.sh"]);

    foreach my $key (sort keys %{$node->{"options"}}) {
        my $param = $node->{"options"}->{$key};
        if (!defined($coi->{"options"}->{$key})) {
            # nothing
        } elsif ($coi->{"options"}->{$key} eq "") {
            push(@args, $key);
        } else {
            my $type = ref $param;
            if ($type eq "HASH") {
                my $fifoIdx = $node->{"fifos"}->{$key};
                push(@args, $key);
                push(@args, [fifoPathBash($fifoIdx)]);
            } elsif ($type eq "ARRAY") {
                foreach my $p (@$param) {
                    push(@args, $key);
                    push(@args, $p);
                }
            } else {
                push(@args, $key);
                push(@args, $param);
            }
        }
    }

    return \@args;
}

sub buildCommandParametersForBash {
    my ($node) = @_;
    my $command_name = $node->{"command_name"};
    my $coi = $command_options{$command_name};

    my $args = buildNodeCommandParametersForBash($node);
    if (defined($coi->{"code"})) {
        # この時点で @$args は ("bash", "???.cmd.sh", ...) というようなリストが入っている
        $args = $coi->{"code"}->($node, [(@$args)[2..(@$args - 1)]]);
    }

    my $env = {};
    if (defined($coi->{"env"})) {
        $env = $coi->{"env"}->($node);
    }

    my @args2 = ();
    foreach my $k (sort keys %$env) {
        my $v = $env->{$k};
        push(@args2, "$k=" . escape_for_bash($v));
    }
    foreach my $a (@$args) {
        if ((ref $a) eq "ARRAY") {
            push(@args2, $a->[0]);
        } else {
            push(@args2, escape_for_bash($a));
        }
    }

    return join(" ", @args2);
}

sub fifoPathRaw {
    my ($fifoIdx) = @_;
    return "$ENV{WORKING_DIR}/fifo-$fifoIdx";
}

sub fifoPathBash {
    my ($fifoIdx) = @_;
    return "\$WORKING_DIR/fifo-$fifoIdx";
}

sub formatWrapperFormatPathRaw {
    my ($fifoIdx) = @_;
    return "$ENV{WORKING_DIR}/fifo-$fifoIdx-f";
}

sub formatWrapperDataPathRaw {
    my ($fifoIdx) = @_;
    return "$ENV{WORKING_DIR}/fifo-$fifoIdx-d";
}

sub formatWrapperDataPathBash {
    my ($fifoIdx) = @_;
    return "\$WORKING_DIR/fifo-$fifoIdx-d";
}

sub formatToString {
    my ($format) = @_;
    return join("/", @$format);
}

# 実行するBashスクリプト(--explainで表示する内容)を生成
sub buildNodeCode {
    my ($graph) = @_;
    my $nodes = $graph->{"nodes"};

    my $code = "";

    for (my $i = 0; $i < @$nodes; $i++) {
        my $node = $nodes->[$i];
        my $command_name = $node->{"command_name"};

        $code .= "# NODE[$i]:$command_name\n";

        my $stdinStr = "";
        if (defined($node->{"fifos"}->{"input"})) {
            my $fifoIdx = $node->{"fifos"}->{"input"};
            $stdinStr = " < " . fifoPathBash($fifoIdx);
        }

        my $stdoutStr = "";
        if (defined($node->{"fifos"}->{"output"})) {
            my $fifoIdx = $node->{"fifos"}->{"output"};
            $stdoutStr = " > " . fifoPathBash($fifoIdx);
        }

        $code .= buildCommandParametersForBash($node) . "$stdinStr$stdoutStr &\n";

        # internalの利用はいまのところ read-file, write-file のみ
        if ($command_name eq "read-file") {
            $code .= "    # input:  " . $node->{"internal"}->{"input"} . "\n";
            $code .= "    # format: " . $node->{"internal"}->{"format-result"} . "\n";
        } elsif ($command_name eq "write-file") {
            $code .= "    # format: " . $node->{"internal"}->{"format"} . "\n";
        }

        foreach my $key (sort keys %{$node->{"connections"}}) {
            my $other = $node->{"connections"}->{$key};
            my $otherIdx = searchNodeFromNodes($nodes, $other->[0]);
            my $format = formatToString($other->[2]);
            if ($otherIdx < $i) {
                my $c = $nodes->[$otherIdx]->{"command_name"};
                $code .= "    # $key < NODE[$otherIdx]:$c/$other->[1] $format\n";
            }
        }
        foreach my $key (sort keys %{$node->{"connections"}}) {
            my $other = $node->{"connections"}->{$key};
            my $otherIdx = searchNodeFromNodes($nodes, $other->[0]);
            my $format = formatToString($other->[2]);
            if ($otherIdx > $i) {
                my $c = $nodes->[$otherIdx]->{"command_name"};
                $code .= "    # $key > NODE[$otherIdx]:$c/$other->[1] $format\n";
            }
        }

        $code .= "\n";
    }

    return $code;
}

################################################################################

# 実行するBashスクリプト(--explainで表示する内容)を生成
sub createSourceFile {
    my ($graph, $working_dir) = @_;
    connectFifo($graph);
    my $code1 = "";
    my $code2 = "";
    $code1 .= buildNodeCode($graph);
    $code2 .= "wait\n";
    $code2 .= "rm -rf \$WORKING_DIR\n";
    my $filepath = $working_dir . "/script.sh";
    open(my $fh, ">", $filepath) or die $!;
    print $fh $code1;
    print $fh $code2;
    close($fh);
    return ($filepath, $code1);
}

################################################################################

my $action = shift(@ARGV);
if ($action eq "complete-zsh") {
    pop(@ARGV);
}

if (@ARGV && $ARGV[0] =~ /\A-v(o|[0-9])+/) {
    if ($action ne "execute") {
        exit;
    }
    if ($ARGV[0] eq "-vo") {
        shift(@ARGV);
    }
    my $xsvutils1_path = $ENV{"XSVUTILS_HOME"} . "/src/run-xsvutils1.sh";
    exec("bash", $xsvutils1_path, @ARGV);
}

my @argv = @ARGV;
my ($graph, $tail_argv, $completion, $error) = parseQuery(\@ARGV, "", $true, $false, "may", "may");

if ($action eq "complete-zsh") {
    #if (defined($completion)) {
    #}
    print "_files\n";
    print "local -a params\n";
    print "params=(-a -b)\n";
    print "_describe -t params parameter params\n";
    print "local -a cmds\n";
    print "cmds=(cut head)\n";
    print "_describe -t cmds command cmds\n";
    #print "COMPLETION\n";
    if (defined($completion)) {
        #print STDERR Dumper($completion);
    } elsif (defined($error)) {
        die "$error\n";
    }
    exit(0);
}

if ($action ne "execute") {
    die;
}

if (defined($error)) {
    die "$error\n";
}
if (!defined($graph)) {
    die;
}

if ($global_option_version) {
    exec("bash", $ENV{"XSVUTILS_HOME"} . "/src/version.sh");
}

my $working_dir = $ENV{"XSVUTILS_HOME"} . "/var/working_dir/$$";
mkpath($working_dir);
$ENV{"WORKING_DIR"} = $working_dir;

connectStdin($graph, $isInputTty);
connectStdout($graph, $isOutputTty);
executeFormatWrapper($graph);
walkPhase0($graph);
walkPhase1($graph);
walkPhase2($graph);
walkPhase3($graph);
walkPhase4($graph);

################################################################################

my ($source_filepath, $code) = createSourceFile($graph, $working_dir);
if ($is_explain) {
    print STDERR $code;
}

# TODO 必要なランタイムのチェックは explain のあとの、ここのタイミングでしたい

exec("bash", $source_filepath);

################################################################################

