use strict;
use warnings;
use utf8;

use File::Path qw/mkpath/;
use Data::Dumper;

our $true = 1;
our $false = "";

require "$ENV{XSVUTILS_HOME}/src/commands-info.pl";
our %command_options;

my @help_files = qw/
    list
    main
    options
    usage
/;
# 新バージョンに対応していない旧バージョン(-vo)のヘルプファイルをいったん削除するのは嫌なので、
# 当面の間は、ヘルプファイルの一覧を上記配列で管理することにする

################################################################################

foreach my $command_name (keys %command_options) {
    my $coi = $command_options{$command_name};
    if (!defined($coi->{"exists_help"})) {
        $coi->{"exists_help"} = $false;
    }
    if (!defined($coi->{"is_internal"})) {
        $coi->{"is_internal"} = $false;
    }
    if (!defined($coi->{"options"})) {
        $coi->{"options"} = {};
    }
    if (!defined($coi->{"parameters"})) {
        $coi->{"parameters"} = [];
    }
    if (!defined($coi->{"input"})) {
        $coi->{"input"} = "tsv";
    }
    if (!defined($coi->{"output"})) {
        $coi->{"output"} = "tsv";
    }
}

################################################################################

my $isInputTty = $false;
if (-t STDIN) {
    $isInputTty = $true;
}
my $isOutputTty = $false;
if (-t STDOUT) {
    $isOutputTty = $true;
}

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
################################################################################

my $is_explain = $false;

################################################################################

sub parseQuery {
    my ($argv, $subqueryCommandName, $is_global, $is_strict_mode, $inputMode, $outputMode) = @_;
    # $inputMode="must"
    # $inputMode="may"
    # $inputMode="mustNot"
    # $outputMode="must"
    # $outputMode="may"
    # $outputMode="mustNot"

    # 2値を返す関数。
    # 1つ目の返り値の例
    # 2つ目は閉じ括弧よりも後ろの残ったパラメータの配列。

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
            if ($a eq "--strict") {
                unless ($is_strict_mode) {
                    return (parseQuery(\@argv_orig, $subqueryCommandName, $is_global, $true, $inputMode, $outputMode));
                }
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

            if ($a eq "--help") {
                my $helpArg = undef;
                $helpArg = $argv->[0] if (@$argv);
                parseAndExecHelpQuery($curr_node, $helpArg);
            }

            if ($a =~ /\A-/) {
                return (undef, undef, undef, "Unknown option: $a");
            }
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
                    if (-f $a) {
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
        my $input_node2 = {
            "command_name" => "read-file",
            "parameters" => [],
            "options" => {
                "-i" => $input_filepath,
            },
            "connections" => {},
        };
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
        my $output_node2 = {
            "command_name" => "write-file",
            "parameters" => [],
            "options" => {
                "-o" => $output_filepath,
            },
            "connections" => {},
        };
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

    my $input_node_input_policy = $command_options{$input_node->{"command_name"}}->{"input"};
    if ($inputMode eq "must" && $input_node_input_policy ne "deny") {
        return (undef, undef, undef, "sub query of '$subqueryCommandName' must have input");
    }
    if ($inputMode eq "mustNot" && $input_node_input_policy eq "deny") {
        return (undef, undef, undef, "sub query of '$subqueryCommandName' must not have input");
    }

    my $output_node_output_policy = $command_options{$output_node->{"command_name"}}->{"output"};
    if ($outputMode eq "must" && $output_node_output_policy ne "deny") {
        return (undef, undef, undef, "sub query of '$subqueryCommandName' must have output");
    }
    if ($outputMode eq "mustNot" && $output_node_output_policy eq "deny") {
        return (undef, undef, undef, "sub query of '$subqueryCommandName' must not have output");
    }

    my $graph = {
        "input" => $input_node,
        "output" => $output_node,
        "nodes" => \@nodes,
    };
    return ($graph, $argv, $completion, undef);
}

################################################################################

sub connectStdin {
    my ($graph, $isInputTty) = @_;
    my $input_node = $graph->{"input"};
    my $command_name = $input_node->{"command_name"};
    my $coi = $command_options{$command_name};
    if ($coi->{"input"} eq "deny") {
        return;
    }
    if ($isInputTty) {
        die "Input not found";
    }
    my $input_node2 = {
        "command_name" => "read-file",
        "parameters" => [],
        "options" => {
            "--stdin" => undef,
        },
        "connections" => {
            "output" => [$input_node, "input"],
        },
    };
    $input_node->{"connections"}->{"input"} = [$input_node2, "output"];
    $graph->{"input"} = $input_node2;
    unshift(@{$graph->{"nodes"}}, $input_node2);
}

################################################################################

sub connectStdout {
    my ($graph, $isOutputTty) = @_;
    my $output_node = $graph->{"output"};
    my $command_name = $output_node->{"command_name"};
    my $coi = $command_options{$command_name};
    if ($coi->{"output"} eq "deny") {
        return;
    }
    my $output_node2;
    if (!$isOutputTty) {
        # ターミナル以外の標準出力
        $output_node2 = {
            "command_name" => "write-file",
            "options" => {},
            "connections" => {
                "input" => [$output_node, "output"],
            },
        };
    } else {
        if ($coi->{"output"} eq "tsv") {
            # ターミナルへのテーブル形式の出力
            $output_node2 = {
                "command_name" => "write-terminal-tsv",
                "options" => {},
                "connections" => {
                    "input" => [$output_node, "output"],
                },
            };
        } else {
            # ターミナルへのテキストの出力
            $output_node2 = {
                "command_name" => "write-terminal-text",
                "options" => {},
                "connections" => {
                    "input" => [$output_node, "output"],
                },
            };
        }
    }
    $output_node->{"connections"}->{"output"} = [$output_node2, "input"];
    $graph->{"output"} = $output_node2;
    push(@{$graph->{"nodes"}}, $output_node2);
}

################################################################################

sub walkPhase1 {
    my ($graph) = @_;
    my $nodes = $graph->{"nodes"};
    for (my $i = 0; $i < @$nodes; $i++) {
        my $node = $nodes->[$i];
        my $command_name = $node->{"command_name"};
        my $coi = $command_options{$command_name};
        if ($coi->{"input"} eq "deny") {
            if (defined($node->{"connections"}->{"input"})) {
                die "`$command_name` subcommand must not have input.";
            }
        }
        if ($coi->{"output"} eq "deny") {
            if (defined($node->{"connections"}->{"output"})) {
                die "`$command_name` subcommand must not have output.";
            }
        }
        if ($coi->{"output"} eq "text") {
            my $output_node = $node->{"connections"}->{"output"}->[0];
            my $output_command_name = $output_node->{"command_name"};
            my $output_coi = $command_options{$output_command_name};
            if ($output_coi->{"input"} eq "tsv") {
                die "Cannot pipe `$command_name` subcommand to `$output_command_name` subcommand.";
            }
        }
    }
}

################################################################################

sub walkPhase2 {
    my ($graph) = @_;
    my $nodes = $graph->{"nodes"};
    for (my $i = 0; $i < @$nodes; $i++) {
        my $node = $nodes->[$i];
        my $command_name = $node->{"command_name"};
        if ($command_name eq "cut") {
            # SPECIAL IMPL FOR cut, cols
            $node->{"command_name"} = "cols";
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
                    push(@{$node->{"options"}->{"--col"}}, ":$c");
                }
            } else {
                $node->{"options"}->{"--col"} = [];
            }
            if (defined($col)) {
                push(@{$node->{"options"}->{"--col"}}, $col);
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
                # sort の --cols には特別に配列を入れる
                push(@{$node->{"options"}->{"--col"}}, "$flag:$col");
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

sub walkPhase3 {
    my ($graph) = @_;
    my $nodes = $graph->{"nodes"};
    while () {
        my $f = $false;
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
                    $node1->{"connections"}->{"output"}->[0]->{"connections"}->{"input"} = [$node1, "output"];
                    $nodes = [@$nodes[0..$i], @$nodes[($i+2)..(@$nodes-1)]];
                    $graph->{"nodes"} = $nodes;

                    $f = $true;
                    last;
                } elsif ($node2->{"command_name"} eq "write-terminal-tsv" && !defined($node2->{"options"}->{"--record-number-start"})) {
                    # SPECIAL IMPL FOR head, offset
                    $node2->{"options"}->{"--record-number-start"} = $node1->{"options"}->{"--start"} + 1;

                    $f = $true;
                    last;
                }
            } elsif ($command_name eq "col-impl" || $command_name eq "sort-impl") {
                if ($node2->{"command_name"} eq $command_name) {
                    # SPECIAL IMPL FOR col, sort
                    push(@{$node1->{"options"}->{"--col"}}, @{$node2->{"options"}->{"--col"}});

                    $node1->{"connections"}->{"output"} = $node2->{"connections"}->{"output"};
                    $node1->{"connections"}->{"output"}->[0]->{"connections"}->{"input"} = [$node1, "output"];
                    $nodes = [@$nodes[0..$i], @$nodes[($i+2)..(@$nodes-1)]];
                    $graph->{"nodes"} = $nodes;

                    $f = $true;
                    last;
                }
            }
        }
        last if (!$f);
    }
}

################################################################################

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

    my $fifoIdx = 0;
    for (my $i = 0; $i < @$nodes; $i++) {
        my $node = $nodes->[$i];
        my $command_name = $node->{"command_name"};
        foreach my $key (sort keys %{$node->{"connections"}}) {
            if (defined($node->{"fifos"}->{$key})) {
                next;
            }
            my $other = $node->{"connections"}->{$key};
            my $otherIdx = searchNodeFromNodes($nodes, $other->[0]);
            $fifoIdx++;
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

sub pad_len {
    my ($str) = @_;
    my $l = length($str);
    if ($l < 100) {
        $str .= " " x (100 - $l);
    }
    return $str;
}

sub buildCommandParametersForBash {
    my ($cmd, $node) = @_;
    my $str = "";
    my $command_name = $node->{"command_name"};
    foreach my $key (sort keys %{$node->{"options"}}) {
        my $param = $node->{"options"}->{$key};
        if (!defined($command_options{$command_name}->{"options"}->{$key})) {
            print STDERR "DEBUG $command_name $key\n";
        }
        if ($command_options{$command_name}->{"options"}->{$key} eq "") {
            $str .= " " . escape_for_bash($key);
        } else {
            my $type = ref $param;
            if ($type eq "HASH") {
                my $fifoIdx = $node->{"fifos"}->{$key};
                $str .= " " . escape_for_bash($key) . " \$WORKING_DIR/fifo-$fifoIdx";
            } elsif ($type eq "ARRAY") {
                foreach my $p (@$param) {
                    $str .= " " . escape_for_bash($key) . " " . escape_for_bash($p);
                }
            } else {
                $str .= " " . escape_for_bash($key) . " " . escape_for_bash($param);
            }
        }
    }
    return $cmd . $str;
}

sub dumpNodes {
    my ($graph) = @_;
    my $nodes = $graph->{"nodes"};
    my $code = "";
    for (my $i = 0; $i < @$nodes; $i++) {
        my $node = $nodes->[$i];
        my $command_name = $node->{"command_name"};
        $code .= "# NODE[$i] $command_name\n";
        my $cmds = buildCommandParametersForBash($node->{"command_name"}, $node);
        $cmds =~ s/\n/\n# /g;
        $code .= "#   " . $cmds . "\n";
        foreach my $key (sort keys %{$node->{"connections"}}) {
            my $other = $node->{"connections"}->{$key};
            my $otherIdx = searchNodeFromNodes($nodes, $other->[0]);
            if ($otherIdx < $i) {
                $code .= "#   # $key < NODE[$otherIdx]/$other->[1]\n";
            }
        }
        foreach my $key (sort keys %{$node->{"connections"}}) {
            my $other = $node->{"connections"}->{$key};
            my $otherIdx = searchNodeFromNodes($nodes, $other->[0]);
            my $ar;
            if ($otherIdx > $i) {
                $code .= "#   # $key > NODE[$otherIdx]/$other->[1]\n";
            }
        }
    }
    return $code;
}

sub buildMkfifoCode {
    my ($graph) = @_;
    my $nodes = $graph->{"nodes"};

    my $code = "";

    my $fifoIdx = 0;
    for (my $i = 0; $i < @$nodes; $i++) {
        my $node = $nodes->[$i];
        my $command_name = $node->{"command_name"};
        foreach my $key (sort keys %{$node->{"connections"}}) {
            my $other = $node->{"connections"}->{$key};
            my $otherIdx = searchNodeFromNodes($nodes, $other->[0]);
            if ($otherIdx < $i) {
                next;
            }
            my $fifoIdx = $node->{"fifos"}->{$key};
            $code .= pad_len("mkfifo \$WORKING_DIR/fifo-$fifoIdx") . " # NODE[$i] -> NODE[$otherIdx]\n";
        }
    }

    return $code;
}

sub buildNodeCode {
    my ($graph) = @_;
    my $nodes = $graph->{"nodes"};

    my $code = "";

    for (my $i = 0; $i < @$nodes; $i++) {
        my $node = $nodes->[$i];
        my $command_name = $node->{"command_name"};
        my $stdinStr = "";
        if (defined($node->{"fifos"}->{"input"})) {
            my $fifoIdx = $node->{"fifos"}->{"input"};
            $stdinStr = " < \$WORKING_DIR/fifo-$fifoIdx"
        } elsif ($command_name eq "read-file" && exists($node->{"options"}->{"--stdin"})) {
            $stdinStr = " < /dev/stdin";
        }
        my $stdoutStr = "";
        if (defined($node->{"fifos"}->{"output"})) {
            my $fifoIdx = $node->{"fifos"}->{"output"};
            $stdoutStr = " > \$WORKING_DIR/fifo-$fifoIdx"
        } elsif ($command_name eq "write-file" && exists($node->{"options"}->{"--stdout"})) {
            $stdoutStr = " > /dev/stdout";
        }
        my $cmd = "bash \$XSVUTILS_HOME/src/" . $node->{"command_name"} . ".cmd.sh";

        my $comment = "NODE[$i]";
        foreach my $key (sort keys %{$node->{"connections"}}) {
            my $other = $node->{"connections"}->{$key};
            my $otherIdx = searchNodeFromNodes($nodes, $other->[0]);
            if ($otherIdx < $i) {
                $comment .= " $key<NODE[$otherIdx]/$other->[1]";
            }
        }
        foreach my $key (sort keys %{$node->{"connections"}}) {
            my $other = $node->{"connections"}->{$key};
            my $otherIdx = searchNodeFromNodes($nodes, $other->[0]);
            my $ar;
            if ($otherIdx > $i) {
                $comment .= " $key>NODE[$otherIdx]/$other->[1]";
            }
        }

        $code .= pad_len(buildCommandParametersForBash($cmd, $node) . "$stdinStr$stdoutStr &") . " # $comment\n";
    }

    return $code;
}

################################################################################

sub createSourceFile {
    my ($graph, $working_dir) = @_;
    connectFifo($graph);
    my $code = "";
    $code .= dumpNodes($graph);
    $code .= buildMkfifoCode($graph);
    $code .= buildNodeCode($graph);
    $code .= "wait\n";
    $code .= "rm -rf \$WORKING_DIR\n";
    my $filepath = $working_dir . "/script.sh";
    open(my $fh, ">", $filepath) or die $!;
    print $fh $code;
    close($fh);
    if ($is_explain) {
        print STDERR $code;
    }
    return $filepath;
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
if (defined($graph)) {
    connectStdin($graph, $isInputTty);
    connectStdout($graph, $isOutputTty);
    walkPhase1($graph);
    walkPhase2($graph);
    walkPhase3($graph);
}

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

################################################################################

my $working_dir = $ENV{"XSVUTILS_HOME"} . "/var/working_dir/$$";
mkpath($working_dir);
$ENV{"WORKING_DIR"} = $working_dir;

my $source_filepath = createSourceFile($graph, $working_dir);

exec("bash", $source_filepath);

################################################################################
