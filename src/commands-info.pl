use strict;
use warnings;
use utf8;

our $true;
our $false;

################################################################################

our %command_options = (
    # なにもしないサブコマンド
    "cat" => {
        "exists_help" => $true,
        "input" => "any",
        "output" => sub {
            # outputが関数の場合はdenyを返してはいけない
            $_[0]->{"connections"}->{"input"}->[2];
        },
        "code" => sub {
            my ($node, $args) = @_;
            ["cat", @$args];
        },
    },

    # レコード選択に関するサブコマンド
    "head" => {
        "exists_help" => $true,
        "options" => {
            "-n" => "LINE_COUNT",
        },
        "parameters" => [
                "-n",
        ],
    },
    "limit" => {
        "exists_help" => $true,
        "options" => {
            "-n" => "LINE_COUNT",
        },
        "parameters" => [
                "-n",
        ],
    },
    "offset" => {
        "exists_help" => $true,
        "options" => {
            "-n" => "LINE_COUNT",
        },
        "parameters" => [
                "-n",
        ],
    },
    "offset-random" => {
        "options" => {
            "-n" => "LINE_COUNT",
        },
        "parameters" => [
                "-n",
        ],
    },
    "range" => {
        "is_internal" => $true,
        "options" => {
            "--start" => "LINE_COUNT",
            "--end" => "LINE_COUNT",
        },
        "code" => sub {
            my ($node, $args) = @_;
            my $start = $node->{"options"}->{"--start"};
            my $end = $node->{"options"}->{"--end"};
            # rangeはこの2つのオプションが必ず設定されている
            $start += 2;
            $end += 1;
            if ($end == 0) {
                $end = '$';
            }
            if ($start == 2) {
                if ($end eq '$') {
                    ["cat"];
                } else {
                    ["head", "-n", $end];
                }
            } else {
                ["sed", "-n", "-e", "1p", "-e", "${start},${end}p"];
            }
        },
    },
    "where" => {
        "options" => {
            "--col" => "COLUMN",
            "--op" => "OPERATOR",
            "--val" => "VALUE",
        },
        "parameters" => [
            "--col",
            "--op",
            "--val",
        ],
    },
    "filter" => {
        "options" => {
            "--col" => "COLUMN",
            "--op" => "OPERATOR",
            "--val" => "VALUE",
        },
        "parameters" => [
            "--col",
            "--op",
            "--val",
        ],
    },
    "filter-record" => {
        "options" => {
            "--record" => "PERL_CODE",
        },
        "parameters" => [
            "--record",
        ],
        "code" => sub {
            my ($node, $args) = @_;
            my $record = $node->{"options"}->{"--record"};
            $record = "." if !defined($record);
            ["perl", ["\$XSVUTILS_HOME/src/filter-record.pl"], $record];
        },
    },

    # 列の選択に関するサブコマンド
    "cut" => {
        "exists_help" => $true,
        "options" => {
            "--cols" => "COLUMNS",
        },
        "parameters" => [
            "--cols",
        ],
    },
    "cols" => {
        "exists_help" => $true,
        "options" => {
            "--cols" => "COLUMNS",
            "--head" => "COLUMNS",
            "--last" => "COLUMNS",
            "--remove" => "COLUMNS",
            "--left-update" => "",
            "--right-update" => "",
        },
        "parameters" => [
        ],
        "code" => sub {
            my ($node, $args) = @_;
            ["perl", ["\$XSVUTILS_HOME/src/cut.pl"], @$args];
        },
    },
    "col" => {
        "exists_help" => $true,
        "options" => {
            "--cols" => "COLUMNS",
            "--col" => "A:COLUMN",
        },
        "parameters" => [
            "--col",
        ],
    },
    "col-impl" => {
        "is_internal" => $true,
        "options" => {
            "--cols" => "COLUMNS",
            "--col" => "A:COLUMN",
        },
        "code" => sub {
            my ($node, $args) = @_;
            ["perl", ["\$XSVUTILS_HOME/src/cut.pl"], @$args];
        },
    },

    # 列を追加するサブコマンド
    "ins-concat" => {
        "options" => {
            "--col" => "A:COLUMN",
            "--dst" => "COLUMN",
        },
        "code" => sub {
            my ($node, $args) = @_;
            ["perl", ["\$XSVUTILS_HOME/src/ins-concat.pl"], @$args];
        },
    },

    # その他のデータを加工するサブコマンド
    "sort" => {
        "exists_help" => $true,
        "options" => {
            "--cols" => "COLUMNS",
            "--col" => "COLUMN",
            "--number" => "",
            "--reverse" => "",
        },
        "parameters" => [
            "--col",
        ],
    },
    "sort-impl" => {
        "is_internal" => $true,
        "options" => {
            "--col" => "A:COLUMN",
        },
        "code" => sub {
            my ($node, $args) = @_;
            ["perl", ["\$XSVUTILS_HOME/src/sort.pl"], @$args];
        },
    },
    "uniq" => {
        "code" => sub {
            my ($node, $args) = @_;
            ["perl", ["\$XSVUTILS_HOME/src/uniq.pl"], @$args];
        },
    },
    "join" => {
        "exists_help" => $true,
        "options" => {
            "--other" => "FILE",
            "--inner" => "",
            "--left-outer" => "",
            "--right-outer" => "",
            "--full-outer" => "",
            "--number" => "",
        },
        "parameters" => [
            "--other",
        ],
    },
    "trim-values" => {
        "code" => sub {
            my ($node, $args) = @_;
            ["perl", ["\$XSVUTILS_HOME/src/trim-values.pl"], @$args];
        },
    },
    "rename-duplicated-column-name" => {
        "code" => sub {
            my ($node, $args) = @_;
            ["perl", ["\$XSVUTILS_HOME/src/rename-duplicated-column-name.pl"], @$args];
        },
    },
    "modify-record" => {
        "options" => {
            "--header" => "PERL_CODE",
            "--record" => "PERL_CODE",
        },
        "parameters" => [
            "--header",
            "--record",
        ],
        "code" => sub {
            my ($node, $args) = @_;
            my $header = $node->{"options"}->{"--header"};
            my $record = $node->{"options"}->{"--record"};
            $header = "." if !defined($header);
            $record = "." if !defined($record);
            ["perl", ["\$XSVUTILS_HOME/src/modify-record.pl"], $header, $record];
        },
    },
    "jq" => {
        "options" => {
            "-q" => "JQ_CODE",
        },
        "parameters" => [
            "-q",
        ],
        "input" => "json",
        "output" => "json",
        "code" => sub {
            my ($node, $args) = @_;
            my $q = $node->{"options"}->{"-q"};
            $q = "." if !defined($q);
            ["jq", $q];
        },
    },

    # 集計するサブコマンド
    "wcl" => {
        "exists_help" => $true,
        "output" => "string",
    },
    "header" => {
        "options" => {
            "--comma" => "",
            "--col" => "",
        },
        "output" => "string",
        "code" => sub {
            my ($node, $args) = @_;
            ["perl", ["\$XSVUTILS_HOME/src/header.pl"], @$args];
        },
    },
    "meaningful-cols" => {
        "options" => {
            "--comma" => "",
            "--col" => "",
        },
        "output" => "string",
        "code" => sub {
            my ($node, $args) = @_;
            ["perl", ["\$XSVUTILS_HOME/src/meaningful-cols.pl"], @$args];
        },
    },
    "summary" => {
        "exists_help" => $true,
        "output" => "text",
        "code" => sub {
            my ($node, $args) = @_;
            ["perl", ["\$XSVUTILS_HOME/src/summary.pl"], @$args];
        },
    },
    "sum" => {
        "code" => sub {
            my ($node, $args) = @_;
            ["perl", ["\$XSVUTILS_HOME/src/total.pl"], "--sum", @$args];
        },
    },
    "average" => {
        "code" => sub {
            my ($node, $args) = @_;
            ["perl", ["\$XSVUTILS_HOME/src/total.pl"], "--avg", @$args];
        },
    },

    # グラフ化するサブコマンド
    "chart-bar" => {
        "output" => "textsimple",
        "code" => sub {
            my ($node, $args) = @_;
            ["perl", ["\$XSVUTILS_HOME/src/chart-bar.pl"], @$args];
        },
    },

    # 入出力のコマンド
    "read-file" => {
        "is_internal" => $true,
        "options" => {
            "-i" => "FILE",
        },
        "input" => "deny",
    },
    "write-file" => {
        "is_internal" => $true,
        "options" => {
            "-o" => "FILE",
            "--terminal" => "",
            "--tsv" => "",
            "--json" => "",
            "--text" => "",
            "--textsimple" => "",
            "--string" => "",
            "--record-number-start" => "LINE_COUNT",
        },
        "input" => "any",
        "output" => "deny",
    },
    "from-postgresql" => {
        "options" => {
            "-h" => "HOST",
            "-p" => "PORT",
            "-U" => "USER",
            "-P" => "PASSWORD",
            "-d" => "DATABASE",
            "-t" => "TABLE",
            "-c" => "QUERY",
        },
        "input" => "deny",
        "code" => sub {
            my ($node, $args) = @_;
            ["perl", ["\$XSVUTILS_HOME/src/from-postgresql.pl"], @$args];
        },
    },

    # フォーマット変換のサブコマンド
    "from-csv" => {
        "is_internal" => $true,
        "input" => "csv",
        "code" => sub {
            my ($node, $args) = @_;
            ["bash", ["\$XSVUTILS_HOME/src/run-rust.sh"], "fromcsv", @$args];
        },
    },
    "to-csv" => {
        "is_internal" => $true,
        "output" => "csv",
        "code" => sub {
            my ($node, $args) = @_;
            ["perl", ["\$XSVUTILS_HOME/src/to-csv.pl"], @$args];
        },
    },
    "from-json" => {
        "is_internal" => $true,
        "options" => {
            "--col" => "A:COLUMN",
        },
        "input" => "json",
        "code" => sub {
            my ($node, $args) = @_;
            ["perl", ["\$XSVUTILS_HOME/src/from-json.pl"], @$args];
        },
    },
    "no-header" => {
        "is_internal" => $true,
        "input" => "any",
        "output" => "any",
        "code" => sub {
            my ($node, $args) = @_;
            ["tail", "-n+2", @$args];
        },
    },
);

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
        $coi->{"input"} = ["tsv", "lf"];
    } elsif ((ref $coi->{"input"}) eq "") {
        $coi->{"input"} = [$coi->{"input"}, "lf"];
    }
    if (!defined($coi->{"output"})) {
        $coi->{"output"} = ["tsv", "lf"];
    } elsif ((ref $coi->{"output"}) eq "") {
        $coi->{"output"} = [$coi->{"output"}, "lf"];
    }
}

################################################################################

1;
