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
    "from-psql" => {
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
            my $cmds = ["psql", "-X", "-A", "-F", ["\$'\\t'"], "--pset=footer=off"];
            if (defined($node->{"options"}->{"-h"})) {
                push(@$cmds, "-h", $node->{"options"}->{"-h"});
            }
            if (defined($node->{"options"}->{"-p"})) {
                push(@$cmds, "-p", $node->{"options"}->{"-p"});
            }
            if (defined($node->{"options"}->{"-U"})) {
                push(@$cmds, "-U", $node->{"options"}->{"-U"});
            }
            if (defined($node->{"options"}->{"-d"})) {
                push(@$cmds, "-d", $node->{"options"}->{"-d"});
            }
            my $query;
            if (defined($node->{"options"}->{"-c"})) {
                $query = "(" . $node->{"options"}->{"-c"} . ")";
            } elsif (defined($node->{"options"}->{"-t"})) {
                $query = "\"" . $node->{"options"}->{"-t"} . "\""; # TODO エスケープ処理 https://www.postgresql.jp/document/11/html/sql-syntax-lexical.html
            } else {
                $query = "(SELECT " .
                           "n.nspname as \"Schema\", " .
                           "c.relname as \"Name\", " .
                           "CASE c.relkind WHEN 'r' THEN 'table' WHEN 'v' THEN 'view' WHEN 'm' THEN 'materialized view' WHEN 'i' THEN 'index' WHEN 'S' THEN 'sequence' WHEN 's' THEN 'special' WHEN 'f' THEN 'foreign table' WHEN 'p' THEN 'table' WHEN 'I' THEN 'index' END as \"Type\", " .
                           "pg_catalog.pg_get_userbyid(c.relowner) as \"Owner\" " .
                         "FROM pg_catalog.pg_class c LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace " .
                         "WHERE c.relkind IN ('r','p','v','m','S','f','') AND n.nspname <> 'pg_catalog' AND n.nspname <> 'information_schema' AND n.nspname !~ '^pg_toast' AND pg_catalog.pg_table_is_visible(c.oid) " .
                         "ORDER BY 1,2)";
            }
            $query = "COPY $query TO STDOUT (delimiter '\t', FORMAT CSV, HEADER TRUE);";
            push(@$cmds, "-c", $query);
            $cmds;
        },
        "env" => sub {
            my ($node) = @_;
            my $env = {};
            if (defined($node->{"options"}->{"-P"})) {
                $env->{"PGPASSWORD"} = $node->{"options"}->{"-P"};
            }
            $env;
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
