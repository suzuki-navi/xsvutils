use strict;
use warnings;
use utf8;

my @args = ();
my $passwd = undef;
my $table = undef;
my $query = undef;

while (@ARGV) {
    my $a = shift(@ARGV);
    if ($a eq "-h") {
        push(@args, $a, shift(@ARGV));
    } elsif ($a eq "-p") {
        push(@args, $a, shift(@ARGV));
    } elsif ($a eq "-U") {
        push(@args, $a, shift(@ARGV));
    } elsif ($a eq "-P") {
        $passwd = shift(@ARGV);
    } elsif ($a eq "-d") {
        push(@args, $a, shift(@ARGV));
    } elsif ($a eq "-t") {
        $table = shift(@ARGV);
    } elsif ($a eq "-c") {
        $query = shift(@ARGV);
    } else {
        die "Unknown argument: $a";
    }
}

unshift(@args, "psql", "-X");

if (defined($passwd)) {
    $ENV{"PGPASSWORD"} = $passwd;
}

if (defined($query)) {
    $query = "(" . $query . ")";
} elsif (defined($table)) {
    $query = "\"" . $table . "\"";
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
$query = "COPY $query TO STDOUT (DELIMITER '\t', FORMAT CSV, HEADER TRUE);";
push(@args, "-c", $query);

exec(@args);

