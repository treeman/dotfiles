#!/usr/bin/perl -w

use utf8;
use locale;

use Modern::Perl;
use DateTime;
use DateTime::Format::Strptime;

my $date_parser = DateTime::Format::Strptime->new(
  pattern => '%Y-%m-%d',
  on_error => 'croak',
);

my @countdown = (["2016-06-12", "Dan Gradering"],
                 ["2016-05-14", "ANRPC Finals"],
                 ["2016-05-06", "Lincon Regionals"],
                 ["2016-05-21", "Sa-Ja Gradering"],
                 ["2016-04-29", "Diablo Season 6"],
                 ["2016-07-07", "Prag"]);

@countdown = sort { @$a[0] cmp @$b[0] } @countdown;

my $today = DateTime->today;
#say "start: ", $today->date;

for my $item (@countdown) {
    my ($date, $what) = @$item;
    my $dt = $date_parser->parse_datetime($date);
    #say "target: ", $dt->date;

    my @start = $today->utc_rd_values;
    my @finish = $dt->utc_rd_values;
    my $days = $finish[0] - $start[0];
    #say $days;

    say "\${color2}\${font Comfortaa:size=14}$days\${font}\${color1} days to $what";
}

