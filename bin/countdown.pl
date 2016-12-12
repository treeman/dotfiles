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

my @countdown = (["2016-12-16", "Rogue One"],
                 ["2016-12-22", "Jullov"]);

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

    if ($days > 0) {
        say "\${color2}\${font Comfortaa:size=14}$days\${font}\${color1} days to $what";
    }
}

