#!/usr/bin/perl -w

use utf8;
use locale;

use Modern::Perl;
use Getopt::Long;

use DateTime;
use DateTime::Format::Atom;

use Data::ICal;
use Data::ICal::DateTime;

use LWP::Simple;

# Url for ical files
my @urls = qw(
    http://timeedit.liu.se/4DACTION/iCal_downloadReservations/timeedit.ics?from=1133&to=1144&id1=28824002&branch=8&lang=1
);

# Correct times
my $today = DateTime->now()->set( hour => 0, minute => 0, second => 0 );
my $tomorrow = DateTime->now()->set( hour => 0, minute => 0, second => 0 )->add( days => 1 );

# Change day to where we have a few lessons
#$today = DateTime->new( year => 2011, month => 8, day => 29 );
#$tomorrow = DateTime->new( year => 2011, month => 8, day => 30 );

my $span = DateTime::Span->from_datetimes( start => $today, end => $tomorrow );

#my @events = $calendar->events($span);
#my @events = $calendar->events();

my @events;

for my $url (@urls) {
    my $str = LWP::Simple::get $url;
    my $calendar = Data::ICal->new(data => $str);

    @events = (@events, $calendar->events($span));
}

my %my_courses = map { $_ => 1; } qw(
    TDDC69
    TDDC70
    TATA24
);

sub shorten_summary
{
    my ($summary) = @_;

    my @stuff = split(/,\s*/, $summary);
    my ($course, $description);

    for my $item (@stuff) {
        if ($my_courses{$item}) {
            $course = $item;
            last;
        }
    }

    return when (!$course);

    # String problems hate them! Need to get it sorted
    my ($what) = $summary =~ /(Lektion|Laboration|F..rel..sning)/;

    my $str = $course;
    if ($what) { $str .= " $what"; }

    return $str;
}

my @lessons;

for my $event (@events) {

    my $summary = shorten_summary($event->property('summary')->[0]->value);

    # No short summary = nothing interesting
    if (!$summary) { next };

    my $f = DateTime::Format::Atom->new();

    # It's not a proper atom timestring! Might want to make something more robust?
    my $start = $f->parse_datetime( $event->start . 'Z' );
    my $end = $f->parse_datetime( $event->end . 'Z' );

    my $pretty_start = $start->strftime( "%H:%M" );
    my $pretty_end = $end->strftime( "%H:%M" );

    my $str = "$pretty_start - $pretty_end ";

    $str .= $summary;

    push (@lessons, $str);
}

if (scalar @lessons) {
    for my $lesson (sort @lessons) {
        #say $lesson;
        say "  \${voffset 8}$lesson";
    }
}
else {
    #say "I'm free!";
    say "  \${voffset 8}I'm free!";
}

