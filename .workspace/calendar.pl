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
use LWP::UserAgent;
use Data::Dumper;

# Url for timeedit
my @timeedit = qw(
    https://se.timeedit.net/web/liu/db1/schema/ri66X826X08Z04Q5Z86g6Y10y4026Y59705gQY5Q57778668Y54467666770Q454XY7.ics
);

# Correct times
my $today = DateTime->now()->set( hour => 0, minute => 0, second => 0 );
my $tomorrow = DateTime->now()->set( hour => 0, minute => 0, second => 0 )->add( days => 1 );

# Change day to where we have a few lessons
#$today = DateTime->new( year => 2011, month => 8, day => 29 );
#$tomorrow = DateTime->new( year => 2011, month => 8, day => 30 );

my $span = DateTime::Span->from_datetimes( start => $today, end => $tomorrow );

my @events;

my $ua = LWP::UserAgent->new;
$ua->timeout(10);
$ua->env_proxy;

for my $url (@timeedit) {
    my $response = $ua->get($url);

    die $response->status_line if !$response->is_success;

    my $str = $response->decoded_content;
    my $calendar = Data::ICal->new(data => $str);

    @events = (@events, $calendar->events($span));
}

sub shorten_summary
{
    my ($summary) = @_;

    utf8::encode($summary);
    #say Dumper($summary);

    # OOOooooh those hacks!
    my ($course) = $summary =~ /Kurs: (\S+)/;
    my ($location) = $summary =~ /Lokal: (\S+)/;
    my ($type) = $summary =~ /Undervisningstyp: (\S+)/;

    $course =~ s/,//;
    $location =~ s/,//;
    $type =~ s/,//;

    return "$course $type $location";
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
    $start->set_time_zone( 'Europe/Stockholm' );
    $end->set_time_zone( 'Europe/Stockholm' );

    my $pretty_start = $start->strftime( "%H:%M" );
    my $pretty_end = $end->strftime( "%H:%M" );

    my $str = "$pretty_start - $pretty_end ";

    $str .= $summary;

    push (@lessons, $str);
}

# Remove duplicates in list
sub uniq {
    my %seen = ();
    my @r = ();
    foreach my $a (@_) {
        unless ($seen{$a}) {
            push @r, $a;
            $seen{$a} = 1;
        }
    }

    return @r;
}

if (scalar @lessons) {
    for my $lesson (uniq (sort @lessons)) {
        say "  \${voffset 8}$lesson";
    }
}
else {
    say "  \${voffset 8}I'm free!";
}

