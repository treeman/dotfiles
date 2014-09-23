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
    https://se.timeedit.net/web/liu/db1/schema/ri607856X25Z04Q5Z26g1Y90y5056Y58Q07gQY5Q53777.ics
    https://se.timeedit.net/web/liu/db1/schema/ri65YXQ0519Z53Qv8X0258Q6y9Y220366Yy5Y7gQ8076970ZZ9Q4y6Z1YQ657x2Yo87X427265g616.ics
    https://se.timeedit.net/web/liu/db1/schema/ri657XQQ519Z50Qv97065gZ6y7Y7202Q6Y45Y8.ics
    https://se.timeedit.net/web/liu/db1/schema/ri607856X25Z04Q5Z36g1Y90y5046Y50Q07gQY5Q53777.ics
);

# Correct times
my $today = DateTime->now()->set( hour => 0, minute => 0, second => 0 );
my $tomorrow = DateTime->now()->set( hour => 0, minute => 0, second => 0 )->add( days => 1 );

# Change day to where we have a few lessons
#$today = DateTime->new( year => 2014, month => 9, day => 5 );
#$tomorrow = DateTime->new( year => 2014, month => 9, day => 6 );

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

#say Dumper(@events);

sub shorten_summary
{
    my ($event) = @_;

    my $summary = $event->property('summary')->[0]->value;
    my $location = $event->property('location')->[0]->value;
    utf8::encode($summary);

    my @list = split (/\s*,\s*/, $summary);
    my ($course) = $list[0] =~ /Kurs: (\S+)/;
    my $type = $list[1];

    return "$course $type $location";
}

my @lessons;

for my $event (@events) {

    #say Dumper($event);

    my $summary = shorten_summary($event);

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

