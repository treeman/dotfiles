#!/usr/bin/perl -w
use utf8;
use locale;

use Modern::Perl;
use DateTime;

my @spots = qw(America/Los_Angeles America/New_York UTC Europe/Stockholm Asia/Seoul);
map { my $time = DateTime->now(); $time->set_time_zone($_); $_ = $time; } @spots;

my @timezones;
my @local_times;

# Easy to build up two separate string with two lists
for my $spot (@spots) {
    if ($spot->time_zone_short_name eq "UTC") {
        push (@timezones, "UTC/GMT");
    }
    else {
        push (@timezones, $spot->time_zone_short_name);
    }
    push (@local_times, $spot->strftime( "%H:%M"));
}

# Pritty print for conky two strings, line up with wallpaper
print "  \${color2}$timezones[0]\${offset 214}$timezones[1]\${offset 360}$timezones[2]\${offset 35}$timezones[3]\${offset 608}$timezones[4]";

print "\${goto 10}\${color1}\${voffset 20}$local_times[0]\${offset 200}$local_times[1]\${offset 356}$local_times[2]\${offset 41}$local_times[3]\${offset 596}$local_times[4]";

