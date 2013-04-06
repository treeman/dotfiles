#!/usr/bin/perl -w

use utf8;
use locale;

use Modern::Perl;

sub trim($)
{
    my $string = shift;
    $string =~ s/^\s+//;
    $string =~ s/\s+$//;
    return $string;
}

my $what = trim(`ticker --limit 10 --format list`);
my @list = split(/^/, $what);

for my $item (@list) {
    chomp $item;
    say "  \${voffset 8}$item";
}

