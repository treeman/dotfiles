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

my $what = trim(`/home/tree/projects/mangaprobe/mangas.pl -s -c`);
my @list = split(/\r?\n/, $what);

my $limit = 5;

if (scalar @list > $limit) {
    @list = @list[0 .. $limit - 1];
}

for my $item (@list) {
    print "  \${voffset 8}$item\n";
}

