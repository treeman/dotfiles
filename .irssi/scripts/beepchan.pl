use strict;
use vars qw($VERSION %IRSSI);
use Irssi qw(signal_add signal_emit settings_add_str settings_get_str
            settings_add_bool settings_get_bool);

$VERSION = '0.10';
%IRSSI = (
    authors     => 'Jerith',
    contact     => 'jerith@nightstar.net',
    name        => 'beepchan',
    description => 'Sends a beep signal when messages are received on '.
                    'selected channels.',
    license     => 'Public Domain',
);

sub beepchan {
    my ($server, $msg, $nick, $address, $target) = @_;

    return if not settings_get_bool('beep_on_channel');
    my $channels = settings_get_str('beep_channels');
    $channels =~ tr/ /|/;
    Irssi::signal_emit('beep') if $target =~ /$channels/i;
}

sub beephilight {
    my ($dest, $text, $stripped) = @_;

    return if not settings_get_bool('beep_on_hilight');

    Irssi::signal_emit('beep') if (($dest->{level} & (MSGLEVEL_HILIGHT|MSGLEVEL_MSGS))
                                && ($dest->{level} & MSGLEVEL_NOHILIGHT) == 0);
}

Irssi::signal_add('message public', 'beepchan');
Irssi::signal_add('message irc action', 'beepchan');
Irssi::signal_add('print text', 'beephilight');
Irssi::settings_add_bool('misc', 'beep_on_channel', 1);
Irssi::settings_add_bool('misc', 'beep_on_hilight', 1);
Irssi::settings_add_str('misc','beep_channels','#treecraft');

