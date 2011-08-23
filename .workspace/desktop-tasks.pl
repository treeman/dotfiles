#!/usr/bin/perl -w

use utf8;
use locale;

use Modern::Perl;
use Getopt::Long;
use DateTime;

my $print_date;

GetOptions(
    'date' => \$print_date,
);

sub trim($)
{
    my $string = shift;
    $string =~ s/^\s+//;
    $string =~ s/\s+$//;
    return $string;
}

# today as YYYYMMDD
sub today
{
    my ($y, $m, $d) = (localtime(time))[5, 4, 3];
    $y += 1900;
    $m += 1;
    if ($m < 10) { $m = "0$m"; }
    if ($d < 10) { $d = "0$d"; }

    return "$y$m$d";
}

my $filter = join(" ", @ARGV);

my $what = `task what rc.annotations:none $filter`;

my @tasks = split(/\r?\n/, $what);

# Remove beginning whitespace, list names and ----
# Remove ending conf override, task num and whitespace
@tasks = @tasks[3 .. @tasks - 4];

my @line_stack;

for my $task (@tasks) {
    if (!($task =~ m/^\s*\d+/)) {
        #say "Need to pop that one:", $task;
        my $last = pop(@line_stack);
        my $new = $last . " " . trim($task);
        push(@line_stack, $new);
    }
    else {
        push(@line_stack, $task);
    }
}

for my $task (@line_stack) {
    # Match my special task what list
    my ($id, $pri, $due, $what) = $task =~ /
        \s*
        (\d+)
        \s*
        ([LMH]?)
        \s*
        (\d{4}-\d{2}-\d{2})?
        \s*
        (.*)
    /x;

    # YYYY-MM-DD
    my $date = $due;

    my $str = $what;

    my $color = "FFFFFF";

    # Ugly date comparison, but it works at least! (Doesn't take into consideration hours..)
    # Change to DateTime if/when needed I guess
    if (defined ($due)) {
        # YYYYMMDD
        $due =~ s/-//g;
        my $today = today();

        #say "due: $due";
        #say "today: $today";

        # Overdue
        if ($due < $today) {
            #$color = "BA0013";
            $color = "D42222";
        }
        # Due in one week
        elsif ($due - $today < 7) {
            $color = "FFB600";
        }
    }

    if (defined ($due) && $print_date) {
        my ($year, $month, $day) = split(/-/, $date);

        my $dt = DateTime->new(
            year => $year,
            month => $month,
            day => $day,
        );
        $str = "\${color $color}" . $dt->strftime( "%d %b" ) . "\${color FFFFFF},   $str";
    }
    else {
        $str = "\${color $color}$str";
    }

    # Trim leading/trailing whitespace
    $str = trim($str);

    say "\${voffset 8}  $str";
}

