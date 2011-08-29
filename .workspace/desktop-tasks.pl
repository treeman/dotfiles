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

my $what = trim(`task what rc.annotations:none $filter`);
#say $what;

my @tasks = split(/\r?\n/, $what);

while (scalar @tasks) {
    my $t = $tasks[0];
    if ($t !~/^\s*(\d+)/) {
        shift (@tasks);
    }
    else {
        last;
    }
}

# Remove two last items
pop (@tasks);
pop (@tasks);

#say join("\n", @tasks);

# Remove beginning whitespace, list names and ----
# Remove ending conf override, task num and whitespace
# Needs rework!!!!!!!
#@tasks = @tasks[3 .. @tasks - 4];
#@tasks = @tasks[1 .. @tasks - 1];

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

if (scalar @line_stack) {

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

        my $str = trim($what);

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

        print "  \${voffset 8}$str\n";
    }
}
else {
    say "  \${voffset 8}Nothing!";
}

