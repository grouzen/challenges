
use strict;
use warnings;

my @letters = ('a' .. 'z');

my $s = <>;
my @uniq = keys %{{ map { $_ => 1 } (grep { /[[:alpha:]]/ } split(//, lc $s)) }};

if($#uniq eq $#letters) {
    print "pangram";
} else {
    print "not pangram";
}
