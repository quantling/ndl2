#!/usr/bin/perl
use strict;
use warnings;
use v5.10;
use File::Compare;
use File::Path qw(remove_tree);

if(@ARGV < 1)
{
	say "Please give build type (debug or release) as argument" and die;
}
my $BUILD_TYPE = $ARGV[0];

mkdir "./testingOut", 0777;
if(-d "./testingOut/toy.output")
{
	remove_tree("./testingOut/toy.output") or die "Could not remove old tests directory";
}
mkdir "./testingOut/toy.output", 0777;
system("$BUILD_TYPE/iterative_rescorla_wagner ./testingData/toy.preproc toy 1.0 1.0 -o ./testingOut/toy.output/matrixes.output -m 0 1 2 </dev/null >/dev/null 2>/dev/null");
system("$BUILD_TYPE/output_matrix_file_to_ascii ./testingOut/toy.output/matrixes.output > ./testingOut/toy.output/matrixes.output.pretty");
if(compare("./testingOut/toy.output/matrixes.output.pretty", "./testingData/toy.output/matrixes.output.pretty"))
{
	die "Tests of handmade matrix failed";
}else
{
	say "Succeeded on test of handmade matrix";
	exit 0;
}
