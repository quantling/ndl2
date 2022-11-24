#!/usr/bin/perl
#This program generates test data for the iterative Rescorla-Wagner model,
#taking as the first and only parameter the name of the corpus on which to run tests.
#The preprocessed corpus should be found ./testingData/name_of_corpus.preproc

use v5.10;
use strict;
use warnings;

use File::Compare;
use File::Basename;
use IPC::Open3;
use File::Path qw(remove_tree);

#Flush output on it being written.
$| = 1;


my $reference_dir = "./testingData/";

if(@ARGV != 1)

{
	say "Please give the name of the corpus as the argument" and die;
}

my $corpus_name = $ARGV[0];
say "$reference_dir/$corpus_name.preproc does not exist" and die unless( -d "$reference_dir/$corpus_name.preproc");

my @traces = (
	"the,the\nthe,that",
	"the,that\t(every 10)",
	"mai,main\nthe,that",
	"the,main\t(first 11)",
	"the,foot\t(at 100 200 20000)",
	"the,the\t(at 20000)",
);

my @german_traces = (
	"der,der\nder,das",
	"der,das\t(every 10)",
	"ist,sein\nder,das",
	"the,der\t(first 11)",
	"che,bücher\t(at 100 200 20000)"
);
my @alphas = (
	"0.001",
	"0.25",
);

my @betas = (
	"0.1",
	"0.01",
);

my @has_duplicates_params = (
	"-d",
	"",
);

my @is_modified_params = (
	"-s",
	""
);

remove_tree("$reference_dir/$corpus_name.output");
mkdir "$reference_dir/$corpus_name.output",0777;

my $language = "english";
$language = "german" if(${corpus_name} =~ /^larger.txt/);

for my $i (0.. $#alphas)
{
	for my $j (0..$#traces)
	{
		for my $k (0 .. $#has_duplicates_params)
		{
			for my $l (0 .. $#is_modified_params)
			{
				open(my $outfile_handle_out,">", "$reference_dir/$corpus_name.output/${i}_${j}_${k}_${l}.result") or die "Couldn't open output for results";
				open(my $outfile_handle_err, ">", "$reference_dir/$corpus_name.output/${i}_${j}_${k}_${l}.error") or die "Couldn't open output for errors";

				my $pid = open3(my $programs_stdin ,my $programs_stdout,my $programs_stderr = "lvaluable",
					"./iterative_rescorla_wagner",
					"$reference_dir/${corpus_name}.preproc",
					"$corpus_name",
					"$alphas[$i]",
					"$betas[$i]",
					"$has_duplicates_params[$k]",
					"$is_modified_params[$l]",
				);
				if($language eq "english")
				{
					print $programs_stdin $traces[$j];
				} else
				{
					print $programs_stdin $german_traces[$j]
				}
				close($programs_stdin);
				print $outfile_handle_out $_ while <$programs_stdout>;
				print $outfile_handle_err $_ while <$programs_stderr>;
				waitpid $pid,0;
				my $retval = $?;
				close $outfile_handle_out;
				if($retval)
				{
					say "./iterative_rescorla_wagner returned $retval status with default alpha=$alphas[$i], beta=$betas[$i] for traces of:\n". $language eq "english"? $traces[$j] : $german_traces[$j];
					say "The command used: ./iterative_rescorla_wagner $reference_dir/${corpus_name}.preproc $corpus_name $alphas[$i] $betas[$i] $has_duplicates_params[$k] $is_modified_params[$l]";
					say "The following was output to standard error";
					open(my $fh,"<","$reference_dir/$corpus_name.output/${i}_${j}_${k}_${l}.error");
					print "\t$_" while<$fh>;
					die;

				}else
				{

						if($corpus_name =~ /^(.*)\.split$/)
						{
							my $otherdir = $1;
							if(File::Compare::compare(
							"$reference_dir/${otherdir}.output/${i}_${j}_${k}_${l}.result",
							"$reference_dir/${corpus_name}.output/${i}_${j}_${k}_${l}.result"
								) != 0
							)
							{
								say "Split and non-split reference data do not match for:";
								say "races of:\n". $language eq "english"? $traces[$j] : $german_traces[$j];
								say "The command used: ./iterative_rescorla_wagner $reference_dir/${corpus_name}.preproc $corpus_name $alphas[$i] $betas[$i] $has_duplicates_params[$k] $is_modified_params[$l]";
								die;
							}
						}
					say "Finished creating reference data on run ($i,$j,$k,$l)";
				}
			}
		}
	}
}
say "Finished making reference data";

