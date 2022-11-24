#!/usr/bin/perl
#This program generates test data for the iterative Rescorla-Wagner model,
#taking as the first and only parameter the name of the corpus on which to run tests.
#The preprocessed corpus should be found ./testingData/name_of_corpus.preproc

use v5.10;
use strict;
use warnings;

use File::Basename;
use File::Compare;
use File::Path qw(remove_tree);
use IPC::Open3;
use Errno qw(:POSIX);

#Flush output on it being written.
$| = 1;


my $reference_dir = "./testingData/";
my $tests_dir = "./testingOut/";
mkdir $tests_dir,0755 unless(-d $tests_dir);

if(@ARGV < 2)
{
	say "Please provide the build type (debug or release) as first argument and the name of the corpus as second argument" and die;
}

my $build_type = $ARGV[0];
my $corpus_name = $ARGV[1];
say "$reference_dir/$corpus_name.preproc does not exist" and die unless( -d "$reference_dir/$corpus_name.preproc");

my @traces = (
	"the,the\nthe,that",
	"the,that\t(every 10)",
	"mai,main\nthe,that",
	"the,main\t(first 11)",
	"the,foot\t(at 100 200 20000)",
	"the,the\t(at 20000)", #Leave this at index 5.
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
	""
);

# is_modified_parameter -s was removed (in iterative_rw commit aea27a8b4c8ccb8c)
my @is_modified_params = (
#	"-s",
	""
);


if(-d "$tests_dir/$corpus_name.output")
{
	remove_tree("$tests_dir/$corpus_name.output") or die "Could not remove old output: $!";
}
mkdir "$tests_dir/$corpus_name.output",0777;


my $language = "english";
$language = "german" if(${corpus_name} =~ /^larger.txt/);


#Build once with matrices and once without, to test whether or not the output is identical.
for my $build_matrices (0 .. 1)
{
	next if(@ARGV > 1 and $ARGV[1] eq "--no-matrices" and $build_matrices == 1);
	for my $i (0.. $#alphas)
	{
		#So that this doesn't take so long to run, only run traces for the first set of alphas...
		next if($i != 0 and $build_matrices);

		for my $j (0..$#traces)
		{
			next if $j > $#german_traces;
			for my $k (0 .. $#has_duplicates_params)
			{
				next if($k != 0 and $build_matrices);
				for my $l (0 .. $#is_modified_params)
				{
					next if($l != 0 and $build_matrices);
					open(my $outfile_handle_out,">", "$tests_dir/$corpus_name.output/${i}_${j}_${k}_${l}.result") or die "Couldn't open output for results";
					open(my $outfile_handle_err, ">", "$tests_dir/$corpus_name.output/${i}_${j}_${k}_${l}.error") or die "Couldn't open output for errors";
					my @matrix_param = $build_matrices ? ("-m","10000","20000","-o","$tests_dir/$corpus_name.output/${i}_${j}_${k}_${l}.matrices") : (); 
					my $pid = open3(my $programs_stdin ,my $programs_stdout,my $programs_stderr = "lvaluable",
						"$build_type/iterative_rescorla_wagner",
						"$reference_dir/${corpus_name}.preproc",
						"$corpus_name",
						"$alphas[$i]",
						"$betas[$i]",
						"$has_duplicates_params[$k]",
						"$is_modified_params[$l]",
						@matrix_param
					);
					if($language eq "english")
					{
						print $programs_stdin $traces[$j];
					}else
					{
						print $programs_stdin $german_traces[$j]
					}

					close($programs_stdin);
					print $outfile_handle_out $_ while <$programs_stdout>;
					close $outfile_handle_out;
					print $outfile_handle_err $_ while <$programs_stderr>;
					close $outfile_handle_err;
					waitpid $pid,0;
					my $retval = $?;
					if($retval)
					{
						say "./iterative_rescorla_wagner returned $retval status with default alpha=$alphas[$i], beta=$betas[$i] for traces of:". $language eq "english" ? $traces[$j] : $german_traces[$j];
						say "The command used: ./iterative_rescorla_wagner $reference_dir/${corpus_name}.preproc $corpus_name $alphas[$i] $betas[$i] $has_duplicates_params[$k] $is_modified_params[$l]";
						say "The following was output to standard error";
						open(my $fh,"<","$tests_dir/$corpus_name.output/${i}_${j}_${k}_${l}.error");
						print "\t$_" while<$fh>;
						die;

					}else
					{
						if(compare("$reference_dir/${corpus_name}.output/${i}_${j}_${k}_${l}.result","$tests_dir/${corpus_name}.output/${i}_${j}_${k}_${l}.result"))
						{
							say "Failed on test for run ($i,$j,$k,$l)";
							say "The command used was: ./iterative_rescorla_wagner $reference_dir/${corpus_name}.preproc $corpus_name $alphas[$i] $betas[$i]". (join @matrix_param, " ");
							say "The files $reference_dir/${corpus_name}.output/${i}_${j}_${k}_${l}.result  and $tests_dir/${corpus_name}.output/${i}_${j}_${k}_${l}.result differ";
							die;
						}else
						{
							if(($j == 5) && $build_matrices) #We test output_matrix_file_to_ascii
							{
								say "Testing output_matrix_file_to_ascii and whether matrix traces are equal to normal traces";
								system("printf \"the,the\\\\t\(at 20000\)\\\\n\" | $build_type/output_matrix_file_to_ascii $tests_dir/$corpus_name.output/${i}_${j}_${k}_${l}.matrices --simulate-trace > $tests_dir/$corpus_name.output/${i}_${j}_${k}_${l}.matrix_trace 2> /dev/null");
								if($?)
								{
									die "output_matrix_file_to_ascii gave nonzero exit status $?";
								}					
								if(compare("$reference_dir/${corpus_name}.output/${i}_${j}_${k}_${l}.result", "$tests_dir/$corpus_name.output/${i}_${j}_${k}_${l}.matrix_trace"))
								{
									say "Matrix trace differs from non-matrix trace";
									die;
								}
								say "Tested output_matrix_file_to_ascii";
							}
							say "Finished running test for run ($i,$j,$k,$l)";
						}
					}
				}
			}
		}
	}
}

say "Finished testing traces";
my @threadnumbers = (1,2,3,4,32);
for my $i (0 .. $#threadnumbers)
{
	next if(@ARGV > 1 and $ARGV[1] eq "--no-matrices");
	#We don't run everything at every single number of threads, just one test run...
	system("echo | $build_type/iterative_rescorla_wagner ./${reference_dir}/${corpus_name}.preproc $corpus_name 0.001 0.1 -d -s -t $threadnumbers[$i] -m 10000 20000 -o ${tests_dir}/${corpus_name}.output/0_0_0_0.matrices.withthreads.$threadnumbers[$i] 2> /dev/null > /dev/null");
	if(compare("$tests_dir/$corpus_name.output/0_0_0_0.matrices", "$tests_dir/$corpus_name.output/0_0_0_0.matrices.withthreads.$threadnumbers[$i]"))
	{
		say "Different number of threads resulted in different output";
		die;
	}
	say "Tested with $threadnumbers[$i] threads";
}
say "Finished testing parallelism";
say "Finished testing everything";
