//(c) 2013 Nathanael Schilling
#include <iostream>

#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <boost/filesystem.hpp>

#include "common_type_definitions.h"
#include "file_management.h"
#include "util.h"
#include "matrix_io.h"
#include "../common/serialization.h"

//This program is used for creating everything needed for input for a (very small) test program.
//It writes this into ./testingData/toy.preproc

using namespace std;

namespace fs = boost::filesystem;
int main(int argc, char* argv[])
{
	(void) argc;
	(void) argv;
	recursively_remove_directory("./testingData/toy.preproc");
	recursively_remove_directory("./testingData/toy.output");
	mkdir("./testingData",0777);
	if(mkdir("./testingData/toy.preproc", 0777))
	{
		cerr<<"Could not create directory ./testingData/toy.preproc"<<endl;
		return -1;
	}
	if(mkdir("./testingData/toy.output",0777))
	{
		cerr<<"Could not create directory ./testingData/toy.output"<<endl;
		return -1;
	}

	ofstream outcomes_file("./testingData/toy.preproc/toy.outcomes");
	if(!outcomes_file)
	{
		cerr<<"Could not open ./testingData/toy.preproc/toy.Outcomes for writing"<<endl;
		return -1;
	}
	outcomes_file<<"the 1"<<endl;
	outcomes_file<<"test 2"<<endl;
	outcomes_file<<"here 3"<<endl;
	outcomes_file.close();

	ofstream cues_file("./testingData/toy.preproc/toy.cues");
	if(!cues_file)
	{
		cerr<<"Could not open ./testingData/toy.preproc/toy.Cues for writing"<<endl;
		return -1;
	}
	cues_file<<"this 1"<<endl;
	cues_file<<"cue 3"<<endl;
	cues_file<<"is 2"<<endl;
	cues_file.close();

	ofstream betas_file("./testingData/toy.preproc/toy.betas");
	if(!betas_file)
	{
		cerr<<"Could not open ./testingData/toy.preproc/toy.betas for writing"<<endl;
		exit(-1);
	}

	vector<double> betas {0, 1.0, 0.2, 0.0001};
	vector<double> alphas {0, 0.5, 0.3, 0.4};

	betas_file<<"the 1.0"<<endl;
	betas_file<<"here 0.0001"<<endl;
	betas_file<<"test 0.2"<<endl;
	betas_file.close();


	ofstream alphas_file("./testingData/toy.preproc/toy.alphas");
	if(!alphas_file)
	{
		cerr<<"Could not open ./testingData/toy.preproc/toy.alphas for writing"<<endl;
		exit(-1);
	}
	alphas_file<<"this 0.5"<<endl;
	alphas_file<<"cue 0.4"<<endl;
	alphas_file<<"is 0.3"<<endl;
	betas_file.close();

	Events sample_events;
	Event first_event;
	first_event.Freq = 1;
	first_event.Cues.push_back(2);
	first_event.Cues.push_back(1);
	first_event.Cues.push_back(3);
	sort(first_event.Cues.begin(), first_event.Cues.end());
	first_event.Outcomes.push_back(1);
	first_event.Outcomes.push_back(2);
	sort(first_event.Outcomes.begin(), first_event.Outcomes.end());
	Event second_event;
	second_event.Freq = 1;
	second_event.Cues.push_back(1);
	second_event.Cues.push_back(3);
	sort(second_event.Cues.begin(),second_event.Cues.end());
	second_event.Outcomes.push_back(3);
	second_event.Outcomes.push_back(1);
	sort(second_event.Outcomes.begin(), second_event.Outcomes.end());
	sample_events.push_back(first_event);
	sample_events.push_back(second_event);
	if(mkdir("./testingData/toy.preproc/toy.events",0777))
	{
		cerr<<"Could not create ./testingData/toy.preproc/toy.preproc/toy.events"<<endl;
		exit(-1);
	}
	writeEvents("./testingData/toy.preproc/toy.events/events_0_0.dat",sample_events);

	//We write our calculated reference 4*4 matrix to a file.
	//vector<AssociationMatrix> ReferenceMatrices(3,AssociationMatrix(4,4));
	AssociationMatrix ReferenceMatrices[3] = {AssociationMatrix(4,4,vector<int>(4,0)),AssociationMatrix(4,4,vector<int>(4,0)),AssociationMatrix(4,4,vector<int>(4,0))};



	double vTotal = 0;
	double lambda = 100;

	ReferenceMatrices[0](0,0) = 0;
	ReferenceMatrices[0](0,1) = 0;
	ReferenceMatrices[0](0,2) = 0;
	ReferenceMatrices[0](0,3) = 0;
	ReferenceMatrices[0](1,0) = 0;
	ReferenceMatrices[0](1,1) = 0;
	ReferenceMatrices[0](1,2) = 0;
	ReferenceMatrices[0](1,3) = 0;
	ReferenceMatrices[0](2,0) = 0;
	ReferenceMatrices[0](2,1) = 0;
	ReferenceMatrices[0](2,2) = 0;
	ReferenceMatrices[0](2,3) = 0;
	ReferenceMatrices[0](3,0) = 0;
	ReferenceMatrices[0](3,1) = 0;
	ReferenceMatrices[0](3,2) = 0;
	ReferenceMatrices[0](3,3) = 0;

	//The Cues 1,2,3 are present and the Outcomes 1,2 are present.
	ReferenceMatrices[1] = ReferenceMatrices[0];
	vTotal = 0; 
	ReferenceMatrices[1](1,0) = 0;//We never see outcome 0
	ReferenceMatrices[1](1,1) = alphas[1]*betas[1]*(lambda - vTotal);
	ReferenceMatrices[1](1,2) = alphas[1]*betas[2]*(lambda - vTotal);
	ReferenceMatrices[1](1,3) = 0; //We haven't seen outcome 3 yet.
	ReferenceMatrices[1](2,0) = 0;
	ReferenceMatrices[1](2,1) = alphas[2]*betas[1]*(lambda - vTotal);
	ReferenceMatrices[1](2,2) = alphas[2]*betas[2]*(lambda - vTotal);
	ReferenceMatrices[1](2,3) = 0;
	ReferenceMatrices[1](3,0) = 0;
	ReferenceMatrices[1](3,1) = alphas[3]*betas[1]*(lambda - vTotal);
	ReferenceMatrices[1](3,2) = alphas[3]*betas[2]*(lambda - vTotal);
	ReferenceMatrices[1](3,3) = 0;


	//The Cues 1,3 are present and the Outcomes 1,3 are present.
	ReferenceMatrices[2] = ReferenceMatrices[1];
		//Leave column 0 unchanged because outcome 0 has not occured until now.
		//Outcome 1 occurs here, let's calculate the vTotal for it:
		vTotal = ReferenceMatrices[1](1,1) + ReferenceMatrices[1](3,1);
		ReferenceMatrices[2](0,1) += 0; //Cue 0 did not occur this time.
		ReferenceMatrices[2](1,1) += alphas[1]*betas[1]*(lambda - vTotal); //Cue 1 does occur.
		ReferenceMatrices[2](2,1) += 0; //Cue 2 does not occur.
		ReferenceMatrices[2](3,1) += alphas[3]*betas[1]*(lambda - vTotal); //Cue 3 does occur.
		//Outcome 2 occured in the last event, so we need to calculate vTotal for it:
		vTotal = ReferenceMatrices[1](1,2) + ReferenceMatrices[1](3,2);
		ReferenceMatrices[2](0,2) += 0; //Cue 0 did not occur this time.
		ReferenceMatrices[2](1,2) += alphas[1]*betas[2]*(0 - vTotal); //Cue 1 does occur.
		ReferenceMatrices[2](2,2) += 0; //Cue 2 does not occur.
		ReferenceMatrices[2](3,2) += alphas[3]*betas[2]*(0 - vTotal); //Cue 3 does occur.
		//Outcome 3 occurs here, let's calculate the vTotal for it:
		vTotal = ReferenceMatrices[1](1,3) + ReferenceMatrices[1](3,3);
		ReferenceMatrices[2](0,3) += 0; //Cue 0 did not occur this time.
		ReferenceMatrices[2](1,3) += alphas[1]*betas[3]*(lambda - vTotal); //Cue 1 does occur.
		ReferenceMatrices[2](2,3) += 0; //Cue 2 does not occur.
		ReferenceMatrices[2](3,3) += alphas[3]*betas[3]*(lambda - vTotal); //Cue 3 does occur.

	ofstream matrix_output_file("./testingData/toy.output/matrixes.output");
	writeMatrixFileHeader(matrix_output_file,
			{
			4,
			4,
			fs::canonical(fs::path("./testingData/toy.preproc")).string(),
			string("toy"),
			1.0,
			1.0,
			{0,1,2},
			false
			}
		);
	appendMatrixToFile(matrix_output_file,ReferenceMatrices[0],0);
	appendMatrixToFile(matrix_output_file,ReferenceMatrices[1],1);
	appendMatrixToFile(matrix_output_file,ReferenceMatrices[2],2);
	writeMatrixFileFooter(matrix_output_file);
	matrix_output_file.close();

	ifstream matrix_output_file_readable("./testingData/toy.output/matrixes.output");
	ofstream matrix_output_file_pretty("./testingData/toy.output/matrixes.output.pretty");
	prettyPrintMatrixFileToOstream(matrix_output_file_pretty,matrix_output_file_readable);



	return 0;
}
