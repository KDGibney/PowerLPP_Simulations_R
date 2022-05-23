loop_anova_withinsubjects_simulations <- function(test_mode = 0,
                                                  datadir = 'W:/VersaceLab/Experiments/kyla/PowerLPP/results_withinsubjects/univariate_withheader',
                                                  outputdir = 'W:/VersaceLab/Experiments/kyla/PowerLPP/results_r_withinsubjects',
                                                  testdatadir = 'W:/VersaceLab/Experiments/kyla/PowerLPP/TEST/results_withinsubjects/univariate_withheader',
                                                  testoutputdir = 'W:/VersaceLab/Experiments/kyla/PowerLPP/TEST/results_r_withinsubjects') {
#R function for repeated-measures ANOVAs of within-subject monte carlo simulations for powerlpp
#Authored by Kyla Gibney and George Kypriotakis
#Direct correspondence to: KDGibney@mdanderson.org

###############################################################################################################
#INS

#datadir: directory that the data to be analyzed is stored in

#outpurdir: directory that the results of the ANOVAs will be stored in

#testdatadir: if test_mode = 1, function will look here for data to be processed

#testoutputdir: if test_mode = 1, function will write results of ANOVAS here

#test_mode: dummy variable to trigger test mode.

##############################################################################################################
#OUTS

#this function does not return anything to the workspace, but will write a results file
#(results_r_withinsubjects.csv') to datadir

#############################################################################################################
#TEST_MODE
  #IF YOU ENABLE TEST MODE, RESULTS WILL WRITE TO A TEST DIRECTORY

  if(test_mode == 1) {
    datadir = testdatadir
    outputdir = testoutputdir
  }

###########################################################################################################
#BEGIN FUNCTION

  results_file <- paste(outputdir,'/results_r_withinsubjects_simulations.csv',sep='') #create filename

  sink(results_file) # this diverts R output to the connection provided by the file to write output

  files <- list.files(datadir, pattern="*.csv", full.names = T) #list all files in datadir

  num_files = seq_along(files) #total number of files in datadir


  for (i in num_files){ #iterate through each file

	  data.i <- files[i] #current file being processed

  	data.current <- read.csv(data.i)  # read the current data file

  	parsefilename = strsplit(data.i, "_subjects")
  	parsesubjects = strsplit(parsefilename[[1]][1],"sampled_")
  	subjects = as.numeric(parsesubjects[[1]][2]) #get number of subjects from file name

  	parsetrialnumber = strsplit(data.i, "_trials")
  	parsetrials = strsplit(parsetrialnumber[[1]][1],"_subjects_")
  	trials = as.numeric(parsetrials[[1]][2]) #get number of trials from file name

  	parseeffectsize = strsplit(data.i,"_microvolts")
  	parseeffect = strsplit(parseeffectsize[[1]][1],"trials_")
  	effectsize = as.numeric(parseeffect[[1]][2]) #get effect size from file name


  	num_steps = subjects*2 #how many rows are passed into the ANOVA at one time
  	#this should be 2x the number of subjects because there are 2 measurements for each subject

  	numrows = nrow(data.current) #total number of rows in the current datafile
  	index1 = seq(1,numrows,num_steps) #create indices along which to sequence, from 1:total num rows in steps of num_steps
  	index2 = seq(num_steps,numrows,num_steps) #same as before but starting at num_steps
  	iterations = numrows/num_steps #how many times we will loop through the data file

  	for (j in 1:iterations){ #loop through the data file

  		Rep_M_Anova <- aov(LPP[index1[j]:index2[j]] ~ factor(condition[index1[j]:index2[j]]) + Error(factor(subjectID[index1[j]:index2[j]])), data=data.current)
  		# y (LPP) predicted by X (condition)
  		# Error= experimental design - here repeated measures).  We can change that to any model.
  		sum_test = unlist(summary(Rep_M_Anova)) #list the results of the ANOVA


  		#these are all the outputs we want printed in our results file
  		print(paste(subjects,"subjects"))
  		print(paste(trials,"trials"))
  		print(paste(effectsize,"microvolts"))
  		print(paste("F val"))
  		print(paste(sum_test[12]))
  		print(paste("P val"))
  		print(paste(sum_test[14]))
  	}
  }

  sink() #ending the diversion
  }
