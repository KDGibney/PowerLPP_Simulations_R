loop_anova_betweensubjects_simulations <- function(test_mode = 0,
                                                   datadir = 'W:/VersaceLab/Experiments/kyla/PowerLPP/results_betweensubjects/withheader',
                                                   outputdir = 'W:/VersaceLab/Experiments/kyla/PowerLPP/results_r_betweensubjects',
                                                   testdatadir = 'W:/VersaceLab/Experiments/kyla/PowerLPP/TEST/results_betweensubjects/withheader',
                                                   testoutputdir = 'W:/VersaceLab/Experiments/kyla/PowerLPP/TEST/results_r_betweensubjects') {
  #R function for ANOVAs of between-subject monte carlo simulations for powerlpp
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
  #(results_r_betweensubjects.csv') to datadir

  #############################################################################################################
  #TEST MODE

  #IF YOU ENABLE TEST MODE, RESTULS WILL WRITE TO A TEST DIRECTORY

  if(test_mode == 1) {
    datadir = testdatadir
    outputdir = testoutputdir
  }

  ###########################################################################################################
  #BEGIN FUNCTION

  results_file <- paste(outputdir,'/results_r_betweensubjects_simulations.csv',sep='') #create filename

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


  	num_steps = subjects #number of subjects to be taken at a time
  	numrows = nrow(data.current) #total number of rows in the file
  	halfpoint = numrows/2 #the end of group 1
  	start = halfpoint+1 #the beginning of group 2
  	finish = numrows #the end of group 2
  	LPP_group_one <- data.current$LPP[1:halfpoint] #extract group 1 LPP data
  	LPP_group_two <- data.current$LPP[start:finish] #extract group 2 LPP data
  	group_one = data.current$group[1:halfpoint] #extract group assignment for group 1
  	group_two = data.current$group[start:finish] #extract group assignment for group 2
  	size1 <- length(LPP_group_one) #get the sizes of each group
  	size2 <- length(LPP_group_two)
  	index1 = seq(1,size1,num_steps) #create an index of start values
  	index2 = seq(num_steps,size1,num_steps) #create an index of stop values
  	iterations = numrows/(num_steps*2); #number of times to repeat


  	for (j in 1:iterations){ #loop through each iteration of the file
  		combined_LPPs <- c(LPP_group_one[index1[j]:index2[j]],LPP_group_two[index1[j]:index2[j]]) #combine LPPs from group 1 and 2
  		combined_groups <- c(group_one[index1[j]:index2[j]],group_two[index1[j]:index2[j]])  #combine group designation

  		lppdata <- data.frame(combined_LPPs,combined_groups) #make a dataframe
  		anova = aov(combined_LPPs ~ as.factor(combined_groups), data=lppdata)	#y (LPPs) predicted by X (group assignment)
  		names = unlist(summary(anova)) #list all the results of the ANOVA

      #these are all the outputs we want printed in our results file
  		print(paste(subjects,"subjects"))
  		print(paste(trials,"trials"))
  		print(paste(effectsize,"microvolts"))
  		print("F value")
  		print(paste(names[7]))
  		print("P value")
  		print(paste(names[9]))
  }
  }

  sink() #end the diversion
}
