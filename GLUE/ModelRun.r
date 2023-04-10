##This is the function to run the DSSAT model.
ModelRun<-function(WD, DSSATD, CropName, GenotypeFileName, CultivarID, RoundOfGLUE, TotalParameterNumber, NumberOfModelRun, RandomMatrix)
{
  eval(parse(text = paste("write('',file='",DSSATD,"/Evaluate_output.txt')",sep = '')));
  #Empty the Evaluate_output.txt file so as to save new data in Evaluate file in each model run.

  ParameterSetIndex<-c();
  #Set the information which parameter should be finally selected. If there is error in the model outputs,
  #the corresponding parameter set will be neglected.

for (i in 1:NumberOfModelRun)
{
################### 1. Model Run ##################
ModelRunNumber<-i
eval(parse(text = paste("source('",WD,"/GenotypeChange.r')",sep = '')));#Tell the location of the function.
GenotypeChange(DSSATD, CropName, GenotypeFileName, CultivarID, TotalParameterNumber, ModelRunNumber, RandomMatrix); #Change the genotype file.

setwd(DSSATD);
#Set the path for program to call the bath file running.

eval(parse(text = paste("system('",DSSATD,"/dscsm048 B DSSBatch.v48')",sep = '')));
#Call the bath file to run the model.
if (file.exists("Evaluate.OUT")== F)
{
  next;
}else
{
 eval(parse(text = paste("EvaluateOut<-readLines('",DSSATD,"/Evaluate.OUT',n=-1)",sep = '')));
 #Read the output in evaluate file.

 eval(parse(text = paste("write(EvaluateOut, file = '",DSSATD,"/Evaluate_output.txt',append = F)",sep = '')));
 #Save the evaluate output, but replace the previous output.

################### 2. Data Processing ##################
 eval(parse(text = paste("EvaluateFile<-readLines('",DSSATD,"/Evaluate_output.txt',n=-1)",sep = '')));

 Error1Address<-match('NaN',EvaluateFile);
 Error2Address<-match("********",EvaluateFile);
 ##Make a judgement with the Evaluate file. If there are something like "********", or "NaN" appeared
 ##in the Evaluate file, this set of model run will be neglected directly, don't need go for data processing.

 FileLength<-length(EvaluateFile);
 TreatmentNumber<-(FileLength-3);

 if (is.na(Error1Address) & is.na(Error2Address))
 {
 eval(parse(text = paste("source('",WD,"/OutputProcessing.r')",sep = '')));
 OutputProcessing(WD, DSSATD, CropName, RoundOfGLUE, ModelRunNumber);
 #Call the function to process the output data of evaluate and plant growth in each model run.

 ParameterSetIndex<-c(ParameterSetIndex,i);
 #Select the paramter set that match the requirement, i.e. do not have bad outputs.
 }
}

}
 ##End of all model runs.

if(RoundOfGLUE==1)
{
RealRandomSets<-RandomMatrix[ParameterSetIndex,];
eval(parse(text = paste("write(t(RealRandomSets), file = '",DSSATD,"/RealRandomSets_1.txt',ncolumns =TotalParameterNumber)",sep = '')));
##Get and save really used random parameter sets as a table for future use.
} else
{
RealRandomSets<-RandomMatrix[ParameterSetIndex,];
eval(parse(text = paste("write(t(RealRandomSets), file = '",DSSATD,"/RealRandomSets_2.txt',ncolumns =TotalParameterNumber)",sep = '')));
}

}

