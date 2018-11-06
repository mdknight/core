#create list unique projects still in HiSeq Scans folder and associated run

library(sqldf)

#Set working directory to hiseq folder and get list of run directories
setwd("Z:/HiSeq Scans/HiSeq4000 Scans")
list.dirs(recursive=FALSE)
runlist <- list.dirs(recursive=FALSE)


for(i in 1:length(runlist)){

  #set working directory to current run in list
  setwd(runlist[i])

  #process for each run (except DISCARD folder, add additional exceptions if needed)
  if(runlist[i]!="./DISCARD"){
    #read in file to new df
    
    ssHeader<-read.csv2("SampleSheet.csv", header=FALSE, nrows=19)
    ssData<-read.csv2("SampleSheet.csv", sep=",", header = TRUE, skip=19)
    
    # need unique sample_projects from sample sheets
    projectsdf<- data.frame(sqldf('SELECT DISTINCT("Sample_Project") from ssData'))
    # add run ID from sample sheet to second column
    projectsdf[,2]<- substring(ssHeader[4,1],17)
    
    #write/append new .csv change name for new file
    write.table(projectsdf, file="../projectslist.csv",row.names=FALSE, sep=",", append=TRUE, quote=FALSE, col.names = FALSE)
    
  }
  #return to hiseq folder
  setwd("..")

}