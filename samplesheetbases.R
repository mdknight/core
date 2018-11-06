#script to read in samplesheet downloaded from basespace and transform 
#8-base indexes to 6

#setwd to file location
setwd("./SampleSheet") 

#read in file to new df
ssHeader<-read.csv2("SampleSheet.csv", header=FALSE, nrows=19)
ssData<-read.csv2("SampleSheet.csv", sep=",", header = TRUE, skip=19)

#trim last two characters from index to return 6-base index
ssData[,7]<-substr(ssData[,7], 1, 6)

#write new .csv with 6-base indexes
write.table(ssHeader, file="SampleSheet2.csv",row.names=FALSE, col.names=FALSE, sep=",", quote=FALSE)
write.table(ssData, file="SampleSheet2.csv",row.names=FALSE, sep=",", append=TRUE, quote=FALSE)