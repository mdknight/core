#script to read in lightcycler 480 raw data and generate table with 
#cycle # for Lexogen Quantseq prep

#setwd to file location
setwd("./qPCR") 

#read in file to new df
nqpcr<-read.table("qPCR_Results.txt", header = TRUE, sep="\t", skip=1)
#header has "#" character and an extra tab at the end of 2nd row. remove from file
#X465.510 - row with data, SamplePos with sample, Cycle with cycle#

library(sqldf)
#get list of SamplePos and create a df with SamplePos and Cycles
cycledf<- data.frame(sqldf('SELECT DISTINCT("SamplePos") from nqpcr'))
#add column for cycle# at 50%
cycledf$Cycles <- NA
for(i in 1:384)
  {
  #find highest X465.510 value for each Sample
  spos<-toString(cycledf$SamplePos[i])
  sql<-sprintf('SELECT MAX("X465.510") from nqpcr WHERE SamplePos="%s"',spos)
  vmax<-sqldf(sql)
  
  #find lowest X465.510 value for each Sample
  sql<-sprintf('SELECT MIN("X465.510") from nqpcr WHERE SamplePos="%s"',spos)
  vmin<-sqldf(sql)
  
  #find (highest-lowest)/2 for each Sample
  v50<- as.numeric(vmin+((vmax-vmin)/2))
  
  #find first cycle that reaches 50% value for each Sample
  sql<-sprintf('SELECT MIN("Cycle") from nqpcr WHERE SamplePos="%s" and "x465.510">%f',spos, v50)
  vcycle<-sqldf(sql)
  cycledf$Cycles[i]<- vcycle
}

#flatten object (incompatable list types)
dfcycles <- apply(cycledf,2,as.character)
#saves two column .csv with well and number of recommended cycles
write.csv(dfcycles, file="2017-9170cycles.csv",row.names=FALSE)
#order df in A1,B1,... (column by column) ordering. 

