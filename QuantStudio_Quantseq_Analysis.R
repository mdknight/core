#script to read in Quantstudio raw data and generate table with cycle #s for amplification
# for Lexogen Quantseq prep. Calculated optimal range to be between 50% & 80% of max with previous
# normalized yields. Overamplification occurs after ~80% cycle. 
# To run, update line 11, 16, 121, and 135. Update lines 89+ if you are not running 2x6 column pools
#
#
# ### IMPORTING DATA----------------
# ### 1. You need to export Multicomponent data ONLY as a .txt from QuantStudio for correct data and formatting!
#        

#setwd to file location. You can move the file to your desktop and work from there, or use the Z:/ location:
setwd("./QuantStudio5 Results/")

# ### 2. You need to update the file name for the qPCR results you are analyzing
#read in file to new df, header 22 rows - skip to get to table
#new dataframe has four columns: Well, Well.Position, Cycle, and SYBR
nqpcr <- read.table("Lexogen_qPCR.txt", header = TRUE, sep="\t", skip=22, stringsAsFactors = FALSE)

#convert data type for SYBR column since it's read in as CHAR due to commas and remove commas
nqpcr$SYBR <- as.numeric(gsub(",", "", as.character(nqpcr$SYBR)))


# ### FORMATTING DATA------------------
# current data table has one row per cycle, per well
# reduce the rows to per well, columns per cycle with SYBR value

library(sqldf)

#get list of Well.Position and create a df with Well.Position and Cycles
cycledf <- data.frame(sqldf('SELECT DISTINCT("Well.Position") from nqpcr'))
#determine number of cycles for qPCR. 45 is default, but may change so confirming is best
cyclecount <- as.integer(sqldf('SELECT COUNT(DISTINCT("Cycle")) from nqpcr'))

#now move SYBR values for each cycle to new column in cycledf
#start with moving SYBR values into vector
vSYBR <- nqpcr[,4]
#create df with values for each well in a row
sybrdf <- as.data.frame(matrix(vSYBR,ncol =cyclecount,byrow = F))
#Add Well position to beginning of df
sybrdf <- cbind(cycledf[,1], sybrdf)
#rename column
colnames(sybrdf)[1]="Well"
#add column for sorting by column
sybrdf$col <- rep(1:12)
#sort by column (default by row for QuantStudio)
sybrdf <- sybrdf[order(sybrdf$col, sybrdf$Well),]
#remove sorting column
sybrdf$col <- NULL

### CALCULATIONS-----------------------

# Begin calculations for determining cycle #

#add column for max value
sybrdf[, "max"] <- apply(sybrdf[, 2:(cyclecount + 1)], 1, max)
#add column for min value
sybrdf[, "min"] <- apply(sybrdf[, 2:(cyclecount + 1)], 1, min)

# determine proper range for available cycles, upper bound 75-80% of max, do not exceed 80% in calculations
uBoundCycle <- ((sybrdf$max-sybrdf$min)*0.75+sybrdf$min)
# lower bound 50%, but may wish to revise! 
lBoundCycle <- ((sybrdf$max-sybrdf$min)*0.50+sybrdf$min)

sybrdf$uBound <- uBoundCycle
sybrdf$lBound <- lBoundCycle

# determine cycles that fall in range for each sample
# make a new matrix with same dimensions that has 1 if cell value is between 

# lboundCycle and uboundCyle, else 0
cycleSums <- lapply(sybrdf[,2:46], function(col) {ifelse(col > lBoundCycle & col < uBoundCycle, 1, 0)})

# in the case of optimizing to avoid overamplification, create matrix with -1 for overamplfication
#cycleSums2 <- lapply(sybrdf[,2:46], function(col) {ifelse(col > uBoundCycle, -1, 0)})

# convert from matrix to dataframe to be able to sum columns
cycleSums <- as.data.frame(cycleSums)
#cycleSums2 <- as.data.frame(cycleSums2)

#add values for both matrices if using overamplification correction
#cycleSumsF <- cycleSums + cycleSums2

### GROUPING DATA--------------------


# split groups/pools, assuming 2 even pools in full plate (6 columns/each)
# each row at this point is one sample. If you have a different number of samples/pool, update the ranges here.
# if you have a number of pools different from two, you will need to adjust the comments to create the 
# appropriate number of pools
pool1<- (cycleSums[1:48,])
#pool1b <- (cycleSumsF[1:48,])
pool2<- (cycleSums[49:96,])
#pool2b <- (cycleSumsF[49:96,])

## remove comments for rows below and adjust indexing if you need a third pool
# pool3<- (cycleSumsF[49:96,])
# pool3b <- (cycleSums[49:96,])

# Sum columns to get cycle counts for each pool
pool1cycle <- colSums(pool1)
pool2cycle <- colSums(pool2)
# pool3cycle <- colSums(pool3)

# Sum column counts for cycle counts with overcycling correction
#pool1bcycle <- colSums(pool1b)
#pool2bcycle <- colSums(pool2b)
# pool3bcycle <- colSums(pool3b)

# Print cycle counts
print(pool1cycle)
print(pool2cycle)

# Print cycle counts with corrections
#print(pool1bcycle)
#print(pool2bcycle)


### REPORTING-----------------

#write SYBR values for each cycle and well to new file
#update this with new file name for your results. This saves as a .csv
write.csv(sybrdf, file="qPCRResults.csv",row.names=FALSE)

#write results of calculation to new file

cycleresults <- rbind.data.frame(pool1cycle)
#cycleresults <- rbind.data.frame(cycleresults[],pool1bcycle)
cycleresults <- rbind.data.frame(cycleresults[],pool2cycle)
#cycleresults <- rbind.data.frame(cycleresults[],pool2bcycle)
# uncomment for third pool
#cycleresults <- rbind.data.frame(cycleresults[],pool3cycle)
#cycleresults <- rbind.data.frame(cycleresults[],pool3bcycle)

colnames(cycleresults) <- colnames(sybrdf[,2:46])

write.csv(cycleresults, file="qPCR_Results.csv",row.names=FALSE)




