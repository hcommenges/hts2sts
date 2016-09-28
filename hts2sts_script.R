
#=======================================================================================
# Convert Household Travel Survey (trips and individuals) into state sequence format
#=======================================================================================


# load packages ----

library("TraMineR")
library("readr")
library("dplyr")

# load functions ----

suppressWarnings(source("hts2sts_fct.R"))
options(scipen = 99999)

# create SPS matrix ----

trpTable <- LoadTrips()
indTable <- LoadIndividuals()
timeReadyTables <- PrepareTimeVariables(X = trpTable, Y = indTable)
trpReady <- timeReadyTables$trp
indReady <- timeReadyTables$ind
spsMatrix <- CreateMatSPS(X = trpReady)

# ready to play ----

tramSeq <- seqdef(data = spsMatrix, informat = "SPS")
seqiplot(tramSeq, border = NA)
seqdplot(tramSeq, border = NA)

