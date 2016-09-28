
#=======================================================================================
# Convert Household Travel Survey (trips and individuals) into state sequence format
#=======================================================================================


# GLOBAL FUNCTIONS ----

# round with variable accuracy
RoundAccur <- function(val, acc){
  return(round(val/acc) * acc)
}

# convert integers to character
ConvNumChar <- function(vec){
  vecChar <- ifelse(vec < 10, paste("0", vec, sep = ""), as.character(vec))
  return(vecChar)
}

# add five minutes (overlap) ----
AddLagfive <- function(df){
  lagFive <- difftime(time1 = as.POSIXct(strptime("06-10-01 08:05:00", format = "%y-%m-%d %H:%M:%S")), 
                      time2 = as.POSIXct(strptime("06-10-01 08:00:00", format = "%y-%m-%d %H:%M:%S")))
  
  obsOrder <- as.POSIXct(strptime(c(t(as.matrix(df[, c("STARTCHAR", "ENDCHAR")]))), format = "%y-%m-%d %H:%M:%S"))
  allDuration <- difftime(obsOrder[-length(obsOrder)], obsOrder[-1])
  obsPlusFive <- obsOrder
  obsPlusFive[which(allDuration == 0) + 1] <- obsOrder[which(allDuration == 0) + 1] + lagFive
  matTimes <- matrix(as.character(obsPlusFive), ncol = 2, byrow = TRUE)
  colnames(matTimes) <- c("START", "END")
  return(as_data_frame(matTimes))
}

# check horary consistency (overlap) ----
CheckHorary <- function(df){
  obsOrder <- c(t(as.matrix(df[, c("START", "END")])))
  obsVsOrdered <- obsOrder == sort(obsOrder)
  checkedSeq <- !any(obsVsOrdered == FALSE)
  return(data_frame(KEEPINDIV = checkedSeq))
}

# create SPS sequences
CreateSeqSPS <- function(df){
  matActMode <- as.matrix(df[, c("O_PURPOSE", "MODE")])
  spreadActMode <- c(t(matActMode), df$D_PURPOSE[nrow(df)])
  matTimes <- as.matrix(df[, c("START", "END")])
  spreadTimes <- as.POSIXct(c("2006-10-01 04:00:00", c(t(matTimes)), "2006-10-02 04:00:00"))
  stateDuration <- difftime(spreadTimes[-length(spreadTimes)], spreadTimes[-1], units = "mins")
  stateDurationInt <- abs(as.integer(stateDuration)) / 5
  spsSeq <- paste("(", spreadActMode, ",", stateDurationInt, ")", sep = "")
  return(spsSeq)
}

# reshape SPS sequences
VectorToMatrix <- function(vec, maxlength){
  matVec <- matrix(nrow = 1, ncol = maxlength)
  matVec[, 1:length(vec)] <- vec
  return(matVec)
}


# ALGORITHM FUNCTIONS ----

# load trips
LoadTrips <- function(){
  myData <- file.choose()
  X <- read_csv(myData)
  colnames(X) <- c("NUMIND", "ORI", "DES", "H_START", "H_END", "M_START", "M_END", "O_PURPOSE", "D_PURPOSE", "MODE")
  X$NUMIND <- as.character(X$NUMIND)
  return(X)
}

# load individuals 
LoadIndividuals <- function(){
  myData <- file.choose()
  X <- read_csv(myData)
  colnames(X) <- c("NUMIND", "WEIGHTS")
  X$NUMIND <- as.character(X$NUMIND)
  return(X)
}

# prepare time variables
PrepareTimeVariables <- function(X, Y){ 
  # recode trips day after
  X <- X %>% filter(H_START < 28 & H_END < 28)
  Y <- semi_join(x = Y, y = X, by = "NUMIND")
  X <- X %>% mutate(H_START = ifelse(X$H_START >= 24, X$H_START - 24, X$H_START),
                    H_END = ifelse(X$H_END >= 24, X$END - 24, X$H_END))
  
  # create time variable rounded by 5 minuts
  X$M_STARTround <- RoundAccur(val = X$M_START, acc = 5)
  X$M_ENDround <- RoundAccur(val = X$M_END, acc = 5)
  X$H_START <- ifelse(X$M_STARTround == 60 & X$M_START > 55, X$H_START + 1, X$H_START)
  X$M_START <- ifelse(X$M_STARTround == 60, 0, X$M_STARTround)
  X$H_END <- ifelse(X$M_ENDround == 60 & X$M_END > 55, X$H_END + 1, X$H_END)
  X$M_END <- ifelse(X$M_ENDround == 60, 0, X$M_ENDround)
  X <- X %>% select(1:10)
  
  # change to times format
  X$STARTCHAR <- paste(ConvNumChar(X$H_START), ConvNumChar(X$M_START), "00", sep = ":")
  X$ENDCHAR <- paste(ConvNumChar(X$H_END), ConvNumChar(X$M_END), "00", sep = ":")
  X$STARTCHAR <- ifelse(X$H_START < 4, paste("06-10-02", X$STARTCHAR, sep = " "), paste("06-10-01", X$STARTCHAR, sep = " "))
  X$ENDCHAR <- ifelse(X$H_END < 4, paste("06-10-02", X$ENDCHAR, sep = " "), paste("06-10-01", X$ENDCHAR, sep = " "))
  X$START <- as.POSIXct(strptime(X$STARTCHAR, format = "%y-%m-%d %H:%M:%S"))
  X$END <- as.POSIXct(strptime(X$ENDCHAR, format = "%y-%m-%d %H:%M:%S"))
  X <- X %>% arrange(NUMIND, STARTCHAR)
  
  # rectify trips and activities of 0 minutes: rounding may delete trips or activities (ex.: start at 8:14 / end at 8:16, both rounded at 8:15)
  lagFive <- difftime(time1 = as.POSIXct("2006-10-01 08:05:00"), time2 = as.POSIXct("2006-10-01 08:00:00"))
  X$END <- ifelse(X$START == X$END, X$END + lagFive, X$END)
  X$END <- as.POSIXct(X$END, origin = "1970-01-01")
  X$ENDCHAR <- substr(as.character(X$END), start = 3, stop = 19)
  timeCorr <- X %>% 
    group_by(NUMIND) %>% 
    do(AddLagfive(df = .)) %>% 
    ungroup()
  
  X$START <- as.POSIXct(strptime(substr(timeCorr$START, 3, 19), format = "%y-%m-%d %H:%M:%S"))
  X$END <- as.POSIXct(strptime(substr(timeCorr$END, 3, 19), format = "%y-%m-%d %H:%M:%S"))
  X$END <- ifelse(X$START == X$END, X$END + lagFive, X$END)
  X$END <- as.POSIXct(X$END, origin = "1970-01-01")
  
  # detect and delete horary inconsistency
  horaryIncon <- X %>% 
    group_by(NUMIND) %>% 
    do(CheckHorary(df = .)) %>% 
    ungroup() %>% 
    filter(KEEPINDIV == TRUE)
  
  Y <- semi_join(x = Y, y = horaryIncon, by = "NUMIND")
  X <- semi_join(x = X, y = Y, by = "NUMIND")
  
  return(list(trp = X, ind = Y))
}


# create SPS matrix
CreateMatSPS <- function(X){
  listIndiv <- split(X, f = X$NUMIND) 
  listSeq <- lapply(listIndiv, CreateSeqSPS)
  maxLength <- max(sapply(listSeq, length))
  listMatSps <- lapply(listSeq, VectorToMatrix, maxlength = maxLength)
  matSps <- do.call(what = "rbind", listMatSps)
  return(matSps)
}


