dynamicStoppingFinal.R

library(MASS)
rm(list = ls())

#setwd("/Users/analytis/Dropbox/Active learning, Selection bias/Final format")
#setwd("/home/mpib/analytis/Datasets")

# The alternative algorithm.

setwd("/Users/pantelispa/Desktop/RepeatedChoice/ReadytoImport")

ExpectedValue <- function(x, s, u) {
  roll <- rnorm(1000000,x,s)
  expected <- mean(roll[roll > u]) - u
  probability <- length(which(roll> u))/1000000
  return(expected*probability)
}

exper <- function(x){return((x-exploit)*dnorm(x,predMean2,standardDeviation))} # the function that calculates the returns from sampling one more alternative.

dataNames <- list.files(path = ".")
p <- 1

for (p in 1:3){
    
     setwd("/Users/pantelispa/Desktop/RepeatedChoice/ReadytoImport")
     theDataset <- read.csv(dataNames[p], header = TRUE, sep = ",")

     # prepare the environment

    k <- 1
    theData <- data.frame(t(matrix(c(rep(0,length(theDataset[,1])*length(theDataset[1,]))),length(theDataset[1,]))))

    while (k <= length(theData[,1])){
        theData[k,] <- theDataset[k,]
        k <-  k + 1}

     # Normalize quality

    theData[,1] <-  (theData[,1] - min(theData[,1])) / (max(theData[,1]) - min(theData[,1]))
    theData2 <- theData

     # Normalize the the attributes for the EW strategy.

    for (k in 1:length(theData2[1,])){
        theData2[,k] <-  (theData2[,k] - min(theData2[,k])) / (max(theData2[,k]) - min(theData2[,k]))}


     # Set up the number of repetitions. 

    repetitions <-  1000

     # Set up the length of search for each dataset. Devide the sample in training and test set.

    theSample <- sample(length(theData[,1]),length(theData[,1])/2)
    trainingSet <- theData[theSample,]
    testSet <- theData[-theSample,]

    # Basic variables 

    maxSearch <- 110
    check <- length(testSet[,1])
    search <- min(check,maxSearch)
    costList <- c(1/2^3,1/2^4,1/2^5,1/2^6,1/2^7,1/2^8,1/2^9,1/2^10)

   # The mlu memories

    mluListDRounds <- list()
    mluListMaxDynamic <- list()
    mluListMemSd <- list()
    mluListExploit <- list()
    mluOutMemory <- list()
    mluMemDRounds <- c(rep(0,repetitions))
    mluMaxDynamic <- c(rep(0,repetitions))
    mluMemSd <- c(rep(0,repetitions)) 

    # the sa memories

    saListDRounds <- list()
    saListMaxDynamic <- list()
    saListMemSd <- list()
    saListExploit <- list()
    saOutMemory <- list()
    saMemDRounds <- c(rep(0,repetitions))
    saMaxDynamic <- c(rep(0,repetitions))
    saMemSd <- c(rep(0,repetitions))

    # the ew memories

    ewListDRounds <- list()
    ewListMaxDynamic <- list()
    ewListMemSd <- list()
    ewListExploit <- list()
    ewOutMemory <- list()
    ewMemDRounds <- c(rep(0,repetitions))
    ewMaxDynamic <- c(rep(0,repetitions))
    ewMemSd <- c(rep(0,repetitions))

   # declare variables

     ########################
     #  Simulation starts   #
     ########################

    for (u in 1:length(costList)){
        
        cost <- costList[u]
        mluMemExploit <- c(rep(0,search))
        saMemExploit <-  c(rep(0,search))
        ewMemExploit <- c(rep(0,search))

        for (m in 1:repetitions){
            
            theSample <- sample(length(theData[,1]),length(theData[,1])/2)
            trainingSet <- theData[theSample,]
            trainingSetEw <- theData2[theSample,]
            testSet <- theData[-theSample,]
            testSetEw <- theData2[-theSample,]  
            
            # counters 

            lr <- 0
            pr <- 1
            ls <- 0
            ps <- 1
            lew <- 0
            pew <- 1

            # memories

            mluMemDynamic <- c(rep(0,search))
            saMemDynamic <- c(rep(0,search))
            ewMemDynamic <-  c(rep(0,search))

            # The Mlu model

            mlu <- lm(X1 ~ ., data = trainingSet)
            mluSd <- summary(mlu)$sigma
            mluPredictions <- predict.lm(mlu, newdata = testSet)
            ordera <- mluPredictions[order(mluPredictions , decreasing = TRUE)]
            mluTheOrder <- theData[as.numeric(names(ordera)),]

            # The SA model

            test <- cor(trainingSet, method = "kendall")
            v <- which(abs(test[1,2:length(trainingSet)]) == max(abs(test[1,2:length(trainingSet)])))
            if(length(v) > 1){v <- sample(v)[1]}
            singleAttribute <- lm(as.formula(paste( "X1 ~ X", v + 1, sep = "")),data = trainingSet)
            saSd <- summary(singleAttribute)$sigma 
            saPredictions <- predict.lm(singleAttribute, newdata = testSet)
            saOrder <- saPredictions[order(saPredictions , decreasing = TRUE)]
            saTheOrder <- theData[as.numeric(names(saOrder)),]

            # The equal weights model.

            V2 <-  EW(trainingSetEw,trainingSetEw)
            trainingSetEw2 <- as.data.frame(cbind(trainingSetEw[,1],V2))
            equalWeighting <- lm(V1 ~ V2, data = trainingSetEw2)
            ewSd <- summary(equalWeighting)$sigma
            V2 <- EW(trainingSetEw,testSetEw)
            testSetEw <- as.data.frame(cbind(testSetEw[,1],V2))
            ewPredictions <- predict.lm(equalWeighting, newdata = testSetEw)
            ewOrder <- ewPredictions[order(ewPredictions , decreasing = TRUE)]
            ewTheOrder <- theData[as.numeric(names(ewOrder)),]

            for (i in 1:search){

                # MLU

                mluExploit <- max(mluTheOrder[1:i,1])
                mluMemExploit[i] <- mluMemExploit[i] + mluExploit - (cost*i)

                # SA

                saExploit <- max(saTheOrder[1:i,1])
                saMemExploit[i] <- saMemExploit[i] + saExploit - (cost*i)

                # EW

                ewExploit <- max(ewTheOrder[1:i,1])
                ewMemExploit[i] <- ewMemExploit[i] + ewExploit - (cost*i)

                 # MLU
                
                if (pr == 1){
                    predMean2 <- ordera[i + 1]
                    standardDeviation <- mluSd
                    exploit <- mluExploit
                    theMluValue <- integrate(exper,exploit,Inf)[[1]]
                    if (is.na(theMluValue) == TRUE){theMluValue <- 0}
                    mluMemDynamic[i] <-mluMemDynamic[i] + mluExploit - (cost*i)
                    if ((theMluValue - cost < 0 ) || (i == length(testSet[,1]) - 1)){pr <- 2}
                    lr <- lr + 1
                }

                # SA
                
                if (ps == 1){
                    predMean2 <- saOrder[i + 1]
                    standardDeviation <- saSd
                    exploit <- saExploit
                    theSaValue <- integrate(exper,exploit,Inf)[[1]]
                    if (is.na(theSaValue) == TRUE){theSaValue <- 0}
                    saMemDynamic[i] <- saMemDynamic[i] + saExploit - (cost*i)
                    if ((theSaValue - cost < 0) ||(i == length(testSet[,1]) - 1)){ps <- 2}
                    ls <- ls + 1
                }

               # EW
                
                if (pew == 1){
                    predMean2 <- ewOrder[i + 1]
                    stadardDeviation <- ewSd
                    exploit <- ewExploit
                    theEwValue <- integrate(exper,exploit,Inf)[[1]]
                    if (is.na(theEwValue) == TRUE){theEwValue <- 0}
                    ewMemDynamic[i] <- ewMemDynamic[i] + ewExploit - (cost*i)
                    if ((theEwValue - cost < 0) || (i == length(testSet[,1]) - 1)){pew <- 2}
                    lew <- lew + 1
                }


            }

           # Save the MLU memories

            mluMemSd[m] <- mluSd
            mluMemDRounds[m] <- lr
            mluMaxDynamic[m] <- mluMemDynamic[lr]
            if (mluMaxDynamic[m] == 0){mluMaxDynamic[m] <- mluMemDynamic[lr - 1]}
            mluOutMemory[[m]] <- mluTheOrder

           # Save the SA memories

            saMemSd[m] <- saSd
            saMemDRounds[m] <- ls
            saMaxDynamic[m] <- saMemDynamic[ls]
            if (saMaxDynamic[m] == 0){saMaxDynamic[m] <- saMemDynamic[ls - 1]}
            saOutMemory[[m]] <- saTheOrder

            # Save the EW memories

            ewMemSd[m] <- ewSd
            ewMemDRounds[m] <- lew
            ewMaxDynamic[m] <- ewMemDynamic[lew]
            if (ewMaxDynamic[m] == 0){ewMaxDynamic[m] <- ewMemDynamic[lew - 1]}
            ewOutMemory[[m]] <- ewTheOrder
        }

        # save the MLU lists

        mluListDRounds[[u]] <- mluMemDRounds
        mluListMaxDynamic[[u]] <- mluMaxDynamic
        mluListMemSd[[u]] <- mluMemSd
        mluListExploit[[u]] <- mluMemExploit

       # Save the SA lists

        saListDRounds[[u]] <- saMemDRounds
        saListMaxDynamic[[u]] <- saMaxDynamic
        saListMemSd[[u]] <- saMemSd
        saListExploit[[u]] <- saMemExploit

       # Save the EW lists

        ewListDRounds[[u]] <- ewMemDRounds
        ewListMaxDynamic[[u]] <- ewMaxDynamic
        ewListMemSd[[u]] <- ewMemSd
        ewListExploit[[u]] <- ewMemExploit
    }


    setwd("/Users/pantelispa/Desktop")
    #setwd("/home/mpib/analytis/Results")
    takeLength <- nchar(dataNames[p])
    theName <- substr(dataNames[p], 1, takeLength - 4)
    saveHere <- paste(theName,"Stopping.Rdata", sep="")
    save.image(file = saveHere)

 }
