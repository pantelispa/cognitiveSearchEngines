Correlations.R

setwd("/Users/pantelispa/Desktop/Datasets/RepeatedChoice/ReadytoImport")
# setwd("/Users/pantelispa/Desktop/Datasets/RepeatedChoice/finalFormat")

saveResults <- matrix(nrow = 12, ncol = 4, 0)
dataNames <- list.files(path = ".")

for (p in 1:12){
theDataset <- read.csv(dataNames[p], header = TRUE, sep = ",")

k <- 1
theData <- data.frame(t(matrix(c(rep(0,length(theDataset[,1])*length(theDataset[1,]))),length(theDataset[1,]))))

 while (k <= length(theData[,1])){
 theData[k,] <- theDataset[k,]
 k <-  k + 1}

theData[,1] <-  (theData[,1] - min(theData[,1])) / (max(theData[,1]) - min(theData[,1]))

themodel <- lm(X1 ~., data = theData)
theR <- summary(themodel)[8][[1]]

cor(theData)
themax <- max(abs(cor(theData)[1,2:length(theData[1,])]))
themean <-  mean(abs(cor(theData)[1,2:length(theData[1,])]))
correlationsb <- cor(theData)
correlations2b <- correlationsb[lower.tri(correlationsb,diag=FALSE)] # have to make it
correlations2b <- abs(correlations2b)
inter <- mean((correlations2b))

saveResults[p,] <- c(theR,themax,themean,inter)}






whiteBig <- c(theR,themax,themean,inter)
redBig <-  c(theR,themax,themean,inter)
load("SimulationBolts.Rdata")


