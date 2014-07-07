# This function calculates the score of cross validation results.

judge <- function(theData,predictions){
    numbers <- seq(1,length(theData[,1]),1)
    allPairs <- combn(numbers,2)
    theJudgment1 <- theData[allPairs[1,],1] - theData[allPairs[2,],1]
    theJudgment1[theJudgment1 < 0] <- -1 
    theJudgment1[theJudgment1 >= 0] <- 1 
    theJudgment2 <- as.vector(predictions[allPairs[1,]]) - as.vector(predictions[allPairs[2,]])
    theJudgment2[theJudgment2 < 0] <- -1 
    theJudgment2[theJudgment2 >= 0] <- 1
    theJudge <- theJudgment1 - theJudgment2
    scoreJudge <- sum(theJudge == 0)/length(theJudgment1)
    return(scoreJudge)
}
