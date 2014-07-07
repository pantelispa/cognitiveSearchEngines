EW <- function(memory, dataset){
      corTable <- cor(memory)  
      corTable[is.na(corTable)] <- 0  # test for negative correlations. 
      corTable2 <- corTable[2:length(corTable[,1]),1]
      corTable2[corTable2 > 0] <- 0
      corTable2[corTable2 < 0] <- -1 
      values <- abs(corTable2 + t(dataset[2:length(dataset[1,])])) # set the lowest attribute value as highest. 
      values <- t(values)
      final <- rowSums(values)  # Sum all the attributes. 
      return(final)}
