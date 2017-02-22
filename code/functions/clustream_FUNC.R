do_clustream <- function(newDataPoint,  micro, assignment, nDim, nMicro, clusTime, t){
###############################################################
# This runs Clustream updates one datapoint at a time
# It exports the status of the microclusters, and the microcluster which each current datapoint has been assigned to
# It depends on being to access nDim, nMicro and clusTime, t
###############################################################

  delta = 0.1
  m = 50
  tau = 2
    
x <- newDataPoint
minDist <- Inf # Initialise
mStar <- 0 # Index of closest microCluster to new data point x
  
#### Determine the closest microcluster ####
  for (m in 1:nMicro){
    tempDist <- dist(rbind(micro$CF1x[m, ]/micro$n[m], x))
    if (tempDist < minDist) {minDist <- tempDist; mStar <- m}
  }
  

  MBF <- sqrt(abs((micro$CF2x[mStar,]/micro$n[mStar]) - (micro$CF1x[mStar,]/micro$n[mStar])^2))*tau
  if(micro$n[mStar] == 1 ){MBF <- rep(1, length = nDim)}

    if(sum(abs(x - micro$CF1x[mStar,]/micro$n[mStar]) < MBF) == nDim){
    
    # Update X into m 
    for (j in 1:nDim) {   
      micro$CF1x[mStar,j] <- as.numeric(micro$CF1x[mStar,j] + x[j])
      micro$CF2x[mStar,j] <- as.numeric(micro$CF2x[mStar,j] + (x[j])^2)
    }
    micro$n[mStar,] <- micro$n[mStar,] + 1
    micro$CF1t[mStar,] <- micro$CF1t[mStar,] + clusTime
    micro$CF2t[mStar,] <- micro$CF1t[mStar,] + clusTime^2
    #n_absorbed <- n_absorbed+1
    assignment[t] <- mStar
    
  } else {
    ###Create a new microcluster###
    relevanceStamp <- vector(length = nMicro)
    for (i in 1:nMicro){
      relevanceStamp[i] <- qnorm(m/(2*micro$n[i]), mean = micro$CF1t[i,]/micro$n[i], sd =sqrt((micro$CF2t[i,]/micro$n[i])-(micro$CF1t[i,]/micro$n[i])^2))
    }                      
    
    mDelete <- which(relevanceStamp == min(relevanceStamp, na.rm = T))
    
    if(length(mDelete>1)){mDelete <- mDelete[1]}
    
    if (relevanceStamp[mDelete] < delta){
      relevanceStamp[mDelete]
      #Replace old cluster with new microcluster
      for (j in 1:nDim) {
        micro$CF1x[mDelete,j] <- as.numeric(x[j])
        micro$CF2x[mDelete,j] <- as.numeric((x[j])^2)
      }
      micro$n[mDelete,] <- 1
      micro$CF1t[mDelete,] <- clusTime
      micro$CF2t[mDelete,] <- clusTime^2
      #n_removed <- n_removed + 1
      assignment[which(assignment == mDelete)] <- NA
      assignment[t] <- mDelete
      #removeList[n_removed,] <- c(mDelete,clusTime)
    } else {
      
      ###Combine two clusters to make space
      
      #First find the closest two microcluster
      centers <- as.data.frame(micro$CF1x[1:nMicro,]/micro$n[1:nMicro])
      distMat <- as.matrix(dist(centers))
      distMat[upper.tri(distMat, diag = T)] <- NA
      combineRef <- which(distMat == min(distMat, na.rm = T), arr.ind = T)
      
      
      ###Combine clusters
      for (j in 1:nDim) {
        micro$CF1x[combineRef[1],j] <- micro$CF1x[combineRef[1],j] + micro$CF1x[combineRef[2],j]
        micro$CF2x[combineRef[1],j] <- micro$CF2x[combineRef[1],j] + micro$CF2x[combineRef[2],j]
      }
      micro$n[combineRef[1]] <- micro$n[combineRef[1]] + micro$n[combineRef[2]]
      micro$CF1t[combineRef[1]] <- micro$CF1t[combineRef[1]] + micro$CF1t[combineRef[2]]
      micro$CF2t[combineRef[1]] <- micro$CF2t[combineRef[1]] + micro$CF2t[combineRef[2]]
      
      ###Start new cluster to replace cluster 2
      mDelete <- combineRef[2]
      for (j in 1:nDim) {
        micro$CF1x[mDelete,j] <- as.numeric(x[j])
        micro$CF2x[mDelete,j] <- as.numeric((x[j])^2)
      }
      micro$n[mDelete,] <- 1
      micro$CF1t[mDelete,] <- clusTime
      micro$CF2t[mDelete,] <- clusTime^2
      #n_merged <- n_merged + 1
      assignment[which(assignment == combineRef[2])] <- combineRef[1]
      assignment[t] <- combineRef[2]
      #mergeList[n_merged,] <- c(combineRef[1],combineRef[2],clusTime)
    }
    
  } # End the ELSE Clause

  return(list(micro, assignment))
  
}

