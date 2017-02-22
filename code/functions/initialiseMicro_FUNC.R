###Initialise Cluster Feature Vectors
initialiseMicro <- function(assignment, nMicro, nDim, data){

# Sets up the first microclusters
# Needs to be able to view variables nMicro, nDim and assignment)

micro <- list(CF1x = matrix(0, nMicro, nDim), CF2x = matrix(0, nMicro, nDim), n = matrix(0, nMicro, 1), CF1t = matrix(0, nMicro, 1), CF2t = matrix(0, nMicro, 1))

for (i in 1:nMicro){
  for (j in 1:nDim){
    micro$CF1x[i, j] <- sum(data[which(assignment == i), j])
    micro$CF2x[i, j] <- sum((data[which(assignment == i), j])^2)
  } 
  micro$n[i,] <- length(which(assignment == i))
  micro$CF1t[i,] <- 1
  micro$CF2t[i,] <- 1
}
return(micro)
}