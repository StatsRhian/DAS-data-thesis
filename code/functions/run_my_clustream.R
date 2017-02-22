# Running Clustream on the first 6 raw features. 

#Load in functions and settings
invisible(sapply(X = list.files(path = "code/functions", pattern = "*.R$", full.names = TRUE, ignore.case = TRUE), source, .GlobalEnv))
source("code/global_settings.R")
nMicro = 100

#Load in data
x = read.csv("data/minisample.csv", header = T, stringsAsFactors = F)
data = x[,1:6]
N <- nrow(data)
nDim = ncol(data)
nClust = 7
runs <- 100

# Initialisation of microclusters for Clustream
initialClustering <- kmeans(data[1:sizeInit,], nMicro, nstart = 20)
micro <- initialiseMicro(initialClustering$cluster, nMicro, nDim, data)
assignment[1:sizeInit] <- initialClustering$cluster
clusTime <- 1


for(t in (sizeInit+1):(sizeInit+runs)){
  
  clusTime <- clusTime + 1
  output <- do_clustream(data[t, ], micro, assignment, nDim, nMicro, clusTime, t)
  micro <- output[[1]]

  if (t%%batchSize == 0){
    print(t)
    centers <- sweep(micro$CF1x, 1, micro$n, FUN = "/")
    macro <- kmeans(centers, nClust, nstart = 20)
    plot(x[1:t,1], x[1:t,2], xlim = range(x[,1]), ylim = range(x[,2]), col = "gray")
   points(centers[,1],centers[,2], col =macro$cluster, pch = 3, cex = 1.2)
  Sys.sleep(1) 
  }
}
