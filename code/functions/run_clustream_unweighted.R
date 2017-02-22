run_clustream_unweighted <- function(dataset, nMicro, nRepeats = 1){

source("code/global_settings.R")

for (currentRepeat in 1:nRepeats){  
#Generate train data
source("code/generate_real_train.R")
runs <- N - sizeInit

assignment <- vector(length = (sizeInit + runs))
performance <- array(NA, dim = c(floor(runs/batchSize),3))

# Initialisation of microclusters for Clustream
initialClustering <- kmeans(data[1:sizeInit,], nMicro, nstart = 20)
micro <- initialiseMicro(initialClustering$cluster, nMicro, nDim, data)
assignment[1:sizeInit] <- initialClustering$cluster
clusTime <- 1

for(t in (sizeInit+1):(sizeInit+runs)){

  clusTime <- clusTime + 1
  output <- do_clustream(data[t, ], micro, assignment, nDim, nMicro, clusTime, t)
  micro <- output[[1]]
  assignment <- output[[2]]
  
  if (t%%batchSize == 0){
    print(t)
    centers <- sweep(micro$CF1x, 1, micro$n, FUN = "/")
    #sp <- spectralClustering_unweighted(centers, nClust, 8)
    
    #Generate test data
    test_data <- data[(t+1):(t+num_test_points),]
    test_trueClusters <- trueClusters[(t+1):(t+num_test_points)]
    
    linked_test <- as.numeric(apply(test_data, 1, FIND_closest_microcluster, centers))
    sp_unsorted <- spectralClustering_unweighted(centers[unique(linked_test),], nClust, 8)
    sp <- rep(NA, nMicro)
    sp[unique(linked_test)] <- sp_unsorted
    
    #labels = LETTERS[1:15];
    #plot(data, col = "grey")
    #text(centers, labels[sp]) # This adds labels to positions (x,y)
    #Sys.sleep(0.2)

    #dir.create(path = sprintf("figures/%s", dataset), showWarnings = FALSE) 
       
    #png(file = sprintf("figures/%s/alternative_%s_%i.png", dataset, dataset, (t-sizeInit)/batchSize), 
     #   bg = "transparent", width = 1600, height = 1600, res = 250)
    #pca <- prcomp(data) 
    #p_cent <- scale(centers, pca$center, pca$scale) %*% pca$rotation  
    #p_test <- scale(test_data, pca$center, pca$scale) %*% pca$rotation
    #par(mar=c(0,0,3,0)+0.1)
    #plot(pca$x[1:t,1], pca$x[1:t,2], col = "grey",
     #    xlab = "", ylab = "", axes = F, main = sprintf("%i",(t-sizeInit)/batchSize),
      #   xlim = range(pca$x[,1]), ylim = range(pca$x[,2]))
    #points(p_test, col = "lightblue", pch = 20, lwd = 3)
    #points(p_cent, col = sp, pch = 3, lwd = 2, cex = 1.5)
    #dev.off()
    
    assignment <- sp[linked_test]
    performance[(t-sizeInit)/batchSize,] <- calc_vmeasure_purity_numClust(assigned = assignment, labels = test_trueClusters)
    }
}

results <- data.frame(purity = performance[ ,1],
                      vmeasure = performance[ ,2],
                      batch_number = 1:nrow(performance),
                      size = rep(nMicro, nrow(performance)),
                      stringsAsFactors = FALSE) 

file_name <- sprintf("clustream_unweighted_alternative_%s_%i_%i_of_%i", dataset, nMicro, currentRepeat, nRepeats)
write.table(results, file = sprintf("results/%s.csv",file_name), sep = ",", row.names = FALSE)
}
}
