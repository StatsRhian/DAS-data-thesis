#Function to run windowed on the dataset and save a results table
#run_clustream<- function(nMicro){
library("stream")
library("streamMOA")
library("animation")
#library("RColorBrewer")
cols <- rainbow(7)#brewer.pal(7,"rainbow")

# Read in data
  
  #Load in functions and settings
  invisible(sapply(X = list.files(path = "code/functions", pattern = "*.R$", full.names = TRUE, ignore.case = TRUE), source, .GlobalEnv))
  source("code/global_settings.R")
  batchSize = 100
  nMicro = 100
  
  #Load in data
  x = read.csv("data/minisample.csv", header = T, stringsAsFactors = F)
  data = x[,1:6]
  N <- nrow(data)
  nDim = ncol(data)
  nClust = 7
  runs <-  N - sizeInit
  
  #center and scale
  data = scale(data, center = FALSE, scale = TRUE)

#error_online = error_online_weighted = rep(0, nRep)

######## CLUSTREAM
#Set up the datastream
stream <- DSD_Memory(data, k = 1, class = rep(1, nrow(data)))
#Define the clustering algorithm 
clustream <- DSC_CluStream(m = nMicro, k = 1)
#Reset the stream
reset_stream(stream)
#Initialise
update(clustream, stream, sizeInit)


#Start Animation

saveHTML({
  ani.options(interval = 0.5)
  #par(mar = c(3, 3, 2, 0.5), mgp = c(2, 0.5, 0), tcl = -0.3, cex.axis = 0.8,
      #cex.lab = 0.8, cex.main = 1)


#Streaming
for (t in (sizeInit+1):runs){
  #Update Clustream  
  update(clustream, stream, 1)
  
    if (t%%batchSize == 0){ #print(t)
    centers = get_centers(clustream, type = "micro")
    macro <- kmeans(centers, nClust, nstart = 20)

      
      
    #For plots 
    points_col = rep("gray", N)
    points_col[1:(t+sizeInit)]  = "black"
    par(mfrow=c(2,3), las=1)
    for(i in c(1:6)) {
      plot(1:N, data[,i], main=i, col = points_col)
      points(rep((sizeInit+t), nMicro), centers[,i], col = cols[rank(macro$centers[,5])[macro$cluster]], pch = 3)
    }
    
    #par(mfrow=c(2,3), las=1)
    #for(i in c(1:6)) {
    #    plot(1:N, data[,i], main=i, col = points_col)
    #  abline(h = centers[,i], col = cols[rank(macro$centers[,5])[macro$cluster]])
    #}
    }
}

  #END Animation
}, img.name = "micro_crosses_plot", title = "Micro-cluster centers", htmlfile = "micro-cluster-crosses.html",
description = c("Plots the location of micro-cluster centers as the stream progresses. Animation updates every 100 time steps.
 #               100 micro-clusters, 7 macro-clusters, 10000 data points"))

######## Plot options which work


#Abline plots
#par(mfrow=c(2,3), las=1)
#for(i in c(1:6)) {
#  plot(1:N, data[,i], main=i, col = points_col)
#abline(h = centers[,i], col = sorted_macro)
#abline(h = macro$centers[,i], col = cols[order(macro$centers[,5])])
#points(rep(t, nMicro), centers[,i], col = macro$cluster, pch = 3)

#PCA 1-2 plots
#par(mfrow = c(1,1))
#plot(data[1:t,1], data[1:t,2], xlim = range(data[,1]), ylim = range(data[,2]), col = "gray")
#points(centers[,1], centers[,2], col = macro$cluster, pch = 3, cex = 1.2)

#Crosses plots
#par(mfrow=c(2,3), las=1)
#for(i in c(1:6)) {
#  plot(1:N, data[,i], main=i, col = points_col)
#  points(rep(t, nMicro), centers[,i], col = macro$cluster, pch = 3)

# par(mfrow=c(2,3), las=1)
#  for(i in c(1:6)) {
#    plot(1:N, data[,i], main=i, col = points_col)
#    points(1:t,  data[1:t,i], col = macro$cluster[get_assignment(clustream, data[1:t,])])
# }


######## ANIMATE PLOTS NOT WORKING
  
#for (t in (sizeInit+1):runs){
#  update(clustream, stream, 1)
#plot(clustream, method = "pairs", dim = NULL, type = "both")
#}


#Animation not working
#library("animation")
#reset_stream(stream)
#animate_data(stream, n = 10000, horizon = 100,
#                xlim = c(-1.5,1), ylim =c(-1.5,1))
#reset_stream(stream)
#r <- animate_cluster(clustream, stream, horizon = 100, n = 10000, type = "macro",
#                      measure = "purity", plot.args = list(xlim = c(-1, 1), ylim = c(-1, 1)))


#animation::ani.options(interval = .1)
#saveHTML(ani.replay())
