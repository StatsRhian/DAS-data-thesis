#Function to run windowed on the dataset and save a results table
#run_clustream<- function(nMicro){
library("stream")
library("streamMOA")
library("animation")



# Read in data

#Load in functions and settings
#invisible(sapply(X = list.files(path = "code/functions", pattern = "*.R$", full.names = TRUE, ignore.case = TRUE), source, .GlobalEnv))
source("code/global_settings.R")
batchSize = 100
nMicro = 250

#Load in data
#x = read.csv("data/minisample.csv", header = T, stringsAsFactors = F)
x = read.csv("data/stripe.csv", header = F, stringsAsFactors = F)
data = x[,1:6]
N = nrow(data)
nDim = ncol(data)
nClust = 4
runs <-  N - sizeInit

name = sprintf("nice_stripe_k_%i_single", nClust)

#center and scale (do this in StreamMOA instead?)
data = scale(data, center = T, scale = TRUE)


######## CLUSTREAM
#Set up the datastream
stream <- DSD_Memory(data, k = NA)
#Define the clustering algorithm 
clustream <- DSC_CluStream(m = nMicro, k = nClust, h = 1000000)
#Reset the stream
reset_stream(stream)
#Initialise
update(clustream, stream, sizeInit)

#Start Animation
saveHTML({
  ani.options(interval = 0.2, loop = FALSE)

  
  #Streaming
  for (t in (sizeInit+1):runs){

    if (t%%batchSize == 0){ #print(t)
      update(clustream, stream, batchSize)
      #centers = get_centers(clustream, type = "micro")
      #macro <- kmeans(centers, nClust, nstart = 20)
      
      #For plots 
      points_col = rep("gray", N)
      points_col[1:(t+sizeInit)]  = "black"
      assignment = rep(0, N)
      assignment[1:(t+sizeInit)] = get_assignment(clustream, data[1:(t+sizeInit),], type = "macro")
      par(mfrow=c(1,1), las=1)
      #for(i in c(1:6)) {
      i=1
        plot(1:N, data[ ,i], main=i, col = points_col)
        abline(v = which(diff(assignment)>0), col = 2)
        #abline(h = centers[,i], col = cols[rank(macro$centers[,5])[macro$cluster]])
        #points(rep((sizeInit+t), nMicro), centers[,i], col = cols[rank(macro$centers[,5])[macro$cluster]], pch = 3)
       #points(rep((sizeInit+t), nMicro), centers[,i], col = cols[rank(macro$centers[,5])[macro$cluster]], pch = 3)
    #  }
    }
  }
  
  #END Animation
}, img.name = sprintf("%s_", name),  htmlfile = sprintf("%s.html", name))




### Notes about this code
# Remove the sizeInit - this is built into streamMOA
# Change centering/scaling to streamMOA for consistancy
# Source the data read in step
# Function it up?  What is my output?