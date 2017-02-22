#Function to run windowed on the dataset and save a results table
#run_clustream<- function(nMicro){
library("stream")
library("streamMOA")
library("animation")

sizeInit = 500
#Load in data
x = read.csv("data/stripe.csv", header = F, stringsAsFactors = F)
data = x[ ,1:10]
N = nrow(data)
nDim = ncol(data)
nClust = 4
runs <-  N - sizeInit
batchSize = 100
nMicro = 250
#name = sprintf("single_segment_stripe_k_%i", nClust)

#center and scale (do this in StreamMOA instead?)
data = scale(data, center = T, scale = TRUE)

######## CLUSTREAM
#Set up the datastream
stream <- DSD_Memory(data, k = NA)
clustream <- DSC_CluStream(m = nMicro, k = nClust, h = 1000000)
reset_stream(stream)
update(clustream, stream, sizeInit)

#Start Animation
#saveHTML({
#  ani.options(interval = 0.2, loop = FALSE)
saveGIF({
  ani.options(nmax = 30)
 

  #Streaming
  for (t in (sizeInit+1):runs){
    
    if (t%%batchSize == 0){ print(t)
      update(clustream, stream, batchSize)
      #centers = get_centers(clustream, type = "micro")
      #macro <- kmeans(centers, nClust, nstart = 20)
      
      #For plots 
      #points_col = rep("gray", N)
      #points_col[1:(t+sizeInit)]  = "black"
      #assignment = rep(0, (t+sizeInit))
      #assignment[1:(t+sizeInit)] = get_assignment(clustream, data[1:(t+sizeInit),], type = "macro")
      assignment = as.vector(get_assignment(clustream, data[1:(t+sizeInit),], type = "macro"))
      segment_lines = which(diff(assignment)!=0)[rle(assignment)$lengths>20]
      segment_lines = segment_lines[-c(1, length(segment_lines))]
      
      par(mfrow=c(1,1), las = 1)
      #for(i in c(1:nDim)) {
      i = 1
        plot(1:N, data[ ,i], main= i, type = "l")
        abline(v = (t+sizeInit), col = 4)
        abline(v = segment_lines, col = 2, lwd = 2)
      #}
    }
  }
  
  #END Animation
#img.name = sprintf("%s_", name),  htmlfile = sprintf("%s.html", name))

}, interval = 0.05, movie.name = "bm_demo.gif", ani.width = 600, ani.height = 600)



### Notes about this code
# Remove the sizeInit - this is built into streamMOA
# Change centering/scaling to streamMOA for consistancy
# Source the data read in step
# Function it up?  What is my output?