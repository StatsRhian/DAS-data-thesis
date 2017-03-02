#Function to run windowed on the dataset and save a results table
#run_clustream<- function(nMicro){
library("stream")
library("streamMOA")
#library("animation")
library("ggplot2")

#Load in data
#x = read.csv("data/stripe.csv", header = F, stringsAsFactors = F)
x = read.csv("data/minisample.csv", stringsAsFactors = F)
data = x[ ,1:6]
N = nrow(data)
nDim = ncol(data)
nClust = 4
runs <-  N
batchSize = 1000
nMicro = 250
plot_list = list()

data = as.data.frame(scale(data, center = T, scale = TRUE))

######## CLUSTREAM
#Set up the datastream
stream <- DSD_Memory(data, k = NA)
#stream <- DSD_ScaleStream(stream, center = TRUE, scale = TRUE)
clustream <- DSC_CluStream(m = nMicro, k = nClust, h = 1000000)
reset_stream(stream)

  #Streaming
  for (t in 1:runs){
    
    if (t%%batchSize == 0){ print(t)
      update(clustream, stream, batchSize)
      assignment = as.vector(get_assignment(clustream, data[1:t,], type = "macro"))
      segment_lines = which(diff(assignment)!=0)[rle(assignment)$lengths>20]
      segment_lines = segment_lines[-c(1, length(segment_lines))]
      
      
      p =  ggplot(data, aes_string(x = 1:nrow(data), y = names(data)[1])) +
        geom_line() +
        labs(x = "Time", y = "Series 1")+
        geom_vline(xintercept = segment_lines, col = 2) +
        geom_vline(xintercept = t, col = 3)
      plot_list[[t/batchSize]] = p
    }
  }


for (i in 1:6){
  ggsave(filename = sprintf("figures/6_790_channels_time_%i.pdf", i), plot = plot_list[[i]])
}


end_plot_list = list()

for (i in 1:6){
  p = ggplot(data, aes_string(x = 1:nrow(data), y = names(data)[i])) +
    geom_line() +
    labs(x = "Time", y = sprintf("Series %i",i)) +
    geom_vline(xintercept = segment_lines, col = 2) 
    end_plot_list[[i]] = p
}

source("code/functions/multiplot.R")
multiplot(end_plot_list[[1]], end_plot_list[[2]],
          end_plot_list[[3]], end_plot_list[[4]],
          end_plot_list[[5]], end_plot_list[[6]], cols=3)


  