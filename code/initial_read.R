#Read in the data

#Quite slow to load in. Maybe start with a smaller sample and save that as a data set?
x = read.csv("data/sample1.csv", header = T, stringsAsFactors = F)
dim(x)
x <- x[,-1]
#First column indicated the timepoint (data has been thinned) we discard this. 

#PCA plots
sample <- x[1:10000,]
write.table(x = sample, file = "data/minisample.csv", sep = ",", row.names = F, col.names = T)
pca <- prcomp(sample, center = TRUE, scale = TRUE) 

#Plot first 8 principle comopnents of the small smample 

#cor timeseries?