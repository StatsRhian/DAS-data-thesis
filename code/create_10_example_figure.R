# Plot mutiple DAS signals

library("ggplot2")

source("code/functions/multiplot.R")

x = read.csv("data/stripe.csv", header = F, stringsAsFactors = F)

das <- as.data.frame(x)

# First plot

plot_list = list()
var_list = names(das)

for (i in 1:10) {
  p <- ggplot(das, aes_string(x=1:nrow(das), y = var_list[i])) +
    geom_line() +
    labs(x = "Time", y = sprintf("Series %i", i))
  plot_list[[i]] = p
}


 multiplot(plot_list[[1]], plot_list[[2]],
          plot_list[[3]], plot_list[[4]],
          plot_list[[5]], plot_list[[6]],
          plot_list[[7]], plot_list[[8]],
          plot_list[[9]], plot_list[[10]],cols=2)


