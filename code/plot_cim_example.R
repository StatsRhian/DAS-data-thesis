#
library("changepoint")

# Example of a change in mean at 100 in simulated normal data

set.seed(1)
x=c(rnorm(50,0,1),rnorm(50,6,1),rnorm(50,14,1),rnorm(50,3,1))

clustering = kmeans(cbind(1:200,x),4)
assignment = clustering$cluster
plot(x, col = assignment)
segment_lines = which(diff(assignment)!=0)[rle(assignment)$lengths>20]
abline(v =  segment_lines, col  =2 )

d = as.data.frame(x)

p1 = ggplot(d, aes(x = 1:nrow(d),y = x)) +
  geom_line() +
  labs(x="Time", y = "Signal")
ggsave(filename = "figures/cim_1.pdf", p1)

p2 = ggplot(d, aes(x = 1:nrow(d),y = x)) +
  geom_point(col = assignment)+
  labs(x="Time", y = "Signal")
ggsave(filename = "figures/cim_2.pdf", p2)


p3 = ggplot(d, aes(x = 1:nrow(d),y = x)) +
  geom_line()+
  geom_vline(xintercept = segment_lines, col = 2)+
  labs(x="Time", y = "Signal")
ggsave(filename = "figures/cim_3.pdf", p3)
