#
library("ggplot2")

# Example of a change in mean at 100 in simulated normal data

set.seed(1)
x=c(rnorm(50,0,1),rnorm(50,12,1),rnorm(50,20,1),rnorm(50,6,1))

clustering = kmeans(x,4, nstart = 20)
assignment = clustering$cluster
plot(x, col = assignment)
segment_lines = which(diff(assignment)!=0)[rle(assignment)$lengths>20]
abline(v =  segment_lines, col  = 2 )

d = as.data.frame(x)

p1 = ggplot(d, aes(x = 1:nrow(d),y = x)) +
  geom_line() +
  labs(x="Time", y = "Signal")

p2 = ggplot(d, aes(x = 1:nrow(d),y = x)) +
  geom_point(col = assignment)+
  labs(x="Time", y = "Signal")

p3 = ggplot(d, aes(x = 1:nrow(d),y = x)) +
  geom_line()+
  geom_vline(xintercept = segment_lines, col = 2)+
  labs(x="Time", y = "Signal")


d[70:71,1] <- c(19,20)
clustering = kmeans(d,4, nstart = 20)
assignment = clustering$cluster
segment_lines = which(diff(assignment)!=0)[rle(assignment)$lengths>20]
p5 = ggplot(d, aes(x = 1:nrow(d),y = x)) +
  geom_point(col = ass)+
  geom_vline(xintercept = segment_lines, col = 2)+
  labs(x="Time", y = "Signal")

ggsave(filename = "figures/cim_1.pdf", p1)
ggsave(filename = "figures/cim_2.pdf", p2)
ggsave(filename = "figures/cim_3.pdf", p3)
ggsave(filename = "figures/cim_4.pdf", p4)
ggsave(filename = "figures/cim_5.pdf", p5)
