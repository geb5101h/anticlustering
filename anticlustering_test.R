library(magrittr)
source("anticlustering.R")

# simulate data,
# run algo, plot clusters

R = 3 #number of clusters to choose
n =100 # num obs
#test data
y = rbinom(n,1,.5)
x = (y*matrix(rnorm(2 * n),n,2) + (1-y)*matrix(rnorm(2 * n,6,1),n,2)) %>% 
  as.data.frame 
me = max_entropy(x,R)

plot(
  x[me == 1,],col = 1,pch = 16,xlim = c(x[,1] %>% min,x[,1] %>% max),
  ylim = c(x[,2] %>% min,x[,2] %>% max)
)
for (i in 2:R)
  points(x[me == i,],col = i,pch = 16)

