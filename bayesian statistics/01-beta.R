# 90% positive of 10 ratings
o1 <- 9
o0 <- 1
M <- 100
N <- 100000

m <- sapply(0:M/M,function(prob)rbinom(N,o1+o0,prob))
v <- colSums(m==o1)
df_sim1 <- data.frame(p=rep(0:M/M,v))
df_beta1 <- data.frame(p=0:M/M, y=dbeta(0:M/M,o1+1,o0+1))

# 80% positive of 500 ratings
o1 <- 400
o0 <- 100
M <- 100
N <- 100000

m <- sapply(0:M/M,function(prob)rbinom(N,o1+o0,prob))
v <- colSums(m==o1)
df_sim2 <- data.frame(p=rep(0:M/M,v))
df_beta2 <- data.frame(p=0:M/M, y=dbeta(0:M/M,o1+1,o0+1))

ggplot(data=df_sim1,aes(p)) +
  scale_x_continuous(breaks=0:10/10) +
  
  geom_histogram(aes(y=..density..,fill=..density..),
                 binwidth=0.01, origin=-.005, colour=I("gray")) +
  geom_line(data=df_beta1 ,aes(p,y),colour=I("red"),size=2,alpha=.5) +
  
  geom_histogram(data=df_sim2, aes(y=..density..,fill=..density..),
                 binwidth=0.01, origin=-.005, colour=I("gray")) +
  geom_line(data=df_beta2,aes(p,y),colour=I("orange"),size=2,alpha=.5)
