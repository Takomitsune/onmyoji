library(ggplot2) 



find_p <- function(x){
  if(x<50){
    p1 <- 0.1
  }else if(x<100){
    p1 <- 0.15
  }else if(x<150){
    p1 <- 0.2
  }else if(x<200){
    p1 <- 0.25
  }else if(x<250){
    p1 <- 0.3
  }else if(x<300){
    p1 <- 0.35
  }else if(x<350){
    p1 <- 0.4
  }
  else if(x<400){
    p1 <- 0.5
  }else if(x<450){
    p1 <- 0.6
  }
  else if(x<500){
    p1 <- 0.8
  }else{
    p1 <- 1
  }
  return(p1)
}

set.seed(114514)
N <- 1e5
l <- rep(0,N)
ssr <- rep(0,N)
for(i in 1:N){
  up <- 0
  n <- 0
  while(1){
    p <- find_p(n)
    n <- n + 1
    if(up<3)
      p1 <- 0.0125*2.5
    else
      p1 <- 0.0125
    if(runif(1, max = 1,min = 0) < p1|n==700){
      up <- up + 1
      if(runif(1, max = 1,min = 0) < p|n==700) break
    }
  }
  l[i] <- n
  ssr[i] <- up
}


df <- data.frame(1:N,l,ssr)
mean(df$l)
summary(df)

g <- ggplot(data = df, aes(x=l))+
  stat_ecdf() +
  theme_bw() +
  geom_vline(xintercept = mean(df$l))+
  scale_y_continuous(breaks = seq(0,1,0.1), labels = paste(seq(0,100,10),'%'),minor_breaks = seq(0,1,0.02)) +
  scale_x_continuous(breaks = seq(0,700,50), minor_breaks = seq(0,700,10))+
  labs(x = 'N',
       y = 'N次以内抽到的概率')
g
f <- ggplot(data = df, aes(x=l))+
  geom_histogram(binwidth = 50, center = 25, col = "white")+ 
  geom_vline(xintercept = mean(df$l))+
  scale_x_continuous(breaks = seq(0,700,50), minor_breaks = seq(0,700,10))+
  scale_y_continuous(breaks = seq(0,1e5,1e3), labels = paste(seq(0,1e5,1e3)/1e3, "%")) +
  labs(x = 'N',
       y = '区间内抽到的频率') +
  theme_bw()
f

h <- ggplot(data = df, aes(y=l)) +
  geom_boxplot() +
  geom_hline(yintercept = c(86,351)) +
  scale_y_continuous(breaks = c(0,86,100,200,300,351,400,500,600,700)) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs( y = "抽卡数")

h

h1 <- ggplot(data = df, aes(y=ssr)) +
  geom_boxplot() +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  scale_y_continuous(breaks = 0:20) +
  labs( y = "黑屏数")

h1



ggsave('全图SPcdf_期望228.84.png', g, path = "./plot" )
ggsave('全图SP频率分布.png',f, path = "./plot"  )
ggsave('全图SP箱型图.png',h, path = "./plot"  )
ggsave('全图SP黑屏数箱型图.png',h1, path = "./plot"  )
