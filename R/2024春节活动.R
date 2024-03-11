library(ggplot2) 
library(dplyr)

rm(list = ls())

roll_newyear<- function(){
  n <- 0
  x <- 0
  rare <- 0
  flg <- 0
  while(1){
    n <- n + 1
    x <- x + 1
    if(runif(1) < 0.0125 | x==60 | n==450){
      x <- 0
      rare <- rare + 1
      if(rare >= 1){
        if(runif(1) < p_sp(n) | n==450){
          flg <- 1
        }
      }
      if(n>=100 & flg == 1){
        return(c(n,rare))
      }
    }
  }
}

roll_sp<- function(){
  n <- 0
  x <- 0
  rare <- 0
  while(1){
    n <- n + 1
    x <- x + 1
    if(runif(1) < 0.0125 | x==60 | n==450){
      x <- 0
      rare <- rare + 1
      if(runif(1) < p_sp(n) | n==450){
        return(c(n,rare))
      }
    }
  }
}

roll_ssr<- function(){
  n <- 0
  x <- 0
  rare <- 0
  while(1){
    n <- n + 1
    x <- x + 1
    if(runif(1) < 0.0125 | x==60 | n==450){
      x <- 0
      rare <- rare + 1
      if(runif(1) < p_ssr(n) | n==450){
        return(c(n,rare))
      }
    }
  }
}

p_sp <- function(n){
  if(n <= 60){
    return(0.10)
  }else if(n <= 120){
    return(0.12)
  }else if(n <= 180){
    return(0.14)
  }else if(n <= 240){
    return(0.18)
  }else if(n < 300){
    return(0.25)
  }else if(n <= 360){
    return(0.40)
  }else if(n <= 420){
    return(0.55)
  }else if(n <= 450){
    return(0.8)
  }else{
    return(1)
  }
}
p_ssr <- function(n){
  if(n <= 60){
    return(0.15)
  }else if(n <= 120){
    return(0.17)
  }else if(n <= 180){
    return(0.19)
  }else if(n <= 240){
    return(0.25)
  }else if(n <= 300){
    return(0.35)
  }else if(n <= 360){
    return(0.45)
  }else if(n <= 420){
    return(0.6)
  }else if(n <= 450){
    return(0.8)
  }else{
    return(1)
  }
}

set.seed(114514)
N <- 1e6
l <- rep(0,N)
for(i in 1:N){
  temp <- roll_newyear()
  l[i] <- temp[1]
}

df_newyear <- data.frame(l = l-100, type = "新年抽卡(减去前100抽)")

set.seed(114514)
N <- 1e6
l <- rep(0,N)
for(i in 1:N){
  temp <- roll_ssr()
  l[i] <- temp[1]
}
df_ssr <- data.frame(l = l, type = "下次新式神是SSR")

set.seed(114514)
N <- 1e6
l <- rep(0,N)
for(i in 1:N){
  temp <- roll_sp()
  l[i] <- temp[1]
}
df_sp <- data.frame(l = l, type = "下次新式神是SP")


df <- rbind(df_newyear, df_ssr) %>% 
  rbind(df_sp)

df <- rbind(df1,df2)
df.s <- df %>% 
  group_by(type) %>% 
  summarise(ex = mean(l),
            qt25 = quantile(l, 0.25),
            qt75 = quantile(l, 0.75))


ggplot(data = df, aes(x = l, col = type))+
  stat_ecdf() +
  theme_bw()+
  geom_vline(data = df.s, aes(xintercept = ex, col = type)) +
  geom_vline(data = df.s, aes(xintercept = qt25, col = type), linetype = "dashed") +
  geom_vline(data = df.s, aes(xintercept = qt75, col = type), linetype = "dashed") +
  geom_hline(yintercept = c(0.25,0.75), linetype = "dashed") +
  scale_y_continuous(breaks = seq(0,1,0.1), labels = paste(seq(0,100,10),'%'),minor_breaks = seq(0,1,0.05)) +
  scale_x_continuous(breaks = seq(0,600,50), minor_breaks = seq(0,600,10))+
  labs(x = 'N',
       y = 'N次以内抽到下期式神的概率（有全图加成）',
       col = "")
ggsave('新年抽卡全图cdf.png',  path = "./plot" )
