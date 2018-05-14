# library load
library(ggplot2)
library(tidyverse)
ggplot() + theme_set(theme_bw(base_size = 14, base_family = "HiraKakuProN-W3"))

# set big & reg probability list
list.prob.big <- c(1/297, 1/287, 1/275, 1/262, 1/248, 1/235)
list.prob.reg <- c(1/496, 1/464, 1/431, 1/397, 1/366, 1/336)

# set settei
# settei <- 6

# dausuu
samp_i <- 10000

# make 0 matrix
d <- matrix(0, samp_i * 6, 6)

# sim
for(settei in 1:6){
  # set big & reg probability by settei
  prob.big <- list.prob.big[settei]
  prob.reg <- list.prob.reg[settei]
  print(settei)
  for(i in 1:samp_i){
    big <- sample(x = c(1,0), size = 8000, replace = T, prob = c(prob.big, 1 - prob.big)) %>% 
      sum()
    reg <- sample(x = c(1,0), size = 8000, replace = T, prob = c(prob.reg, 1 - prob.reg)) %>% 
      sum()
    d_i <- c(big, reg, round(1 / (big / 8000)), round(1 / (reg / 8000)), round(1 / ((big + reg) / 8000)), settei)
    d[i + samp_i * (settei - 1), ] <- d_i
  }
}

# set colnames
colnames(d) <- c("big", "reg", "prob.big", "prob.reg", "prob.all", "settei")

d <- d %>% as_tibble

for(j in 1:6){
  dj <-
    d %>%
      filter(settei == j) %>% 
      summarise_each(funs(max , min ,mean) , prob.all)
  print(dj)
  }


ggplot(d, aes(x = big)) +
  geom_density(size=0.5,alpha=0.5)
# 
g1 <- ggplot(d, aes(x = big, y = reg)) + 
  geom_point(aes(colour=settei), size=1, alpha=0.5)　
#描画
g1

