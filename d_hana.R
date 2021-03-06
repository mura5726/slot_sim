# library load
library(ggplot2)
library(dplyr)
ggplot() + theme_set(theme_bw(base_size = 14, base_family = "HiraKakuProN-W3"))

# set big & reg probability list
list.prob.big <- c(1/297, 1/287, 1/275, 1/262, 1/248, 1/235)
list.prob.reg <- c(1/496, 1/464, 1/431, 1/397, 1/366, 1/336)

# set settei
# settei <- 6

# dausuu
samp_i <- 100000

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

# convert to tibble
d <- d %>% as_tibble

for(j in 1:6){
  dj <-
    d %>%
    filter(settei == j) %>% 
    summarise(max = max(prob.all), min = min(prob.all), mean = mean(prob.all))
  print(dj)
}

d %>% 
  filter(settei == 6) %>% 
  arrange(prob.big)

# make settei charactor
d <- d %>% mutate(settei = as.character(settei))

# plot
ggplot(d, aes(x = prob.all)) +
  geom_density(aes(fill=settei), size=0.3, alpha=0.5)

ggplot(d, aes(x = big, y = reg)) + 
  geom_point(aes(colour=settei), size=0.3, alpha=0.5)　
