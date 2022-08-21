# create the dataset
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# means and standard deviations
galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))

# scatterplot of father and son heights
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)

# father-son correlation
galton_heights %>% summarize(cor(father, son))


# compute sample correlation
my_sample <- slice_sample(galton_heights, n = 25, replace = TRUE)

R <- my_sample %>% summarize(cor(father, son))
R 
# Monte Carlo simulation to show distribution of sample correlation
B <- 1000
N <- 25
R <- replicate(B, {
  slice_sample(galton_heights, n = N, replace = TRUE) %>% 
    summarize(r=cor(father, son)) %>% .$r
})
data.frame(R) %>% ggplot(aes(R)) + geom_histogram(binwidth = 0.05, color = "black")

# expected value is the population correlation
mean(R)
# standard error is high relative to its size
sd(R)

# QQ-plot to evaluate whether N is large enough
data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))


library(Lahman)
filtered_teams <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(r_p_game = R/G,bats_p_game=AB/G)
cor(filtered_teams$r_p_game,filtered_teams$bats_p_game)
filtered_teams <- filtered_teams %>% mutate(win_per_game=W/G,errors_p_game=E/G)
cor(filtered_teams$win_per_game,filtered_teams$errors_p_game)
cor(filtered_teams$errors_p_game,filtered_teams$win_per_game)

filtered_teams <- filtered_teams %>%mutate(triple_per_g= X3B/G, double_per_ga = X2B/G)
cor(filtered_teams$triple_per_g,filtered_teams$double_per_ga)
