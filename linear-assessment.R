library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)

Teams_small <- Teams_small %>%mutate(R_per_game = R/G, hr_p_game =HR/G)
fit <- lm(avg_attendance~R_per_game,data=Teams_small)
fit <-lm(avg_attendance~hr_p_game,data=Teams_small)

fit <- lm(avg_attendance~W,data=Teams_small)
fit <- lm(avg_attendance~yearID,data=Teams_small)
fit  

cor(Teams_small$R_per_game,Teams_small$W)
cor(Teams_small$hr_p_game,Teams_small$W)

set.seed(1)
stratified <- Teams_small%>%
  group_by(W)%>%
  mutate(w_per_10= round(W/10))
stratified %>% filter(w_per_10 ==8)
stratified
  nrow(stratified)
library(dslabs)
  
?admissions
admissions
