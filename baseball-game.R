

library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()
##Relation bw HRs(home runs and wins )
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

## Relation bw stolen bases and Wins 
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

## Relation bw bases on balls and runs 
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

## Relation bw bats per game and runs 
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

?Teams
## To know that SOA variable is strikeouts by pitchers 
## Y vs X
Teams %>% filter(yearID %in% 1961:2001 ) %>% mutate(W_per_game = W/G, Err_per_game = E/G) %>%
  ggplot(aes(W_per_game,Err_per_game))+
  geom_point(alpha=0.5)



Teams %>% filter(yearID %in% 1961:2001 )%>%
  mutate(triple_per_g= X3B/G, double_per_ga = X2B/G) %>%
  ggplot(aes(double_per_ga,triple_per_g))+
  geom_point(alpha=0.5)
