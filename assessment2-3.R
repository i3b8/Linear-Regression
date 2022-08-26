library(tidyverse)
library(HistData)
data("GaltonFamilies")
# set.seed(1) # if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))
library(broom)
f_dau <- galton %>%filter(pair =="father_daughter") 
cor(f_dau$parentHeight,f_dau$childHeight)
fit <- lm(childHeight~parentHeight, data=f_dau)
nrow(f_dau)
m_son <- galton %>%filter(pair =="mother_son") 
fit <- lm(childHeight~parentHeight, data=m_son)
fit
nrow(f_dau)
cor(m_son$parentHeight,m_son$childHeight)
nrow(m_son)


galton %>%
  group_by(pair) %>%
  summarize(n = n())
galton %>%
  group_by(pair) %>%
  summarize(correl= cor(childHeight,parentHeight))
galton %>%
  group_by(pair) %>%
  summarize(cor = cor(parentHeight, childHeight)) %>%
  filter(cor == min(cor))



asba <- galton %>%
  group_by(pair)%>%
  summarize(fit =tidy(lm(childHeight ~ parentHeight, data = galton)))

       


galton %>%
  group_by(pair) %>%
  summarize(tidy(lm(childHeight ~ parentHeight, data = across()), conf.int = TRUE)) %>%
  filter(term == "parentHeight", pair == "mother_son")   

galton %>%
  group_by(pair) %>%
  summarize(tidy(lm(childHeight ~ parentHeight, data = across()), conf.int = TRUE)) %>%
  filter(term == "parentHeight", pair == "mother_daughter")   

galton %>%
  group_by(pair) %>%
  summarize(tidy(lm(childHeight ~ parentHeight, data = across()), conf.int = TRUE)) %>%
  filter(term == "parentHeight", pair == "father_son")   
galton %>%
  group_by(pair) %>%
  summarize(tidy(lm(childHeight ~ parentHeight, data = across()), conf.int = TRUE)) %>%
  filter(term == "parentHeight", pair == "father_daughter")   
