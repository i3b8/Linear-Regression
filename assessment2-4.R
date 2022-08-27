library(Lahman)
library(tidyverse)
fit <- Teams %>% 
  filter(yearID ==1971) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  lm(R ~ BB + HR, data = .)
tidy(fit, conf.int = TRUE)


Teams %>%
  filter(yearID == 1971) %>%
  lm(R ~ BB + HR, data = .) %>%
  tidy() %>%
  filter(term == "HR") %>%
  pull(estimate)


fit <- Teams %>% 
  filter(yearID %in%1961:2018) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  group_by(yearID)%>%
  summarise(tidy(lm(R ~ BB + HR, data = .)))
 

fit %>% print(n=100)


res <- Teams %>%
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  summarize(tidy(lm(R ~ BB + HR, data = across()))) %>%
  ungroup() 

res %>%
  filter(term == "BB") %>%
  ggplot(aes(yearID, estimate)) +
  geom_point() +
  geom_smooth(method = "lm")  



