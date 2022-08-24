set.seed(1989) #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)
female_heights %>% lm(mother ~ daughter, data = .)
female_heights %>%do(tidy(lm(mother ~ daughter, data = .)))

fit <- lm(mother ~ daughter, data = female_heights)
fit$coef[2]
fit$coef[1]


predicted_height <- predict(fit,se.fit = TRUE)
head(predicted_height)
female_heights$mother



##
predict(fit)[1]
female_heights$mother[1]


library(Lahman)
bat_02 <- Batting %>% filter(yearID >=1999 & yearID <=2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)
  
  

  library(dplyr)


  #summarise_at(vars(-playerID), funs(mean(., na.rm=TRUE)))


#bat_02 <- bat_02 %>%                                        # Specify data frame
  #group_by(playerID) %>%                         # Specify group indicator
 # summarise_at(vars(b),              # Specify column
              # list(name = mean)) 

bat_99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb))
bat_03 <-  Batting %>% filter(yearID ==2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(singles = mean(singles), bb = mean(bb))

bat <- inner_join(bat_03,bat_99_01,by="playerID")
bat
rdat <- bat %>% 
  summarise(singles_r = cor(singles,mean_singles ), bb_r = cor(bb, mean_bb ))
rdat


bat %>% ggplot(aes(mean_singles,singles)) +geom_point()

bat %>% ggplot(aes(mean_bb,bb))+geom_point()


fit <-lm(bb~mean_bb,data =bat)
predict(fit)
