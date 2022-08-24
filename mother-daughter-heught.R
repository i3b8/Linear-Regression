set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

female_heights
mu_mo <- mean(female_heights$mother)
mu_mo
sd_mo <- sd(female_heights$mother)
sd_mo

mu_dau <-mean(female_heights$daughter)
mu_dau

sd_dau <-sd(female_heights$daughter)
sd_dau

rou <- cor(female_heights$mother,female_heights$daughter)
rou

## Regression line : daughter given mother 
m <- rou *sd_dau/sd_mo
m ## slope 
## intercept 
b  <- mu_dau -m*mu_mo
b
## change in daughter if mother increasedc by 1 


## percentage of variability height for daughter explained by mother's height 
percentage_dau <- rou^2 
percentage_dau *100


## Expected value for dau height if mother = 60 
dau_expected <- m*60 +b
dau_expected
