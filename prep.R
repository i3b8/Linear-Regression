library(dslabs)
data(heights)
heights
class(heights)
class(heights$sex)
class(heights$height)
class("Male")
class(75.000)
class(75.55)
nrow(heights)
heights$height[777]
head(heights)
max(heights$height)
min(heights$height)
heights[which.min(heights$height),]
which.min(heights$height)
##
mean(heights$height)
median(heights$height)
##
sum(heights$sex =="Male")/nrow(heights)
mean(heights$sex=="Male")

## taller than 
sum(heights$height >78)

## female taller than 78 
sum(heights$sex=="Female" & heights$height >78)
