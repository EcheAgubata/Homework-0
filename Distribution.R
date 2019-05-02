library(dslabs)
data(heights)
tab <- table(heights$height)
sum(tab == 1)
###Normal Distribution
library(dslabs)
data(heights)
x <- heights$height[heights$sex == "Male"]
mean(x>69 & x<=72)
sd(x>69 & x<=72)
pnorm(x,0.3337438,0.4718401,"TRUE", "FALSE")

##if population of men is an average of 69inches and deviation of 3inches 
##calculate the proportion of men that are 7 foot and above and assign to variable p
##if the population of NBA players (men) within the age of 18 and 40 is 1billion
##calculate the proporation of them that are 7 foot and above
##normal distribution functions is pnorm(limit,mean,deviation)
p <- 1-pnorm(84,69,3)
round(p * 1000000000)


###percentiles
library(dslabs)
#library(gg_plot)
data(heights)
#quantile(heights$height, seq(.01, 0.99, 0.01))
male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]
female_percentiles <- quantile(female, c(0.1, 0.3, 0.5, 0.7, 0.9))
male_percentiles <- quantile(male, c(0.1, 0.3, 0.5, 0.7, 0.9))

df <- data.frame(female = female_percentiles, male = male_percentiles)

df

##How erroneous entries affect mean
x <- Galton$child
error_avg <- function(k){
  #x_with_error <- x
  x[1] <- k
  mean(x)
}

error_avg(10000)
error_avg(-10000)

##group density plots
## add the group argument then a layer with +
library(dslabs)
library(ggplot2)
data(heights)
heights %>% 
  ggplot(aes(height, group = sex, fill = sex, color = sex))+
  geom_density(alpha = 0.1)

