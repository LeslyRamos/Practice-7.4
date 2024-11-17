#Load the necessary libraries
library (psych)
library (tidyverse)
library(gridExtra)
library(car)
library(ggpubr)
library(rstatix)


#First download the data
rairuoho<-read.table('rairuoho.txt', header=T, sep='\t', dec='.')

#Create my own Pearson Correlation Test function
pearson.test <- function(x, y) {
  if(length(x) != length(y)) {
    stop("Vectors x and y must have the same length")
  }
  mean_x <- mean(x)
  mean_y <- mean(y)
  numerator <- sum((x - mean_x) * (y - mean_y))
  denominator <- sqrt(sum((x - mean_x)^2) * sum((y - mean_y)^2))
  r <- numerator / denominator
  n <- length(x)
  df <- n - 2
  t_stat <- r * sqrt(df / (1 - r^2))
  # Calculate the p-value using the pt function (two-tailed)
  p_value <- 2 * pt(-abs(t_stat), df)
  result <- list(
    correlation = r,
    t_statistic = t_stat,
    p_value = p_value,
    df = df
  )
  return(result)
}



## Apply the function to the rairuoho data
# Test correlation for day3 and day4, day3 and day8
result_3_4 <- rairuoho %>% 
  group_by(treatment) %>%
  summarise(test_3_4 = list(pearson.test(day3, day4)))

result_3_8 <- rairuoho %>% 
  group_by(treatment) %>%
  summarise(test_3_8 = list(pearson.test(day3, day8)))

print(result_3_4)
print(result_3_8)


## Show the results
# Extract the correlation, t-statistic, and p-value from the list
result_3_4$correlation <- sapply(result_3_4$test_3_4, function(x) x$correlation)
result_3_4$t_statistic <- sapply(result_3_4$test_3_4, function(x) x$t_statistic)
result_3_4$p_value <- sapply(result_3_4$test_3_4, function(x) x$p_value)

result_3_8$correlation <- sapply(result_3_8$test_3_8, function(x) x$correlation)
result_3_8$t_statistic <- sapply(result_3_8$test_3_8, function(x) x$t_statistic)
result_3_8$p_value <- sapply(result_3_8$test_3_8, function(x) x$p_value)

# Now print the results to check them
print(result_3_4)
print(result_3_8)


## The results for test between day 3 and day 4 
## For both nutrient and water treatment, the correlation is a strong positive correlation. 
## The t-statistic is significant since the values are high
## Since p-value is less than 0.05, the correlation is statistically significant. 

## The results for test between day 3 and day 8
## For both nutrient and water treatment, the correlation is weak
## The t-statistic is low for nutrient, and a little bit higher for water treatment.
## For nutrient treatment, p-value is higher than 0.05 which means that is not statistically significant.
## On the other hand, the water treatment is less than 0.05 which makes it statistically significant


