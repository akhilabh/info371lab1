#Akhila Bhamidipati
#INFO 371 WEEK 2 LAB  

#1. variance of normal RVS

#n = 1000

sample_size <- rnorm(1000)

mean_of_sample <- mean(sample_size)

var1 <- mean((sample_size - mean_of_sample)^2)

var2 <- ((mean_of_sample* mean(sample_size^2) - mean((mean_of_sample * sample_size)^2)))

var3 <- var(sample_size)

sd_sample <- sd(sample_size)

sample_t_test <- t.test(sample_size, conf.level = 0.95)

 

hist(sample_size, 
     main="Histogram for Random Sample Size", 
     border="black", 
     col="pink")

##I think the results for this section are interesting, because it shows the amount of difference that there 
# between each type of variance formula. 

#2. Variance of Means:

#n = 4, m = 500

n <- 4
m <- 500 
test1 <- sapply(1:m, function(s) mean(rnorm(n)))
variance  <- var(test1)


#test 2, 100x

a <- 400
b <- 500            

test2 <- sapply(1:b, function(s) mean(rnorm(a)))
variance2 <- var(test2)

# the variance is much smaller than the first test!
# the standard deviation will be smaller due to the large sample size, there will be less chances of variation in the data.
