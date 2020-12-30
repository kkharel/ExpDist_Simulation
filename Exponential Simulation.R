# We have 40 observations with exponential distribution
# of rate 0.2. We simulate 40 observation 1000 times. The 
# mean and standard deviation of exponential distribution is
# 1/lambda. 

library(ggplot2)
library(knitr)
no_sim <- 1000    #No of Simulation
lambda <- 0.2
n <- 40           # Sample Size

sim_data <- matrix(rexp(n = no_sim*n, rate = lambda),no_sim,n)
mean_sample <- rowMeans(sim_data)

# Let's calculate theoretical mean and sample mean and compare
# the difference between these means.
theoretical_mean <- 1/lambda
sample_mean <- mean(mean_sample)

result <-data.frame("Mean"=c(sample_mean,theoretical_mean),row.names = c("Sample mean ","Theoretical mean"))
result
# We can see that these two means are very close.

# Let's plot the histogram for sample means
sample_data <- as.data.frame(mean_sample)
sample_data
ggplot(sample_data, aes(mean_sample))+
  geom_histogram(alpha=.5, position="identity", col="black",binwidth = 1/10)+
  geom_vline(xintercept = theoretical_mean, colour="red",show.legend=TRUE)+
  geom_vline(xintercept = sample_mean, colour="green", show.legend=TRUE)+
  ggtitle ("Histogram of the sample means ")+
  xlab("Sample mean")+ylab("Density")

# Sample variance vs sample mean
sample_variance = var(mean_sample)
theoretical_variance = 1/(lambda^2 * n)
result1 <- data.frame("variance" = c(sample_variance,theoretical_variance),
                                      row.names = c("Sample Variance","Theoretical variance"))
result1

# Central limit theorem states that if we have sufficiently large sample size 
# from a population with finite level of variance, the mean of all the samples
# from the same population will be approximately equal to the mean of the population.(Investopedia)
# In simple words, the mean of samples follow normal distribution.
# Let's see the distribution of sample means and normal distribution.

ggplot(sample_data, aes(mean_sample))+
  geom_histogram(aes(y=..density..), alpha=.5, position="identity", fill="white", col="black",binwidth = 1/10)+
  geom_density(colour="red", size=1)+
  stat_function(fun = dnorm, colour = "green", args = list(mean = theoretical_mean, sd = sqrt(theoretical_variance)))+
  ggtitle ("Sample means & Normal Curve ")+
  xlab("Sample mean")+
  ylab("Density")
# We can see the distribution of sample mean shown in red approximately matches
# the normal distribution which is shown in black circles.

#Another way of looking at it is to draw the Normal probability plot.
qqnorm(mean_sample, main ="Normal probability plot")
qqline(mean_sample,col = "10")

# We can see that sample mean approximately matches the normal probability plot.
