# Source:  https://www.dummies.com/education/math/statistics/z-testing-r/

#The distance from the sample mean to the population mean in units 
#of the standard error

z.test.one.sample = function(
    sample.mean, 
    sample.size, 
    popmu, 
    popstdev, 
    alpha.level) {
  
  # sample.mean is your sample mean
  # sample.size is the number of values in your sample mean
  # popmu is the population mean
  # popvar is the population standard deviation
  
  one.tail.p <- NULL
  
  #compute the z-score
  
  stderror.of.pop.mean <- popstdev/sqrt(sample.size)
  z.score <- (sample.mean-popmu)/stderror.of.pop.mean
  
  one.tail.p <- round(pnorm(abs(z.score),lower.tail = FALSE),5)
  
  number.tails <- 2
  
  x.percentile.lower <- qnorm((1-(1-alpha.level))/number.tails, mean=0, sd=1)
  x.percentile.upper <- x.percentile.lower*-1

  z.probability <- pnorm(z.score)*number.tails
  
  x.percentile.lower <- qnorm((alpha.value/number.tails), mean=0, sd=1)
  
  
  z.confidence_interval_upper = sample.mean + x.percentile.lower*stderror.of.pop.mean
  z.confidence_interval_lower = sample.mean - x.percentile.lower*stderror.of.pop.mean
  
  return (list("z" = z.score, 
               "z score probability" = z.probability,
               "stderrorOfPopMean" = stderror.of.pop.mean,
               "conf int upper limit" =  z.confidence_interval_upper,
               "conf int lower limit" = z.confidence_interval_lower,
               "x.percentile.lower" = x.percentile.lower,
               "x.percentile.upper" = x.percentile.upper))
  
}

z.test.vector = function(x, popmu, popstdev) {
  # x is your data set/observations
  # popmu is the population mean
  # popvar is the population standard deviation
  
  one.tail.p <- NULL
  
  #compute the z-score
  
  stderror.of.pop.mean <- popstdev/sqrt(length(x))
  z.score <- round((mean(x)-popmu)/stderror.of.pop.mean, 3)
  
  one.tail.p <- round(pnorm(abs(z.score),lower.tail = FALSE),5)
  
  #cat("z score:", z.score, "\n")
  #cat("one-tailed probability:", one.tail.p, "\n")
  #cat("two-tailed probability:", 2*one.tail.p, "\n")
  
  return (list("z" = z.score, 
               "stderrorOfPopMean" = stderror.of.pop.mean,
               "one.tail.probability" = one.tail.p,
               "two.tail.probability" = 2*one.tail.p))
  
}

IQ.data.vector <- c(100,103,104,109,109,88, 103, 155, 119, 103, 116,105,108,97)

z.results <- z.test.vector(IQ.data.vector, 100, 15)




#######################################################################
##### Katie Ann Jager vidoe on Normal distribution
# https://www.youtube.com/watch?v=QkmSUq498iY

mu = 2
stdev = 0.6
x = 1.5

#Draw the normal distribution, with x axis
#marked off in standard deviations.
#graph centered at the population mean of 2

range = seq(mu-4*stdev, mu+4*stdev, 0.01)

#density function to generate range for x values
y = dnorm(x=range, mean = mu, sd = stdev)

mainTitle = paste("Normal Distribution with mu =",
                  mu, "and population st. dev. = ",
                  stdev,
                  sep=" ")

plot(range, 
     y, 
     xlab="Each X-axis tic is one standard deviation",
     main = mainTitle, type='l', ylim = c(0, max(y)+0.01), axes = FALSE)
#generate axes as you wish
axis(1, at=seq(mu-3*stdev, mu+3*stdev, stdev))
axis(2)

#CREATES A SECOND POLYGON that fits within the curve just drawn
#finding the X value in the curve and coloring area to the left of X
#combine values zero, a sequence from the lowest range value through x
#in increments of 0.01
cord.a = c(0, seq(min(range),x,0.01),                   x)
cord.b = c(0, dnorm(seq(min(range),x,0.01), mu, stdev), 0)
polygon(cord.a, cord.b, col="blue")

cord.c = c(x, seq(x, max(range), 0.01),                   max(range))
cord.d = c(0, dnorm(seq(x, max(range), 0.01), mu, stdev), 0)
polygon(cord.c, cord.d, col=grey(0.90))

p.left.of.x <- pnorm(x, mu, stdev)
p.right.of.x <- 1-pnorm(x, mu, stdev)

#################################
#WIKIPEDIA EXAMPLE: https://en.wikipedia.org/wiki/Z-test


#Suppose that in a particular geographic region, the mean and standard 
#deviation of scores on a reading test are 100 points, and 12 points, 
#respectively. Our interest is in the scores of 55 students in a particular 
#school who received a mean score of 96. We can ask whether this mean score 
#is significantly lower than the regional mean—that is, are the students 
#in this school comparable to a simple random sample of 55 students 
#from the region as a whole, or are their scores surprisingly low?

alpha.value <- 0.05
number.tails <- 2
popmu = 100
popstdev=12

x.percentile.lower <- qnorm((alpha.value/number.tails), mean=0, sd=1)
x.percentile.upper <- qnorm(1-(alpha.value/number.tails), mean=0, sd=1)

z.results <- z.test.one.sample(sample.mean=96, 
                               sample.size=55, 
                               popmu=popmu, 
                               popstdev=popstdev,
                               alpha.level = alpha.value)

cat ("The sample mean is ", z.results$z, " standard error units from ",
       "the population mean of ", popmu)


builtin.z.test <- z.test()

z.results <- z.test.one.sample(sample.mean=346, 
                               sample.size=50, 
                               popmu=popmu, 
                               popstdev=108,
                               alpha.level = alpha.value)

cat ("The sample mean is ", z.results$z, " standard error units from ",
     "the population mean of ", popmu)

popmu=191
z.results <- z.test.one.sample(sample.mean=197.1, 
                               sample.size=100, 
                               popmu=popmu, 
                               popstdev=25.6,
                               alpha.level = alpha.value)

cat ("The sample mean is ", z.results$z, " standard error units from ",
     "the population mean of ", popmu)



