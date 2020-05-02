# Source:  https://www.dummies.com/education/math/statistics/z-testing-r/

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


##### Katie Ann Jager vidoe on Normal distribution
# https://www.youtube.com/watch?v=QkmSUq498iY

mu = 2
stdev = 0.6
x = 1.584

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


