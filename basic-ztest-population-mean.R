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

IQ.data.vector <- c(100,103,104,109,109,116,105,108,97)

z.results <- z.test.vector(IQ.data.vector, 100, 7.5)
