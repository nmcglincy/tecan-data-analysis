growth.curve.analysis = function(foo) {
   # calculating interesting measurements
  library(grofit)
  spl.fit = smooth.spline(foo$expt.time/3600000, foo$corrected.abs)
  firstDeriv = predict(spl.fit, sort(foo$expt.time/3600000), deriv = 1)
  mu = firstDeriv$y[which.max(firstDeriv$y)]
  mu.time = firstDeriv$x[which.max(firstDeriv$y)]
#   secondDeriv = predict(spl.fit, sort(foo$expt.time/3600000), deriv = 2)
#   lambda = secondDeriv$y[which.max(secondDeriv$y)]
#   lambda.time = secondDeriv$x[which.max(secondDeriv$y)]
# 
# new lambda is eqv to lambda.time
  lambda = -(spl.fit$y[which.max(firstDeriv$y)] - (mu * mu.time))/mu
  A = max(spl.fit$y)
  AUC = grofit::low.integrate(spl.fit$x, spl.fit$y)
  # Making a nice graph
  # TODO - a legend would be nice
  png(file = paste(paste(foo$sample, foo$well, sep = "-"), ".png", sep = ""), 
      width = 7, height = 7, units = "in", res = 300)
  plot(foo$expt.time/3600000, foo$corrected.abs, ylim = c(0,1),
       xlab = "Time, hrs", ylab = "Absorbance, A.U.")
  lines(spl.fit, col = "red")
  lines(firstDeriv$x, firstDeriv$y/max(firstDeriv$y), col = "blue")
  points(firstDeriv$x, firstDeriv$y/max(firstDeriv$y), col = "blue")
  abline(v = mu.time, col = "darkgreen", lty = 2)
#   lines(firstDeriv$x, secondDeriv$y/max(secondDeriv$y), col = "purple")
#   points(firstDeriv$x, secondDeriv$y/max(secondDeriv$y), col = "purple")
#   abline(v = lambda.time, col = "darkgreen", lty = 2)
  abline(h = A, col = "darkgreen", lty = 2)
  abline(a = spl.fit$y[which.max(firstDeriv$y)] - (mu * mu.time) , b = mu)
  abline(v = lambda, col = "orange")
  dev.off()
  # A list to hold the results, TODO - needs some names, and a more user friendly order
  list(mu =  mu, lambda = lambda, A = A, AUC = AUC)
} 