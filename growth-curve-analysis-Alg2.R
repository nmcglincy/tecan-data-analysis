growth.curve.analysis = function(foo) {
# 
# calculating interesting characteristics of the growth curve, fitting a smooth.spline()
# on an hour time-base.
# Requires @param expt.time & @param corrected.abs
  require(grofit)
  spl.fit = smooth.spline(foo$expt.time/3600000, foo$corrected.abs)
  firstDeriv = predict(spl.fit, sort(foo$expt.time/3600000), deriv = 1)
  mu.time = firstDeriv$x[which.max(firstDeriv$y)]
  mu = firstDeriv$y[which.max(firstDeriv$y)]
  mu.abs = spl.fit$y[which.max(firstDeriv$y)]
  norm.growth.rate = mu/mu.abs
# 
# norm.growth.rate is a bit noisy because of the noise in the second derivative
# TODO - find a way to fix it, NI suggests plotting 1st derivative over OD might
# sharpen it. I had though of some sort of windowing....
# 
# These functions are left over from my first attempt: using the maximum of 
# the second derivative of the spline fit to find the point of maximum accelration.
# This proved too sensitive to noise in the lag-phase measurements. The values of
# lambda were highly variable within groups, and didn't seem to reflect my visual
# impression of the curves.
	# secondDeriv = predict(spl.fit, sort(foo$expt.time/3600000), deriv = 2)
	# lambda = secondDeriv$y[which.max(secondDeriv$y)]
	# lambda.time = secondDeriv$x[which.max(secondDeriv$y)]
# 
# new lambda is eqv to lambda.time, defined here as the x-axis intercept of the 
# straight line from the log-phase. This seems to be a bit conservative (there is 
# a clear inflection of the curve lambda - an "acceleration phase"), but is more 
# stable and better reflects my visual impression of the curves.
  lambda = -(spl.fit$y[which.max(firstDeriv$y)] - (mu * mu.time))/mu
  A = max(spl.fit$y)
  AUC = grofit::low.integrate(spl.fit$x, spl.fit$y)
# Making a nice graph
# TODO - a legend would be nice
  png(file = paste(paste(foo$sample, foo$well, sep = "-"), ".png", sep = ""), 
      width = 7, height = 7, units = "in", res = 300)
  plot(foo$expt.time/3600000, 
       foo$corrected.abs, 
       ylim = c(0,1),
       xlab = "Time, hrs", 
       ylab = "Absorbance, A.U.")
  lines(spl.fit, 
        col = "red")
  lines(firstDeriv$x, 
        firstDeriv$y/max(firstDeriv$y), 
        col = "blue")
  points(firstDeriv$x, 
         firstDeriv$y/max(firstDeriv$y), 
         col = "blue")
  abline(v = mu.time, 
         col = "darkgreen", 
         lty = 2)
# 
# Left over from the first method:
#   lines(firstDeriv$x, secondDeriv$y/max(secondDeriv$y), col = "purple")
#   points(firstDeriv$x, secondDeriv$y/max(secondDeriv$y), col = "purple")
#   abline(v = lambda.time, col = "darkgreen", lty = 2)
  abline(h = A, 
         col = "darkgreen", 
         lty = 2)
  abline(a = spl.fit$y[which.max(firstDeriv$y)] - (mu * mu.time) , 
         b = mu)
  abline(v = lambda, 
         col = "orange", 
         lty = 2)
  dev.off()
# 
# A list to hold the results
# TODO - include the fit and derivative objects maybe?
  list(mu =  mu, lambda = lambda, A = A, AUC = AUC, norm.growth.rate = norm.growth.rate)
} 
