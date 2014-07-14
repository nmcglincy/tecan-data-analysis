source("read-tecan-data2.R")
data = read.tecan.data2("20140627-gcn20arb1del.csv", "20140627-sample-info.csv")
tecanData = data[[1]]
# 
# EDA plotting of raw data 
library(ggplot2)
# ggplot(tecanData, aes(x = expt.time/3600000, y = abs, colour = sample, group = well)) +
#     geom_point() +
#     scale_colour_brewer(palette = "Paired") +
#     scale_x_continuous(name = "Time, hrs",
#                        breaks = seq(from = 0, to = 24, by = 4)) +
#     ggtitle("Raw OD600 readings") +
#     theme(panel.border = element_rect(fill = NA, colour = "black"),
#           axis.title.x = element_text(vjust = 0, size = 14),
#           axis.title.y = element_text(vjust = 0.2, size = 14),
#           axis.text.x = element_text(size=12),
#           axis.text.y  = element_text(size=12),
#           plot.title = element_text(size = 16),
#           legend.text = element_text(size = 12),
#           legend.title = element_text(size = 14))
# ggsave("raw.png", dpi = 400)
# ggplot(tecanData, aes(x = expt.time/3600000, y = abs, colour = sample, group = well)) +
#     geom_line(size = 1.5) +
#     scale_colour_brewer(palette = "Paired") +
#     scale_x_continuous(name = "Time, hrs",
#                        breaks = seq(from = 0, to = 24, by = 4)) +
#     ggtitle("Raw OD600 readings, by well") +
#     theme(panel.border = element_rect(fill = NA, colour = "black"),
#           axis.title.x = element_text(vjust = 0, size = 14),
#           axis.title.y = element_text(vjust = 0.2, size = 14),
#           axis.text.x = element_text(size=12),
#           axis.text.y  = element_text(size=12),
#           plot.title = element_text(size = 16),
#           legend.text = element_text(size = 12),
#           legend.title = element_text(size = 14))
# ggsave("raw-by-well.png", dpi = 400)
# ggplot(tecanData, aes(x = expt.time/3600000, y = abs, colour = sample, group = well)) +
#     geom_line(size = 1.5) +
#     scale_colour_brewer(palette = "Paired") +
#     facet_grid(~ media) +
#     scale_x_continuous(name = "Time, hrs", breaks = seq(from = 0, to = 24, by = 4)) +
#     ggtitle("Raw OD600 readings, faceted by media") +
#     theme(panel.border = element_rect(fill = NA, colour = "black"),
#           axis.title.x = element_text(vjust = 0, size = 14),
#           axis.title.y = element_text(vjust = 0.2, size = 14),
#           axis.text.x = element_text(size=12),
#           axis.text.y  = element_text(size=12),
#           plot.title = element_text(size = 16),
#           legend.text = element_text(size = 12),
#           legend.title = element_text(size = 14),
#           strip.text.x = element_text(size = 12),
#           strip.text.y = element_text(size = 12))
# ggsave("rawLines-facet-media.png", dpi = 400)
tecanData.negative = subset(tecanData, strain == "NONE" )
# ggplot(tecanData.negative, aes(x = expt.time/3600000, y = abs, colour = sample)) +
#     geom_point() +
#     geom_line(aes(group = well)) +
#     scale_x_continuous(name = "Time, hrs", breaks = seq(from = 0, to = 24, by = 4)) +
#     ggtitle("Raw OD600 readings, median only wells") +
#     theme(panel.border = element_rect(fill = NA, colour = "black"),
#           axis.title.x = element_text(vjust = 0, size = 14),
#           axis.title.y = element_text(vjust = 0.2, size = 14),
#           axis.text.x = element_text(size=12),
#           axis.text.y  = element_text(size=12),
#           plot.title = element_text(size = 16),
#           legend.text = element_text(size = 12),
#           legend.title = element_text(size = 14))
# ggsave("raw-neg-wells.png", dpi = 400)
# 
library(plyr)
tecanData.neg.summ = ddply(tecanData.negative,
                          .(well),
                          summarize,
                          well.median = median(abs))
sample.info = data[[2]]
# ls()
# sample.info
tecanData.neg.summ = merge(tecanData.neg.summ,
                           sample.info,
                           by.x = "well",
                           by.y = "well")
media.bkgd = ddply(tecanData.neg.summ,
                    .(media),
                    summarize,
                    media.median = median(well.median))
# media.bkgd
tecanData = tecanData[order(tecanData$media),]
# tecanData
bkgd = c(rep(media.bkgd$media.median[1], length(which(tecanData$media == "AA-SHOCK"))), 
         rep(media.bkgd$media.median[2], length(which(tecanData$media == "YEPD"))))
tecanData = data.frame(tecanData, bkgd)
tecanData$corrected.abs = tecanData$abs - tecanData$bkgd

# head(tecanData)
# ggplot(tecanData, aes(x = expt.time/3600000, y = corrected.abs, colour = sample, group = well)) +
#     geom_point() +
#     scale_colour_brewer(palette = "Paired") +
#     scale_x_continuous(name = "Time, hrs",
#                        breaks = seq(from = 0, to = 24, by = 4)) +
#     ggtitle("Corrected OD600 readings") +
#     theme(panel.border = element_rect(fill = NA, colour = "black"),
#           axis.title.x = element_text(vjust = 0, size = 14),
#           axis.title.y = element_text(vjust = 0.2, size = 14),
#           axis.text.x = element_text(size=12),
#           axis.text.y  = element_text(size=12),
#           plot.title = element_text(size = 16),
#           legend.text = element_text(size = 12),
#           legend.title = element_text(size = 14))
# 
# ggplot(tecanData, aes(x = expt.time/3600000, y = corrected.abs, colour = sample, group = well)) +
#     geom_line(size = 1.5) +
#     scale_colour_brewer(palette = "Paired") +
#     scale_x_continuous(name = "Time, hrs",
#                        breaks = seq(from = 0, to = 24, by = 4)) +
#     ggtitle("Corrected OD600 readings, by well") +
#     theme(panel.border = element_rect(fill = NA, colour = "black"),
#           axis.title.x = element_text(vjust = 0, size = 14),
#           axis.title.y = element_text(vjust = 0.2, size = 14),
#           axis.text.x = element_text(size=12),
#           axis.text.y  = element_text(size=12),
#           plot.title = element_text(size = 16),
#           legend.text = element_text(size = 12),
#           legend.title = element_text(size = 14))
# 
# ggplot(tecanData, aes(x = expt.time/3600000, y = corrected.abs, colour = sample, group = well)) +
#     geom_line(size = 1.5) +
#     scale_colour_brewer(palette = "Paired") +
#     facet_grid(~ media) +
#     scale_x_continuous(name = "Time, hrs", breaks = seq(from = 0, to = 24, by = 4)) +
#     ggtitle("Corrected OD600 readings, faceted by media") +
#     theme(panel.border = element_rect(fill = NA, colour = "black"),
#           axis.title.x = element_text(vjust = 0, size = 14),
#           axis.title.y = element_text(vjust = 0.2, size = 14),
#           axis.text.x = element_text(size=12),
#           axis.text.y  = element_text(size=12),
#           plot.title = element_text(size = 16),
#           legend.text = element_text(size = 12),
#           legend.title = element_text(size = 14),
#           strip.text.x = element_text(size = 12),
#           strip.text.y = element_text(size = 12))

# Growth curve analysis
library(grofit)
head(tecanData)
# 
# writing my own grofit smooth.spline function
# 
# pseudo-code:
# will work quicker on a list by sample
# spline fit
tecanData.l = dlply(tecanData, .(sample, well))
# str(tecanData.l)
# names(tecanData.l)
foo = tecanData.l[[43]]
# foo
# with(foo, plot(expt.time/3600000, corrected.abs))

spl.fit = smooth.spline(foo$expt.time/3600000, foo$corrected.abs)
# plot(foo$expt.time/3600000, foo$corrected.abs)
# lines(spl.fit, col = "red")

firstDeriv = predict(spl.fit, sort(foo$expt.time/3600000), deriv = 1)
# firstDeriv
# plot(foo$expt.time/3600000, foo$corrected.abs)
# lines(spl.fit, col = "red")
# lines(firstDeriv$x, firstDeriv$y/max(firstDeriv$y), col = "blue")

# calculating mu
# Change in abs per hour, because fit was on time/3600000
mu = firstDeriv$y[which.max(firstDeriv$y)]
# mu
mu.time = firstDeriv$x[which.max(firstDeriv$y)]
# mu.time 

# plot(foo$expt.time/3600000, foo$corrected.abs)
# lines(spl.fit, col = "red")
# lines(firstDeriv$x, firstDeriv$y/max(firstDeriv$y), col = "blue")
# points(firstDeriv$x, firstDeriv$y/max(firstDeriv$y), col = "blue")
# abline(v = mu.time, col = "green", lty = 2)

#
# calculating lambda - the time of the lag phase
secondDeriv = predict(spl.fit, sort(foo$expt.time/3600000), deriv = 2)
# secondDeriv
# plot(foo$expt.time/3600000, foo$corrected.abs)
# lines(spl.fit, col = "red")
# lines(firstDeriv$x, firstDeriv$y/max(firstDeriv$y), col = "blue")
# points(firstDeriv$x, firstDeriv$y/max(firstDeriv$y), col = "blue")
# abline(v = mu.time, col = "green", lty = 2)
# lines(firstDeriv$x, secondDeriv$y/max(secondDeriv$y), col = "purple")
# points(firstDeriv$x, secondDeriv$y/max(secondDeriv$y), col = "purple")

lambda = secondDeriv$y[which.max(secondDeriv$y)]
# lambda
lambda.time = secondDeriv$x[which.max(secondDeriv$y)]
# lambda.time

# plot(foo$expt.time/3600000, foo$corrected.abs)
# lines(spl.fit, col = "red")
# lines(firstDeriv$x, firstDeriv$y/max(firstDeriv$y), col = "blue")
# points(firstDeriv$x, firstDeriv$y/max(firstDeriv$y), col = "blue")
# abline(v = mu.time, col = "green", lty = 2)
# lines(firstDeriv$x, secondDeriv$y/max(secondDeriv$y), col = "purple")
# points(firstDeriv$x, secondDeriv$y/max(secondDeriv$y), col = "purple")
# abline(v = lambda.time, col = "green", lty = 2)

# 
# the maximum growth level, A
# str(spl.fit)
A = max(spl.fit$y)
# A

plot(foo$expt.time/3600000, foo$corrected.abs)
lines(spl.fit, col = "red")
lines(firstDeriv$x, firstDeriv$y/max(firstDeriv$y), col = "blue")
points(firstDeriv$x, firstDeriv$y/max(firstDeriv$y), col = "blue")
abline(v = mu.time, col = "darkgreen", lty = 2)
lines(firstDeriv$x, secondDeriv$y/max(secondDeriv$y), col = "purple")
points(firstDeriv$x, secondDeriv$y/max(secondDeriv$y), col = "purple")
abline(v = lambda.time, col = "darkgreen", lty = 2)
abline(h = A, col = "darkgreen", lty = 2)

# 
# the AUC - 
AUC = grofit::low.integrate(spl.fit$x, spl.fit$y)
# AUC

# 
# A function to lapply across the list of results
rm(foo)
rm(spl.fit, firstDeriv, mu, mu.time, secondDeriv, lambda, lambda.time, A, AUC)
ls()
growth.curve.analysis = function(foo) {
# calculating interesting measurements
  require(grofit)
  spl.fit = smooth.spline(foo$expt.time/3600000, foo$corrected.abs)
  firstDeriv = predict(spl.fit, sort(foo$expt.time/3600000), deriv = 1)
  mu = firstDeriv$y[which.max(firstDeriv$y)]
  mu.time = firstDeriv$x[which.max(firstDeriv$y)]
  secondDeriv = predict(spl.fit, sort(foo$expt.time/3600000), deriv = 2)
  lambda = secondDeriv$y[which.max(secondDeriv$y)]
  lambda.time = secondDeriv$x[which.max(secondDeriv$y)]
  A = max(spl.fit$y)
  AUC = grofit::low.integrate(spl.fit$x, spl.fit$y)
# Making a nice graph
  png(file = paste(foo$sample, foo$well, ".png" sep = "-"), width = 7, height = 7, units = "in", res = 300)
  plot(foo$expt.time/3600000, foo$corrected.abs,
       xlab = "Time, hrs", ylab = "Absorbance, A.U.",
       main = paste(foo$sample, foo$well, sep = "."))
  lines(spl.fit, col = "red")
  lines(firstDeriv$x, firstDeriv$y/max(firstDeriv$y), col = "blue")
  points(firstDeriv$x, firstDeriv$y/max(firstDeriv$y), col = "blue")
  abline(v = mu.time, col = "darkgreen", lty = 2)
  lines(firstDeriv$x, secondDeriv$y/max(secondDeriv$y), col = "purple")
  points(firstDeriv$x, secondDeriv$y/max(secondDeriv$y), col = "purple")
  abline(v = lambda.time, col = "darkgreen", lty = 2)
  abline(h = A, col = "darkgreen", lty = 2)
  dev.off()
# A list to hold the results
  list(spl.fit, firstDeriv, mu, mu.time, secondDeriv, lambda, lambda.time, A, AUC)
}

gc.analysis = grow

