source("read-tecan-data2.R")
data = read.tecan.data2("20140627-gcn20arb1del.csv", "20140715-sample-info.csv")
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
# Background correction
tecanData = subset(tecanData, strain != "NONE")
library(plyr)
tecanData.neg.summ = ddply(tecanData.negative,
                          .(well),
                          summarize,
                          well.median = median(abs))
sample.info = data[[2]]
tecanData.neg.summ = merge(tecanData.neg.summ,
                           sample.info,
                           by.x = "well",
                           by.y = "well")
media.bkgd = ddply(tecanData.neg.summ,
                    .(media),
                    summarize,
                    media.median = median(well.median))
tecanData = tecanData[order(tecanData$media),]
bkgd = c(rep(media.bkgd$media.median[1], length(which(tecanData$media == "AA-SHOCK"))), 
         rep(media.bkgd$media.median[2], length(which(tecanData$media == "YEPD"))))
tecanData = data.frame(tecanData, bkgd)
tecanData$corrected.abs = tecanData$abs - tecanData$bkgd
# 
# EDA plots of the background corrected data - didn't save these ones, just for visual confirmation
# that the general trends survived the background correction.
library(RColorBrewer)
# ggplot(tecanData, aes(x = expt.time/3600000, y = corrected.abs, colour = sample, group = well)) +
#     geom_point() +
#     scale_colour_manual(values = brewer.pal(8, "Paired")) +
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
#     scale_colour_manual(values = brewer.pal(8, "Paired")) +
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
#     scale_colour_manual(values = brewer.pal(8, "Paired")) +
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
# 
# Growth curve analysis - something has gone wrong here - need to fix
source("growth-curve-analysis-Alg2.R")
tecanData.l = dlply(tecanData, .(sample, well))
gc.analysis = lapply(tecanData.l, growth.curve.analysis)
gc.analysis.df = ldply(lapply(gc.analysis, as.data.frame))
sample.info$sample.well = paste(sample.info$sample, sample.info$well, sep = ".")
gc.analysis.df = merge(sample.info, gc.analysis.df, by.x = "sample.well", by.y = ".id")
# 
# need to be careful of absolute reference here
gc.analysis.dfl = melt(gc.analysis.df, 
                      id.vars = names(gc.analysis.df)[1:9],
                      measure.vars = names(gc.analysis.df)[10:length(names(gc.analysis.df))],
                      value.name = "measurement")
gc.analysis.dfl = subset(gc.analysis.dfl, strain != "NONE")
# 
# Some EDA
# reorder factors - might be better to do this earlier
# gc.analysis.dfl$sample = factor(gc.analysis.dfl$sample, levels = rev(unique(gc.analysis.dfl$sample)))
# ggplot(gc.analysis.dfl, aes(x = interaction(gcn20.genotype, arb1.genotype), y = measurement, 
#                             fill = interaction(gcn20.genotype, arb1.genotype), shape = media)) +
#   geom_point(position = position_jitter(w = 0.25), size = 3, pch = 21, colur = "black") +
#   facet_wrap(~ variable, scales = "free_y") +
#   scale_fill_manual(values = rev(brewer.pal(8, "Paired"))) +
#   theme(panel.border = element_rect(fill = NA, colour = "black"),
#         axis.title.x = element_text(vjust = 0, size = 14),
#         axis.title.y = element_text(vjust = 0.4, size = 14),
#         axis.text.x = element_text(size=11, angle = 90, hjust = 1, vjust = 0.5),
#         axis.text.y  = element_text(size=11),
#         plot.title = element_text(size = 15),
#         legend.text = element_text(size = 11),
#         legend.title = element_text(size = 13),
#         strip.text.x = element_text(size = 13),
#         strip.text.y = element_text(size = 13))
# ggsave("measures.png", dpi = 300)
# 
# Adding summary mean and se
gc.analysis.dfl.summ = ddply(gc.analysis.dfl,
                            .(sample, strain, media, variable),
                            summarize,
                            mean = mean(measurement),
                            se = sd(measurement)/(sqrt(length(measurement))))
# gc.analysis.dfl.summ
# ggplot(gc.analysis.dfl, aes(x = sample, y = measurement)) +
#   geom_crossbar(data = gc.analysis.dfl.summ, aes(x = sample, y = mean, ymin = mean-se, ymax = mean+se), size = 0.25) +
#   geom_point(position = position_jitter(w = 0.25), size = 3, pch = 21, colur = "black", aes(fill = sample)) +
#   facet_wrap(~ variable, scales = "free_y") +
#   scale_fill_manual(values = rev(brewer.pal(8, "Paired"))) +
#   theme(panel.border = element_rect(fill = NA, colour = "black"),
#         axis.title.x = element_text(vjust = 0, size = 14),
#         axis.title.y = element_text(vjust = 0.4, size = 14),
#         axis.text.x = element_text(size=11, angle = 90, hjust = 1, vjust = 0.5),
#         axis.text.y  = element_text(size=11),
#         plot.title = element_text(size = 15),
#         legend.text = element_text(size = 11),
#         legend.title = element_text(size = 13),
#         strip.text.x = element_text(size = 13),
#         strip.text.y = element_text(size = 13))
# # ggsave("measures2-alg2.png", dpi = 300)
# # 
# Summary graph with the smooth.spline on all data per sample would be cool too
# careful of the re-leveling, might be different each time

gc.analysis.dfl$strain = factor(gc.analysis.dfl$strain, levels = levels(gc.analysis.dfl$strain)[c(5, 1:3, 4, 6)])
levels(gc.analysis.dfl$strain)
gc.analysis.dfl.summ$strain = factor(gc.analysis.dfl.summ$strain, levels = levels(gc.analysis.dfl.summ$strain)[c(5,1:3,4,6)])
levels(gc.analysis.dfl.summ$strain)

# ggplot(gc.analysis.dfl, aes(x = strain, y = measurement, shape = factor(media, levels = c("YEPD", "AA-SHOCK")))) +
#   geom_crossbar(data = gc.analysis.dfl.summ, aes(x = strain, y = mean, ymin = mean-se, ymax = mean+se), size = 0.25) +
#   geom_point(position = position_jitter(w = 0.25), size = 2, fill = "white") +
#     scale_shape_manual(values=c(21,24), name = "Media") +
#     facet_wrap(~ variable, scales = "free_y") +
#     theme(panel.border = element_rect(fill = NA, colour = "black"),
#         axis.title.x = element_text(vjust = 0, size = 14),
#         axis.title.y = element_text(vjust = 0.4, size = 14),
#         axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = 0.5),
#         axis.text.y  = element_text(size = 10),
#         plot.title = element_text(size = 14),
#         legend.text = element_text(size = 10),
#         legend.title = element_text(size = 12),
#         strip.text.x = element_text(size = 12),
#         strip.text.y = element_text(size = 12))

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(gc.analysis.dfl, aes(x = strain, y = measurement, fill = media)) +
  geom_crossbar(data = gc.analysis.dfl.summ, aes(x = strain, y = mean, ymin = mean-se, ymax = mean+se, colour = media, fill = NULL), 
                size = 0.4, width = 0.5) +
  geom_point(position = position_jitter(w = 0.15), size = 2.5, pch = 21) +
  scale_fill_manual(values=cbPalette[2:3]) +
  scale_colour_manual(values=cbPalette[2:3]) +
  facet_wrap(~ variable, scales = "free_y") +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        axis.title.x = element_text(vjust = 0, size = 14),
        axis.title.y = element_text(vjust = 0.4, size = 14),
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y  = element_text(size = 10),
        plot.title = element_text(size = 14),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))
ggsave("measures3.png", dpi = 300)
# 
# A final EDA plot - smooth.spline on pooled replicate well to give some visual summary of average qualities
tecanData.l2 = dlply(tecanData, .(sample))
growth.curve.lite = function(foo) {
  # calculating interesting characteristics of the growth curve, fitting a smooth.spline()
  # on an hour time-base.
  require(grofit)
  spl.fit = smooth.spline(foo$expt.time/3600000, foo$corrected.abs, keep.data = FALSE)
  data.frame(fit.time = spl.fit$x, abs.fit = spl.fit$y)
} 
spl.fits.l = lapply(tecanData.l2, growth.curve.lite)
spl.fits.df = ldply(spl.fits.l)
spl.fits.df$.id = factor(spl.fits.df$.id)
spl.fits.df$.id = factor(spl.fits.df$.id, levels = rev(levels(spl.fits.df$.id)))
ggplot(tecanData, aes(x = expt.time/3600000, y = corrected.abs, colour = sample)) +
    geom_point(alpha = 0.5) +
    scale_colour_manual(values = brewer.pal(8, "Paired")) +
    geom_line(data = spl.fits.df, aes(x = fit.time, y = abs.fit, colour = .id), size = 1.5) +
    scale_x_continuous(name = "Time, hrs",
                       breaks = seq(from = 0, to = 24, by = 4)) +
    ggtitle("Corrected OD600 readings, line of smooth.spline fit by pooled sample") +
    theme(panel.border = element_rect(fill = NA, colour = "black"),
          axis.title.x = element_text(vjust = 0, size = 14),
          axis.title.y = element_text(vjust = 0.9, size = 14),
          axis.text.x = element_text(size=12),
          axis.text.y  = element_text(size=12),
          plot.title = element_text(size = 16),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14))
ggsave("smooth-spline-bySample.png", dpi = 300)