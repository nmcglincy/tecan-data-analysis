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
# Background correction
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
# 
# Growth curve analysis - something has gone wrong here - need to fix
source("growth-curve-analysis-Alg2.R")
tecanData.l = dlply(tecanData, .(sample))
gc.analysis = lapply(tecanData.l, growth.curve.analysis)
gc.analysis.df = ldply(lapply(gc.analysis, as.data.frame))
sample.info$sample.well = paste(sample.info$sample, sample.info$well, sep = ".")
gc.analysis.df = merge(sample.info, gc.analysis.df, by.x = "sample.well", by.y = ".id")
gc.analysis.dfl = melt(gc.analysis.df, 
              				id.vars = names(gc.analysis.df)[1:7],
              				measure.vars = names(gc.analysis.df)[8:length(names(gc.analysis.df))],
              				value.name = "measurement")
gc.analysis.dfl = subset(gc.analysis.dfl, strain != "NONE")
# 
# Some EDA
# reorder factors - might be better to do this earlier
gc.analysis.dfl$sample = factor(gc.analysis.dfl$sample, levels = rev(unique(gc.analysis.dfl$sample)))
ggplot(gc.analysis.dfl, aes(x = sample, y = measurement, fill = sample)) +
  geom_point(position = position_jitter(w = 0.25), size = 3, pch = 21, colur = "black") +
  facet_wrap(~ variable, scales = "free_y") +
  scale_fill_manual(values = rev(brewer.pal(8, "Paired"))) +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        axis.title.x = element_text(vjust = 0, size = 14),
        axis.title.y = element_text(vjust = 0.6, size = 14),
        axis.text.x = element_text(size=11, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y  = element_text(size=11),
        plot.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text.x = element_text(size = 13),
        strip.text.y = element_text(size = 13))
# ggsave("measures.png", dpi = 300)
# 
# Adding summary mean and se
gc.analysis.dfl.summ = ddply(gc.analysis.dfl,
    				                .(sample, strain, media, variable),
            				        summarize,
                    				mean = mean(measurement),
                    				se = sd(measurement)/(sqrt(length(measurement))))
ggplot(gc.analysis.dfl, aes(x = sample, y = measurement)) +
  geom_crossbar(data = gc.analysis.dfl.summ, aes(x = sample, y = mean, ymin = mean-se, ymax = mean+se), size = 0.25) +
  geom_point(position = position_jitter(w = 0.25), size = 3, pch = 21, colur = "black", aes(fill = sample)) +
  facet_wrap(~ variable, scales = "free_y") +
  scale_fill_manual(values = rev(brewer.pal(8, "Paired"))) +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        axis.title.x = element_text(vjust = 0, size = 14),
        axis.title.y = element_text(vjust = 0.6, size = 14),
        axis.text.x = element_text(size=11, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y  = element_text(size=11),
        plot.title = element_text(size = 15),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        strip.text.x = element_text(size = 13),
        strip.text.y = element_text(size = 13))
# ggsave("measures2-alg2.png", dpi = 300)
# 
# Would be cooler if media was shape - through all graphs
# Summary graph with the smooth.spline on all data per sample would be cool too