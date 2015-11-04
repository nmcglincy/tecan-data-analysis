library(readr)
library(magrittr)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggthemes)

# data.file is character string of the tecan output .csv formatted for number in excel
# coding is easier if you record the whole plate - should probably do something about this
# sample-info.csv must have column named "strain", and be produced by the function.

Initial.processing = function(data.file) {
	data = read_csv(file = data.file,
	                skip = 35,
	                n_max = 96,
	                col_names = TRUE) %>%
	  na.omit()
	names(data)[2] = "Temp.C"
	names(data) = make.names(names = names(data),
	                         unique = TRUE,
	                         allow_ = TRUE)
	data$Cycle.Nr. = as.numeric(data$Cycle.Nr.)

	sample.info = read_csv(file = "sample-info.csv",
	                       col_names = TRUE)

	data = data %>%
	  select(c(1:2, 4, seq(from = 3, to = length(data), by = 2))) %>%
	  gather("well", "od600", A1:H12) %>%
	  join(data, sample.info, by = "well")
	  
  	ggplot(data,
  	       aes(x = Time..ms./3600000,
  	           y = od600,
  	           colour = strain)) +
  	  geom_line(size = 1) +
  	  facet_wrap( ~ well, ncol = 12) +
  	  xlab("Time, hrs") +
  	  ylab("OD, 600 nm") +
  	  scale_x_continuous(breaks = c(0, 10)) +
  	  scale_colour_colorblind() +
  	  guides(colour = guide_legend(override.aes = list(size=10))) +
  	  theme(panel.border = element_rect(fill = NA, colour = "black"),
  	        axis.title.x = element_text(vjust = 0, size = 16),
  	        axis.title.y = element_text(vjust = 1, size = 16),
  	        axis.text.x = element_text(size=16),
  	        axis.text.y  = element_text(size=16),
  	        plot.title = element_text(size = 20),
  	        legend.text = element_text(size = 16),
  	        legend.title = element_text(size = 18),
  	        strip.text.x = element_text(size = 16),
  	        strip.text.y = element_text(size = 16))
  	ggsave("by-well.png")
	
	# Analysis of temperature drift
	cycle.data  = data %>%
	  select(Cycle.Nr., Temp.C) %>%
	  distinct(Cycle.Nr.)

	png(file = "cycle-temp-plot.png", width = 7, height = 7, units = "in", res = 300)
	plot(x = cycle.data$Cycle.Nr.,
	     y = cycle.data$Temp.C,
	     type = "b",
	     ylim = c(29, 31),
	     xlim = c(0, 96),
	     xlab = "Cycle",
	     ylab = "Temperature, C")
	with(cycle.data, abline(lm(Temp.C ~ Cycle.Nr.), col = "blue"))
	with(cycle.data, abline(line(Cycle.Nr., Temp.C), col = "red"))
	dev.off()

	cycle.temp.stats = capture.output(with(cycle.data, summary(lm(Temp.C ~ Cycle.Nr.))))
	cat(cycle.temp.stats, file = "cycle-temp-stats.txt", sep = "\n", append = TRUE)
	cat("Spearman's correlation coefficient", file = "cycle-temp-stats.txt", sep = "\n", append = TRUE)
	cycle.temp.stats = capture.output(cor(cycle.data$Cycle.Nr. , cycle.data$Temp.C, method = "spearman"))
	cat(cycle.temp.stats, file = "cycle-temp-stats.txt", sep = "\n", append = TRUE)
	cat("Pearson's correlation coefficient", file = "cycle-temp-stats.txt", sep = "\n", append = TRUE)
	cycle.temp.stats = capture.output(cor(cycle.data$Cycle.Nr., cycle.data$Temp.C, method = "pearson"))
	cat(cycle.temp.stats, file = "cycle-temp-stats.txt", sep = "\n", append = TRUE)

}