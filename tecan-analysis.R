# A function to generate a template for the sample.info file:
generate.sample.info.template = function() {
  plate.letters = rep(LETTERS[1:8], each = 12)
  plate.numbers = rep(1:12, 8)
  plate.template = data.frame(plate.letters,
                              plate.numbers,
                              well = paste(plate.letters, plate.numbers, sep = ""))
  write.csv(plate.template, file = "plate-template.csv", row.names = FALSE)
  rm(plate.letters, plate.numbers, plate.template)
}
# 
# A function to read in tecan data and return a long annotated data.frame:
read.tecanData = function(datafile, samplefile) {
  # 
  # File must be saved as a .csv first
  # Read in the whole thing to extract the number of cycles.
  # Assumes size of header is constant.
  header = read.csv(datafile,
                    header = FALSE,
                    nrows = 35,
                    colClasses = "character")
  no.cycles = as.integer(header[grep("Kinetic Cycles", header$V1),5])
  tecanData = read.csv(datafile,
                       skip = 35,
                       header = TRUE,
                       check.names = FALSE)
  # 
  # Will give an error because of the non-standard celcius character
  # Correcting column labels
  col.names = colnames(tecanData)
  col.names[1] = "cycle.no"
  col.names[2] = "temp.C"
  colnames(tecanData) = col.names
  # 
  # Removing empty lines and end time comment from the end of the file
  # TODO - some sort of check to make sure this happens
  tecanData = tecanData[1:no.cycles,]
  #
  # Plotting temperature as a function of cycle no. to look for dependancies
  # change nature of these columns
  tecanData$cycle.no = as.integer(as.character(tecanData$cycle.no))
  tecanData$temp.C = as.numeric(as.character(tecanData$temp.C))
  # 
  # the plotting
  # black line is linear model lm(), red line is line(); implementing a robustly fitted line,
  # following the method of Tukey in EDA.
  png(file = "cycle-temp-plot.png", width = 7, height = 7, units = "in", res = 300)
  with(tecanData, plot(cycle.no, temp.C, type = "b"))
  with(tecanData, abline(lm(temp.C ~ cycle.no), col = "blue"))
  with(tecanData, abline(line(cycle.no, temp.C), col = "red"))
  dev.off()
  # 
  # Some statistical analysis:
  cycle.temp.stats = capture.output(with(tecanData, summary(lm(temp.C ~ cycle.no))))
  cat(cycle.temp.stats, file = "cycle-temp-stats.txt", sep = "\n", append = TRUE)
  cat("Spearman's correlation coefficient", file = "cycle-temp-stats.txt", sep = "\n", append = TRUE)
  cycle.temp.stats = capture.output(cor(tecanData$cycle.no , tecanData$temp.C, method = "spearman"))
  cat(cycle.temp.stats, file = "cycle-temp-stats.txt", sep = "\n", append = TRUE)
  cat("Pearson's correlation coefficient", file = "cycle-temp-stats.txt", sep = "\n", append = TRUE)
  cycle.temp.stats = capture.output(cor(tecanData$cycle.no , tecanData$temp.C, method = "pearson"))
  cat(cycle.temp.stats, file = "cycle-temp-stats.txt", sep = "\n", append = TRUE)
  # 
  # Remove the temperature data from the dataframe
  tecanData = tecanData[,3:length(tecanData)]
  # 
  # TODO - might be cool to output some basic plots of some/all of the empty wells, to see
  # what they look like.
  # 
  # # Can use this object to look at what an empty well looks like
  # plot(tecanData[,2], tecanData[,1])
  # plot(tecanData[,4], tecanData[,3])
  # plot(tecanData[,6], tecanData[,5])
  # plot(tecanData[,8], tecanData[,7])
  # # Some have a high value at the beginning, that makes it look like something is happening, 
  # # but the range is so small.
  # 
  # Completed sample.info file; empty well must be labelled EMPTY
  sample.info = read.csv(samplefile, header = TRUE, stringsAsFactors = TRUE)
  # 
  # Removing the empty wells from tecanData
  # FOR I IN TECANDATA COLUMN NAMES
  #     IF CORRESPONDING SAMPLE.INFO SAMPLE IS NA
  #         THEN REMOVE THAN COLUMN, AND THE ONE NEXT TO IT
  #
  drops = numeric()  
  for (i in 1:length(colnames(tecanData))) {
    if (colnames(tecanData)[i] %in% sample.info$well[which(sample.info$sample == "EMPTY")]) {
      drops = c(drops, i, i+1)
    }
  }
  tecanData = tecanData[,-drops, drop = FALSE]
  # 
  # TODO - sort of check to make sure it worked
  # 
  # Extracting data into a long dataframe
  require(reshape2)
  growth.data = melt(tecanData, value.name = "OD600", variable.name = "well")
  time.data = melt(tecanData[,seq(from = 2, to = length(tecanData), by = 2)])
  tecanData.long = data.frame(growth.data,
                              time.ms = time.data$value)
  # 
  # Fuse with sample info:
  tecanData.long = merge(sample.info, tecanData.long,
                         by.x = "well",
                         by.y = "well")
  list(tecanData.long = tecanData.long,
       sample.info = sample.info,
       growth.data = growth.data,
       time.data = time.data,
       header = header)
}
data = read.tecanData("20140627-gcn20arb1del.csv", "20140627-sample-info.csv")
# 
# Extracting the formatted data for plotting
tecanData = data[[1]]
# 
# EDA plotting of raw data 
library(ggplot2)
ggplot(tecanData, aes(x = time.ms/3600000, y = OD600, colour = sample, group = well)) +
    geom_point() +
    scale_colour_brewer(palette = "Paired") +
    scale_x_continuous(name = "Time, hrs",
                       breaks = seq(from = 0, to = 24, by = 4)) +
    ggtitle("Raw OD600 readings") +
    theme(panel.border = element_rect(fill = NA, colour = "black"),
          axis.title.x = element_text(vjust = 0, size = 14),
          axis.title.y = element_text(vjust = 0.2, size = 14),
          axis.text.x = element_text(size=12),
          axis.text.y  = element_text(size=12),
          plot.title = element_text(size = 16),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14))
ggsave("raw.png", dpi = 400)
ggplot(tecanData, aes(x = time.ms/3600000, y = OD600, colour = sample, group = well)) +
    geom_line(size = 1.5) +
    scale_colour_brewer(palette = "Paired") +
    scale_x_continuous(name = "Time, hrs",
                       breaks = seq(from = 0, to = 24, by = 4)) +
    ggtitle("Raw OD600 readings, by well") +
    theme(panel.border = element_rect(fill = NA, colour = "black"),
          axis.title.x = element_text(vjust = 0, size = 14),
          axis.title.y = element_text(vjust = 0.2, size = 14),
          axis.text.x = element_text(size=12),
          axis.text.y  = element_text(size=12),
          plot.title = element_text(size = 16),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14))
ggsave("raw-by-well.png", dpi = 400)
ggplot(tecanData, aes(x = time.ms/3600000, y = OD600, colour = sample, group = well)) +
    geom_line(size = 1.5) +
    scale_colour_brewer(palette = "Paired") +
    facet_grid(~ media) +
    scale_x_continuous(name = "Time, hrs", breaks = seq(from = 0, to = 24, by = 4)) +
    ggtitle("Raw OD600 readings, faceted by media") +
    theme(panel.border = element_rect(fill = NA, colour = "black"),
          axis.title.x = element_text(vjust = 0, size = 14),
          axis.title.y = element_text(vjust = 0.2, size = 14),
          axis.text.x = element_text(size=12),
          axis.text.y  = element_text(size=12),
          plot.title = element_text(size = 16),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          strip.text.x = element_text(size = 12),
          strip.text.y = element_text(size = 12))
ggsave("rawLines-facet-media.png", dpi = 400)
tecanData.negative = subset(tecanData, strain == "NONE" )
ggplot(tecanData.negative, aes(x = time.ms/3600000, y = OD600, colour = sample)) +
    geom_point() +
    geom_line(aes(group = well)) +
    scale_x_continuous(name = "Time, hrs", breaks = seq(from = 0, to = 24, by = 4)) +
    ggtitle("Raw OD600 readings, median only wells") +
    theme(panel.border = element_rect(fill = NA, colour = "black"),
          axis.title.x = element_text(vjust = 0, size = 14),
          axis.title.y = element_text(vjust = 0.2, size = 14),
          axis.text.x = element_text(size=12),
          axis.text.y  = element_text(size=12),
          plot.title = element_text(size = 16),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14))
ggsave("raw-neg-wells.png", dpi = 400)
# 
# Background correction - Nick's suggestion of taking per-well median over time-course, then accross-well median
# of these medians and using this as a per-media background correction.
library(plyr)
tecanData.neg.summ = ddply(tecanData.negative,
                          .(well),
                          summarize,
                          well.median = median(OD600))
sample.info = data[[2]]
tecanData.neg.summ = merge(tecanData.neg.summ,
                           sample.info,
                           by.x = "well",
                           by.y = "well")
media.bkgd = ddply(tecanData.neg.summ,
                    .(media),
                    summarize,
                    media.median = median(well.median))
media.bkgd
# 
# Applying the correction accross the actual data
tecanData = tecanData[order(tecanData$media),]
bkgd = c(rep(0.129700, length(which(tecanData$media == "AA-SHOCK"))), rep(0.135725, length(which(tecanData$media == "YEPD"))))
tecanData = data.frame(tecanData, bkgd)
library(dplyr)
tecanData = tecanData %.%
  mutate(corrected.OD600 = OD600 - bkgd)
#
library(grofit)
