# Constructing a proper time base for tecan expt
header = read.csv("20140627-gcn20arb1del.csv",
                header = FALSE,
                nrows = 35,
                colClasses = "character")
no.cycles = as.integer(header[grep("Kinetic Cycles", header$V1),5])
timeData = read.csv("20140627-gcn20arb1del.csv", 
					header = TRUE, 
					stringsAsFactors = FALSE,
					skip = 35,
					check.names = FALSE)
col.names = colnames(timeData)
col.names[1] = "cycle.no"
col.names[2] = "temp.C"
colnames(timeData) = col.names
timeData = timeData[1:no.cycles, 3:length(timeData)]
# 
# Now I've got just the data; I want to extract the time values
# 
# removing the absorbance data
even.columns = seq(from = 2, to = length(timeData), by = 2)
# even.columns
timeData = timeData[,even.columns]
# head(timeData)
# 
# Specificying the right column names
plate.letters = rep(LETTERS[1:8], each = 12)
plate.numbers = rep(1:12, 8)
wells = paste(plate.letters, plate.numbers, sep = "")
# wells
colnames(timeData) = wells
# head(timeData)
# 
# calculating the average cycle to cycle interval, where the recorded data is in a sensible format
# head(timeData, 20)
# use the first 11 cycles/rows
# 
good.timeData = timeData[1:11,]
# good.timeData
good.timeData.diffs.df = apply(good.timeData, 2, diff)
# dim(good.timeData.diffs.df)
dim(good.timeData.diffs.df)[1] * dim(good.timeData.diffs.df)[2] == length(as.vector(good.timeData.diffs.df))
# Should be TRUE, and is
# 
good.timeData.diffs.vt = as.vector(good.timeData.diffs.df)
png(file = "hist-cycle-time-lags.png", width = 7, height = 7, units = "in", res = 300)
hist(good.timeData.diffs.vt)
abline(v = median(good.timeData.diffs.vt), col = "blue")
abline(v = mean(good.timeData.diffs.vt), col = "red")
dev.off()
# 
# Mean and median don't really feel right here, ca. double the number of tags are in the class 913405 - 913410
# table(good.timeData.diffs.vt)
png(file = "table-plot.png", width = 7, height = 7, units = "in", res = 300)
plot(table(good.timeData.diffs.vt), las = 2, xlab = "")
dev.off()
# 
# Surprisingly sparse range of values
# Looks like there are two modal classes essentially, so the median might be a good pick after all.
med.cycle.diff = median(good.timeData.diffs.vt)
med.cycle.diff
# 
# Ok, so I had the crazy thought that the cycle to cycle difference might depend on plate location
# Examine this using a long dataframe and ggplot2
library(reshape2)
good.timeData.diffs.df.long = melt(good.timeData.diffs.df,
									value.name = "time.ms")
# head(good.timeData.diffs.df.long)
#
# Adding a row variable
plate.rows = rep(unique(plate.letters), each = 10 * 12) 
good.timeData.diffs.df.long$plate.rows = plate.rows
# 
library(ggplot2)
# 
# colour blind friendly qualitative palette from http://jfly.iam.u-tokyo.ac.jp/color/
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(good.timeData.diffs.df.long, aes(x = as.factor(Var1), y = time.ms, colour = plate.rows)) +
	geom_jitter() +
	xlab("Cycle No.") +
	scale_colour_manual(values=cbPalette) +
	geom_hline(aes(yintercept = med.cycle.diff), linetype = "dashed", size = 0.25) +
	ggtitle("Cycle to cycle time difference; colour by plate row, line at median") +
	theme(panel.border = element_rect(fill = NA, colour = "black"),
          axis.title.x = element_text(vjust = 0),
          axis.title.y = element_text(vjust = 0.2),
          plot.title = element_text(vjust = 0.8))
	ggsave("cycle-diffs-by-row.png", dpi = 300)
# 
# Now colour by plate column
plate.columns = rep(rep(1:12, each = 10), 8)
good.timeData.diffs.df.long$plate.columns = plate.columns
# head(good.timeData.diffs.df.long)
# good.timeData.diffs.df.long
# 
# had to use the default colour because of the number of classes in factor(plate.columns)
ggplot(good.timeData.diffs.df.long, aes(x = as.factor(Var1), y = time.ms, colour = factor(plate.columns))) +
	geom_jitter() +
	xlab("Cycle No.") +
	geom_hline(aes(yintercept = med.cycle.diff), linetype = "dashed", size = 0.25) +
	ggtitle("Cycle to cycle time difference; colour by plate column, line at median") +
	theme(panel.border = element_rect(fill = NA, colour = "black"),
          axis.title.x = element_text(vjust = 0),
          axis.title.y = element_text(vjust = 0.2),
          plot.title = element_text(vjust = 0.8))
	ggsave("cycle-diffs-by-column.png", dpi = 300)
# 
# Ok, I think I've had enough of this - there's no obvious association by plate column - TODO: could look at the
# distribution of cycle-to-cycle diffs by column and row, but I'm starting to doubt the utility of this.
# 
# Constructing the actual time base:
# 
# Actually, before I do, I should look at the time delay between the two halves of the plate, to see how that
# varies from cycle to cycle. Cycle to cycle analysis suggests it must do.
# 
# E12 - A12 should give the right value
good.timeData$E12 - good.timeData$A12
good.timeData$F12 - good.timeData$B12
good.timeData$G12 - good.timeData$C12
good.timeData$H12 - good.timeData$D12
# 
# Yep, it varies; but, at least it's the same between rows
# 
png(file = "time-delay-between-plate-halves-byCycle.png", width = 7, height = 7, units = "in", res = 300)
plot(1:11, good.timeData$E12 - good.timeData$A12, 
		xlab = "Cycle No.", 
		ylab = "Time delay between well A12 and E12, ms",
		type = "b")
abline(h = median(good.timeData$E12 - good.timeData$A12), col = "red")
abline(h = mean(good.timeData$E12 - good.timeData$A12), col = "blue")
dev.off()
png(file = "time-delay-between-plate-halves-freq.png", width = 7, height = 7, units = "in", res = 300)
plot(table(good.timeData$E12 - good.timeData$A12), 
	las = 2,
	xlab = "Time delay between well A12 and E12, ms",
	ylab = "Frequency")
dev.off()
# 
# I choose the median
med.timeDelay.plate.halves = median(good.timeData$E12 - good.timeData$A12)
ls()
med.timeDelay.plate.halves
# 
# 
# 
tecanTime = data.frame(row = rep(plate.letters, no.cycles),
					   column = rep(plate.numbers, no.cycles),
					   well = rep(wells, no.cycles),
					   cycle = rep(1:no.cycles, each = no.cycles))
tecanTime

expt.time = numeric()

for (i in 1:nrow(tecanTime)) {
	if(tecanTime$row[i] %in% LETTERS[1:4]) {
		expt.time[i] = ((tecanTime$column[i] -1) * 640) + ((tecanTime$cycle[i] -1) * med.cycle.diff)
	} else {
		expt.time[i] = (((22 * 640) + med.timeDelay.plate.halves) - (640 * (tecanTime$column[i] -1))) + ((tecanTime$cycle[i] -1) * med.cycle.diff)
	}
}

head(expt.time, 500)
head(tecanTime)

tecanTime$expt.time = expt.time
head(tecanTime)
# 
# Need to explot correlation with good.timeDatq to see whether there are any systematic deviations evident 

good.timeData
good.timeData.long = melt(good.timeData)
head(good.timeData.long, 20)
ls()
no.cycles
cycles = rep(1:11, 96)
length(cycles)
good.timeData.long$cycle = cycles
colnames(good.timeData.long) = c("well", "time", "cycle")
good.timeData.long$row = rep(LETTERS[1:8], each = 11 * 12)
good.timeData.long$column = rep(rep(1:12, each = 11), 8)
good.timeData.long
head(tecanTime)
tecanTime.short = subset(tecanTime, cycle <= 11)
dim(tecanTime)
dim(tecanTime.short)
?sort
?order
tecanTime.short = tecanTime.short[order(tecanTime.short$well),]
head(tecanTime.short, 20)
good.timeData.long = good.timeData.long[order(good.timeData.long$well),]
head(good.timeData.long, 20)
str(good.timeData.long)
str(tecanTime.short)
# 
# try something
plot(sort(good.timeData.long$time), sort(tecanTime.short$expt.time))
# Seem like this is valid to me
tecanTime.short$well = factor(tecanTime.short$well, levels = wells)
plot(good.timeData.long$time, tecanTime.short$expt.time,
     xlim = c(0, 2000000), ylim = c(0, 2000000))
plot(good.timeData.long$time, tecanTime.short$expt.time,
     xlim = c(0, 10000), ylim = c(0, 10000))
cor(good.timeData.long$time, tecanTime.short$expt.time)
# lol, this is 1 - looks like it worked