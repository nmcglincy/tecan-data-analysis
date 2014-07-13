read.tecan.data2 = function(datafile, sampleInfo, emptyWells = "FALSE") {
	# Second attempt at reading in tecan data
	# 
	# Constructing time and well data
	# Require objects to construct tecanTime
	# 
	# plate.letters & plate.numbers
	plate.letters = rep(LETTERS[1:8], each = 12)
	plate.numbers = rep(1:12, 8)
	#
	# wells object derived from the both
	wells = paste(plate.letters, plate.numbers, sep = "")
	# 
	# the number of cycles, from the header of the datafile
	header = read.csv(datafile,
	                	header = FALSE,
	                	nrows = 35,
	                	colClasses = "character")
	no.cycles = as.integer(header[grep("Kinetic Cycles", header$V1),5])
	# 
	# Read in the actual data
	tecanData = read.csv(datafile,
	                     skip = 35,
	                     header = TRUE,
	                     check.names = FALSE)
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
	sample.info = read.csv(sampleInfo, header = TRUE, stringsAsFactors = TRUE)
	# 
	# Splitting tecanData into time and absorbance data
	# Odd rows are data, even rows are time
	absData = tecanData[,seq(from = 1, to = length(tecanData), by = 2)]
	timeData = tecanData[,seq(from = 2, to = length(tecanData), by = 2)]
	# 
	# Take the time data first, and derive useful measures from it
	# median cycle to cycle difference, ms
	colnames(timeData) = wells
	good.timeData = timeData[1:11,]
	good.timeData.diffs.vt = as.vector(apply(good.timeData, 2, diff))
	med.cycle.diff = median(good.timeData.diffs.vt)
	# 
	# median time delay between plate halves
	med.timeDelay.plate.halves = median(good.timeData$E12 - good.timeData$A12)
	# 
	# The time object
	tecanTime = data.frame(row = rep(plate.letters, no.cycles),
						   					column = rep(plate.numbers, no.cycles),
						   					well = rep(wells, no.cycles),
						   					cycle = rep(1:no.cycles, each = no.cycles))
	expt.time = numeric()
	for (i in 1:nrow(tecanTime)) {
		if(tecanTime$row[i] %in% LETTERS[1:4]) {
			expt.time[i] = ((tecanTime$column[i] -1) * 640) + ((tecanTime$cycle[i] -1) * med.cycle.diff)
		} else {
			expt.time[i] = (((22 * 640) + med.timeDelay.plate.halves) - (640 * (tecanTime$column[i] -1))) + ((tecanTime$cycle[i] -1) * med.cycle.diff)
		}
	}
	tecanTime$expt.time = expt.time
	tecanTime$well.cycle = paste(tecanTime$well, tecanTime$cycle, sep = ".")
	# 
	# Now the asbData
	library(reshape2)
	absData = melt(absData, variable.name = "well", value.name = "abs")
	cycle = rep(1:96, 96)
	absData$cycle = cycle
	absData$well.cycle = paste(absData$well, absData$cycle, sep = ".")
	tecanData.comp = merge(tecanTime, absData,
												by.x = "well.cycle",
												by.y = "well.cycle")
	tecanData.comp$well.y = NULL
	tecanData.comp$cycle.y = NULL
	foo = sample.info[,3:length(sample.info)]
	tecanData.comp = merge(tecanData.comp, foo,
												by.x = "well.x", by.y = "well")
	colnames(tecanData.comp)[c(1,5)] = c("well", "cycle")
	rm(foo, i)
	if (emptyWells == "FALSE") {
		tecanData.comp = subset(tecanData.comp, media != "EMPTY")
	} 
	list(tecanData.comp, sample.info)
}
# 