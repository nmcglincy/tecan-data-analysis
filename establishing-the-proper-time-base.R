# Constructing a proper time base for tecan expt
timeData = read.csv("20140627-gcn20arb1del-justTheTime.csv", header = TRUE, stringsAsFactors = FALSE)
head(timeData)
# 
# removing the absorbance data
dim(timeData)
length(timeData)
odd.columns = seq(from = 1, to = length(timeData), by = 2)
odd.columns
timeData = timeData[,odd.columns]
head(timeData)
# 
# calculating the average cycle to cycle interval, where the recorded data is in a sensible format
head(timeData, 20)
# first 11 cycles/rows
# 
