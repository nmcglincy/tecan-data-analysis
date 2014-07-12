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
# use the first 11 cycles/rows
# 
?diff
(1:10)^2
diff(x = (1:10)^2)
# 
good.timeData = timeData[1:11,]
good.timeData
?apply
good.timeData.diffs.df = apply(good.timeData, 2, diff)
dim(good.timeData.diffs.df)
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
table(good.timeData.diffs.vt)
png(file = "table-plot.png", width = 7, height = 7, units = "in", res = 300)
plot(table(good.timeData.diffs.vt), las = 2, xlab = "")
dev.off()
# 
# Surprisingly sparse range of values
# Looks like there are two modal classes essentially, so the median might be a good pick after all.
med.cycle.diff = median(good.timeData.diffs.vt)

