# File must be saved as a .csv first
# Read in the whole thing and see if I can get the number of cycles out
header = read.csv("20140627-gcn20arb1del.csv",
                  header = FALSE,
                  nrows = 35,
                  colClasses = "character")
no.cycles = as.integer(header[grep("Kinetic Cycles", header$V1),5])
no.cycles


tecanData = read.csv("20140627-gcn20arb1del.csv",
					skip = 35,
					header = TRUE,
					check.names = FALSE)

# Some sort of class problem with the first two columns
col.names = colnames(tecanData)
col.names
col.names[1] = "cycle.no"
col.names[2] = "temp.C"
colnames(tecanData) = col.names
head(tecanData[,1:10])
#?tail
tail(tecanData)
dim(tecanData)
tecanData = tecanData[1:no.cycles,]
# Would be cool if I could extract the cycle name from the tecan 
# results file.
#
dim(tecanData)
tail(tecanData)
#
# # Separate off the temperature data
# head(tecanData)
# temp.data = tecanData[,1:2]
# temp.data
# #
# length(tecanData)
# tecanData = tecanData[,3:length(tecanData)]
# head(tecanData)
# #
# Plot the temperature data
# head(temp.data)
# str(temp.data)
with(tecanData, plot(cycle.no, temp.C))
# yes, it's reading the first two columns as factors somehow...
tecanData$cycle.no = as.character(tecanData$cycle.no)
tecanData$cycle.no = as.integer(tecanData$cycle.no)
# 
# As a one-liner
tecanData$cycle.no = as.integer(as.character(tecanData$cycle.no))
tecanData$temp.C = as.numeric(as.character(tecanData$temp.C))
# str(tecanData)
# Plotting temperature as a function of cycle no.; examine the evidence for an association
with(tecanData, plot(cycle.no, temp.C))
cor(tecanData$cycle.no , tecanData$temp.C, method = "spearman")
cor(tecanData$cycle.no , tecanData$temp.C, method = "pearson")
with(tecanData, abline(lm(temp.C ~ cycle.no)))
# black line on the plot
# 
with(tecanData, summary(lm(temp.C ~ cycle.no)))
# 
# Call:
#     lm(formula = temp.C ~ cycle.no)
# 
# Residuals:
#     Min       1Q   Median       3Q      Max 
# -0.62489 -0.20911  0.01839  0.22285  0.48854 
# 
# Coefficients:
#                   Estimate    Std. Error t value  Pr(>|t|)    
#       (Intercept) 30.163575   0.058421    516.312 <2e-16 ***
#       cycle.no     0.001803   0.001046    1.724   0.0879 .  
# ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.284 on 94 degrees of freedom
# Multiple R-squared:  0.03066,    Adjusted R-squared:  0.02035 
# F-statistic: 2.973 on 1 and 94 DF,  p-value: 0.08793
# 
# Mild increase in temperature with cycle.no, but narrowly misses significance
# 
with(tecanData, abline(line(cycle.no, temp.C), col = "red"))
# Robustly fitting line, following the method of Tukey in EDA. Coloured red. 
# Relationship looks a bit more deterministic, but still within a very narrow range.
# 
# Remove the temperature data from the dataframe
head(tecanData[,1:10])
length(tecanData)
tecanData = tecanData[,3:length(tecanData)]
# 
# Can use this object to look at what an empty well looks like
plot(tecanData[,2], tecanData[,1])
plot(tecanData[,4], tecanData[,3])
plot(tecanData[,6], tecanData[,5])
plot(tecanData[,8], tecanData[,7])
# Some have a high value at the beginning, that makes it look like something is happening, but the range is
# so small.
# 
# Template for sample.info file
plate.letters = rep(LETTERS[1:8], each = 12)
plate.letters
plate.numbers = rep(1:12, 8)
plate.numbers
plate.template = data.frame(plate.letters,
                            plate.numbers,
                            well = paste(plate.letters, plate.numbers, sep = ""))
write.csv(plate.template, file = "plate-template.csv", row.names = FALSE)
# 
# Completed sample.info file
sample.info = read.csv("20140627-sample-info.csv", header = TRUE, stringsAsFactors = TRUE)
sample.info
str(sample.info)
# 
# Removing the empty wells from tecanData
# 
# Pseudo-code for the filter
# 
# FOR I IN TECANDATA COLUMN NAMES
#     IF CORRESPONDING SAMPLE.INFO SAMPLE IS NA
#         THEN REMOVE THAN COLUMN, AND THE ONE NEXT TO IT
# 
for (i in 1:length(colnames(tecanData))) {
    if (colnames(tecanData)[i] %in% sample.info$well[which(sample.info$sample == "EMPTY")]) {
        drops = c(drops, i, i+1)
    }
}
drops
tecanData.filtered = tecanData[,-drops, drop = FALSE]
dim(tecanData)
dim(tecanData.filtered)
head(tecanData.filtered)
# Looks like it worked
# 
# Extracting data into a long dataframe
# 
library(reshape2)
growth.data = melt(tecanData.filtered, value.name = "OD600", variable.name = "well")
head(growth.data)

# time.data = numeric()
# for (i in seq(from = 2, to = length(tecanData.filtered), by = 2)) {
#     time.data = c(time.data, tecanData.filtered[,i])
# }
# head(time.data)
# dim(time.data)
# length(time.data)
# # It's the right length; (5 * 12) * 96
# tecan.data.long = data.frame(growth.data,
#                              time.ms = time.data)
# head(tecan.data.long)
# head(subset(tecan.data.long, well == "C2"))
# 
# huh, that gave me the wrong match...
# a different approach
# 
colnames(tecanData.filtered)
length(colnames(tecanData.filtered)[seq(from = 2, to = length(tecanData.filtered), by = 2)])
time.data = melt(tecanData.filtered[,seq(from = 2, to = length(tecanData.filtered), by = 2)])
head(time.data)
head(tecanData.filtered[,1:10])
tecanData.long = data.frame(growth.data,
                            time.ms = time.data$value)
head(tecanData.long)
head(subset(tecanData.long, well == "C2"))
# 
# that seems to have worked
# Fuse with sample info:
sample.info
tecanData.long = merge(sample.info, tecanData.long,
                       by.x = "well",
                       by.y = "well")
head(tecanData.long)
# Looks good! let get to the graphs
library(ggplot2)

brewerplot <- function (palette) {
    p + scale_fill_brewer(palette = palette) + opts(title=palette)
}

ggplot(tecanData.long, aes(x = time.ms, y = OD600, colour = sample, group = well)) +
    geom_line(size = 2) +
    scale_colour_brewer(palette = "Paired")


