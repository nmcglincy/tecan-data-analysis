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
str(tecanData)
class(tecanData)
head(tecanData[,1:5])
#?colnames
col.names = colnames(tecanData)
col.names
col.names[1] = "cycle.no"
col.names[2] = "temp.C"
colnames(tecanData) = col.names
#?tail
tail(tecanData)
dim(tecanData)
tecanData = tecanData[1:96,]
# Would be cool if I could extract the cycle name from the tecan 
# results file.
#
dim(tecanData)
tail(tecanData)
#
# Separate off the temperature data
head(tecanData)
temp.data = tecanData[,1:2]
temp.data
#
length(tecanData)
tecanData = tecanData[,3:length(tecanData)]
head(tecanData)
#
# Plot the temperature data
head(temp.data)
str(temp.data)
temp.data$cycle.no = numeric(temp.data$cycle.no)
temp.data$temp.C = numeric(temp.data$temp.C)
str(temp.data)
with(temp.data, plot(cycle.no, temp.C, type = "line"))