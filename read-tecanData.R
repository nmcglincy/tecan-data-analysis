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
