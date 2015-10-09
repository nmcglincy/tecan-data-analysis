generate.sample.info.template = function() {
  plate.letters = rep(LETTERS[1:8], each = 12)
  plate.numbers = rep(1:12, 8)
  plate.template = data.frame(plate.letters,
                              plate.numbers,
                              well = paste(plate.letters, plate.numbers, sep = ""))
  write.csv(plate.template, file = "plate-template.csv", row.names = FALSE)
  # rm(plate.letters, plate.numbers, plate.template)
}
# 
# uncomment the next line to make it an "executable" using Rscript
generate.sample.info.template()