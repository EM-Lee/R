# numbers with different base

# https://www.rdocumentation.org/packages/R.utils/versions/2.5.0/topics/intToBin
# intToBin(x), intToOct(x), intToHex(x): returns a character
# R.utils package
# https://www.rdocumentation.org/packages/R.utils/versions/2.5.0

install.packages("R.utils")
library(R.utils)

numRange <- c(32:255)
numTable = data.frame(
  "No." = numRange, "Char" = intToChar(numRange),
  "Bin" = intToBin(numRange), "Oct" = intToOct(numRange),
  "Hex" = intToHex(numRange))
numTable


numRange2 <- c(65:90, 97:122)
numTable2 = data.frame(
  "No." = numRange2, "Char" = intToChar(numRange2),
  "Bin" = intToBin(numRange2), "Oct" = intToOct(numRange2),
  "Hex" = intToHex(numRange2))
numTable2

# check with options: options()
# options(max.print = 9999)
# doesn't work in RStudio

for (i in 1: 15){
  print(paste(i, as.octmode(i), as.hexmode(i), sep = " : "))
}

numRange3 <- c(65:90, 97:122)
strsplit(as.character(numRange3), " ")
length(numRange3)
for (ascVal in numRange3){
  print(intToUtf8(ascVal))
  print(as.raw(ascVal))
}

numRange3 <- c(65:90, 97:122)
ascValue <- strsplit(intToUtf8(numRange3), "")
numTable3 = data.frame(
  numRange3, ascValue,
  #bin
  strtoi(as.octmode(numRange3)),
  #"Hex" = strtoi(as.hexmode(numRange3))),
  as.character(as.hexmode(numRange3)))
colnames(numTable3) <- c("No", "AsciiValue", "Oct", "Hex")
numTable3


