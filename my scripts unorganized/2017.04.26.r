bill <- readline(prompt = "What is the bill? ")
bill <- as.integer(bill)
tipRate <- readline(prompt = "What is the tip percentage? ")
tipRate <- as.integer(tipRate)

total <- bill + bill * tipRate / 100

cat(sprintf("The tip is %d%%.", tipRate), "\n")
cat(sprintf("The total is $%.2f.", total))