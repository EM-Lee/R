#http://www.rexamples.com/4/Reading%20user%20input

#bill <- readline(prompt = "What is the bill? ")
#bill <- as.numeric(bill)
tryCatch(
  {
    bill <- readline(prompt = "What is the bill? ")
    bill <- as.numeric(bill)
  }
)
tipRate <- readline(prompt = "What is the tip percentage? ")
tipRate <- as.integer(tipRate)

total <- bill + bill * tipRate / 100

cat(sprintf("The tip is %d%%.", tipRate), "\n")
cat(sprintf("The total is $%.2f.", total))


readinteger <- function()
{
  n <- readline(prompt = "Enter an integer: ")
  
  #checks for a string that consists of nothing but one or more digits
  if(!grepl("^[0-9]+$",n))
  {
    print("Sould be an integer. Try again.")
    return(readinteger())
  }
  return(as.integer(n))
}

print(readinteger())