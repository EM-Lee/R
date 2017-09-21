### Tip Calculator App
# prompt for a bill amount & a tip rate
# compute the tip
# then display both the tip & total amount of the bill
###

### user input
# ask
# gtools
# https://www.rdocumentation.org/packages/gtools/versions/3.5.0

# menu
menu(c("Yes", "No"), title = "Do you want this?")

switch(menu(c("List letters", "List LETTERS")) +1,
       cat("Nothing done\n"), letters, LETTERS)
# switch
# https://www.programiz.com/r-programming/switch-function
# http://www.endmemo.com/program/R/switch.php

# readline
# http://www.rexamples.com/4/Reading%20user%20input
# as.integer
# http://www.r-tutor.com/r-introduction/basic-data-types/integer
# as.character, as.numeric
# http://www.statmethods.net/management/typeconversion.html
# printing strings
# http://bradleyboehmke.github.io/tutorials/char_basics
# http://www.studytrails.com/r/core/r_console_printing/
bill <- as.numeric(readline("How much is it? "))
tipR <- as.numeric(readline("What is the tip rate? "))
tip <- bill * tipR/100
total <- as.character(bill + tip)
print(paste("The total is $", total, ".", sep = ""), quote = FALSE)
