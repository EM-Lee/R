### print Hello
# World

# print("Hello\nWorld")
cat("Hello\nWorld")

# example on NewLine Char
for (i in 1:5){
  for (j in 1:i){
    cat(j)
  }
  cat("\n")
}

# str <- c("Hello", "World")
# noquote(strsplit(str, "\n"))