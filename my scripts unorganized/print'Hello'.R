# print 'Hello'

noquote("'Hello'")

cat("'Hello'")

noquote(paste("'Hello'"))
noquote(paste("'", "Hello", "'", sep = ""))

print("'Hello'", quote = FALSE)
print("\'Hello\'", quote = FALSE)

noquote(sQuote("Hello"))


