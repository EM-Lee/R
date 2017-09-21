### print D:\!!!AllData!!!\GoodBooks\programmingBasic100\007.print
### including double quotations

# A forward slash isn't a special character
cat('"D:/!!!AllData!!!/GoodBooks/programmingBasic100/007.print/"')
cat('"D:\\!!!AllData!!!\\GoodBooks\\programmingBasic100\\007.print\\"')

noquote(dQuote("D:/!!!AllData!!!/GoodBooks/programmingBasic100/007.print/"))
print("D:/!!!AllData!!!/GoodBooks/programmingBasic100/007.print/")

# not working as i expected?
noquote(dQuote("D:\\!!!AllData!!!\\GoodBooks\\programmingBasic100\\007.print\\"))
print("D:\\!!!AllData!!!\\GoodBooks\\programmingBasic100\\007.print\\")
