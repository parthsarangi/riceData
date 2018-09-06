ricedf <- read.csv("riceData.csv")
names(ricedf)
head(ricedf)
dim(ricedf)
str(ricedf)
require(caret)
df1 <- ricedf[c(11:17)]
df1 <- lapply(df1, as.numeric)
df1matrix <- as.matrix(df1)
correlationMatrix <- cor(df1matrix)


df1 = ricedf
for(n in c(1:ncol(df1))) {
  print(n)
  #str(df1[[n]])
  df1[[n]] <- as.numeric(df1[[n]])
}

correlation = cor(df1)

write.csv(correlation,"correlation.csv")


