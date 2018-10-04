getwd()
setwd("riceData/glm/")

# riceData is ready to use and properly formatted

df = read.csv("riceData.csv")

names(df)
str(df)

for (i in c(1:3)) {
  df[[i]] = as.factor(df[[i]])
}

str(df)

write.csv(df,"riceDataFactored.csv",row.names = FALSE)

library(quantreg)
model.quantileRegression <- rq(yield_pa_area ~ .,data = df,tau=0.25)

# Testing data
df.nrow <- nrow(df)
index.text <- sample(df.nrow,size = round(0.3*df.nrow))
df.test <- df[index.text,]

names(df.test)
dim(df.test)

# Value from RF
predicted = predict(model.quantileRegression,df.test)

#Check lengths of outputs
length(predicted)


df.results = data.frame(actual=df.test$yield_pa_area,predicted)

write.csv(df.results,"randomForest_outputs.csv")
str(predicted)
summary(model.randomForest)

