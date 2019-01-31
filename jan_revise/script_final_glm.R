# Wordking directory
# setwd("~/Documents/R/riceData/dec_revise")
print(">> Script starting now")
df = read.csv("ricedata_jan.csv")

for (i in c(2:3)) {
  df[[i]] = as.factor(df[[i]])
}

# Testing data
df.nrow <- nrow(df)
index.text <- sample(df.nrow,size = round(0.3*df.nrow))
df.test <- df[index.text,]
df.train <- df[-index.text,]

final.dataset <- data.frame(actual=df.test$output_yield)

# ------------------------------------------------------------------------------------
# GLM modelling
print("GLM model")
start.time <- Sys.time()
glm.model.gaussian <- glm(output_yield ~ .,data = df.train,family = 'gaussian')

build.end.time <- Sys.time()
build.time.taken.gaussian <- build.end.time - start.time

predicted.gaussian = predict.lm(glm.model.gaussian,newdata = df.test[c(-1)])
predict.end.time <- Sys.time()
predict.time.taken.gaussian <- predict.end.time - start.time

final.dataset <- cbind(final.dataset,predicted.gaussian)

# ------------------------------------------------------------------------------------
# SVM modelling
library(e1071)
print("SVM model")
start.time <- Sys.time()
model.svm2 <- svm(output_yield ~ .,data = df.train, kernel="radial")

build.end.time <- Sys.time()
build.time.taken.svm <- build.end.time - start.time

predicted.svm = predict(model.svm2,df.test)
predict.end.time <- Sys.time()
predict.time.taken.svm <- predict.end.time - start.time

final.dataset <- cbind(final.dataset,predicted.svm)


# ------------------------------------------------------------------------------------
# Random Forest modelling
print("Random Forest model")
library(randomForest)
start.time <- Sys.time()

model.randomForest <- randomForest(output_yield ~ .,data = df.train[-3])
build.end.time <- Sys.time()
build.time.taken.random <- build.end.time - start.time

predicted.random = predict(model.randomForest,df.test)
predict.end.time <- Sys.time()
predict.time.taken.random <- predict.end.time - start.time

final.dataset <- cbind(final.dataset,predicted.random)


# ------------------------------------------------------------------------------------
# neural network
print("Neural Network model")
df1 <- df

df1.factor <- as.data.frame(lapply(df1[c(2,3,4)], as.integer))
df1.continuous <- as.data.frame(lapply(df1[c(1,5:14)], as.numeric))
df2.Year_2532.min <- min(df1.factor$year)
df2.Year_2532 <- df1.factor$year - df2.Year_2532.min

df2.Regional_min <- min(df1.factor$reginal_zone)
df2.Regional <- df1.factor$reginal_zone - df2.Regional_min

df2.Province_min <-  min(df1.factor$province_code)
df2.Province <- df1.factor$province_code - df2.Province_min

df3 <- data.frame(Year_coded=df2.Year_2532,Region_coded=df2.Regional,Province_coded=df2.Province)

df3 <- cbind(df1.continuous,df3)

n = names(df3)
f <- as.formula(paste(n[1]," ~ ",paste(n[-1],collapse = " + ")))

set.seed(2)
indx <- sample(round(nrow(df3)*0.7))

# min max normalization
normalizeMinMax <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df3.minmax <- as.data.frame(lapply(df3,normalizeMinMax))
df4.train <- df3.minmax[indx,]
df4.test <- df3.minmax[-indx,]

library(neuralnet)
df4.test <- df3.minmax[-indx,]

start.time <- Sys.time()
model.neuralNetwork <- neuralnet(f,df4.train,hidden = c(2,10),linear.output = TRUE,stepmax = 1e6)

build.end.time <- Sys.time()
build.time.taken.neural <- build.end.time - start.time

predicted.neural = compute(model.neuralNetwork,df4.test[-1])

predict.end.time <- Sys.time()
predict.time.taken.neural <- predict.end.time - start.time

final.dataset <- cbind(final.dataset,predicted.neural$net.result)


# ------------------------------------------------------------------------------------
# print the time taken
cat("\n")
print(paste0(" Time to build Gaussian Model ",build.time.taken.gaussian))
print(paste0(' Time to predict the data ',predict.time.taken.gaussian))
cat("\n")
print(paste0(" Time to build SVM model ",build.time.taken.svm))
print(paste0(' Time to predict the data ',predict.time.taken.svm))
cat("\n")
print(paste0(" Time to build Random Forest model ",build.time.taken.random))
print(paste0(' Time to predict the data ',predict.time.taken.random))
cat("\n")
print(paste0(" Time to build Neural Network model ",build.time.taken.neural))
print(paste0(' Time to predict the data ',predict.time.taken.neural))
cat("\n")
print(">> Script ending now")