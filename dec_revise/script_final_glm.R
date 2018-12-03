df = read.csv("riceData.csv")

for (i in c(1:3)) {
  df[[i]] = as.factor(df[[i]])
}

# Testing data
df.nrow <- nrow(df)
index.text <- sample(df.nrow,size = round(0.3*df.nrow))
df.test <- df[index.text,]
df.train <- df[-index.text,]

# ------------------------------------------------------------------------------------
# GLM modelling
start.time <- Sys.time()
glm.model.gaussian <- glm(yield_pa_area ~ .,data = df.train,family = 'gaussian')

build.end.time <- Sys.time()
build.time.taken.gaussian <- build.end.time - start.time

predicted.gaussian = predict.lm(glm.model.gaussian,newdata = df.test[c(-11)])
predict.end.time <- Sys.time()
predict.time.taken.gaussian <- predict.end.time - start.time

final.dataset <- data.frame(actual=df.test$yield_pa_area,predicted.gaussian)


# ------------------------------------------------------------------------------------
# SVM modelling
library(e1071)
start.time <- Sys.time()
model.svm2 <- svm(yield_pa_area ~ .,data = df.train, kernel="radial")

build.end.time <- Sys.time()
build.time.taken.svm <- build.end.time - start.time

predicted.svm = predict(model.svm2,df.test)
predict.end.time <- Sys.time()
predict.time.taken.svm <- predict.end.time - start.time

final.dataset <- cbind(final.dataset,predicted.svm)


# ------------------------------------------------------------------------------------
# Random Forest modelling
start.time <- Sys.time()

build.end.time <- Sys.time()
build.time.taken.random <- build.end.time - start.time

predict.end.time <- Sys.time()
predict.time.taken.random <- predict.end.time - start.time

final.dataset <- cbind(final.dataset,predicted.random)


# ------------------------------------------------------------------------------------
# neural network

start.time <- Sys.time()

build.end.time <- Sys.time()
build.time.taken.neural <- build.end.time - start.time

predict.end.time <- Sys.time()
predict.time.taken.neural <- predict.end.time - start.time

final.dataset <- cbind(final.dataset,predicted.neural)


# ------------------------------------------------------------------------------------
# print the time taken
print(paste0(" Time to build model ",build.time.taken.gaussian))
print(paste0(' Time to predict the data ',predict.time.taken.gaussian))
print(paste0(" Time to build model ",build.time.taken.svm))
print(paste0(' Time to predict the data ',predict.time.taken.svm))
print(paste0(" Time to build model ",build.time.taken.random))
print(paste0(' Time to predict the data ',predict.time.taken.random))
print(paste0(" Time to build model ",build.time.taken.neural))
print(paste0(' Time to predict the data ',predict.time.taken.neural))

