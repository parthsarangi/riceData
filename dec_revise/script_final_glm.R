df = read.csv("riceData.csv")

for (i in c(1:3)) {
  df[[i]] = as.factor(df[[i]])
}

# Testing data
df.nrow <- nrow(df)
index.text <- sample(df.nrow,size = round(0.3*df.nrow))
df.test <- df[index.text,]

start.time <- Sys.time()
glm.model.gaussian <- glm(yield_pa_area ~ .,data = df,family = 'gaussian')
build.end.time <- Sys.time()
build.time.taken <- build.end.time - start.time

predicted.gaussian = predict.lm(glm.model.gaussian,newdata = df.test[c(-11)])
predict.end.time <- Sys.time()
predict.time.taken <- predict.end.time - start.time

print(paste0(" Time to build model ",build.time.taken,'  Time to predict the data ',predict.time.taken))
