start.time <- Sys.time()

df = read.csv("riceData.csv")

for (i in c(1:3)) {
  df[[i]] = as.factor(df[[i]])
}

# Testing data
df.nrow <- nrow(df)
index.text <- sample(df.nrow,size = round(0.3*df.nrow))
df.test <- df[index.text,]


glm.model.gaussian <- glm(yield_pa_area ~ .,data = df,family = 'gaussian')

predicted.gaussian = predict.lm(glm.model.gaussian,newdata = df.test[c(-18)])

df.results = data.frame(actual=df.test$yield_pa_area,predicted.gaussian)
end.time <- Sys.time()
time.taken <- end.time - start.time

