getwd()
setwd("glm/")

# riceData is ready to use and properly formatted

df = read.csv("riceData.csv")

names(df)
str(df)

for (i in c(1:3)) {
  df[[i]] = as.factor(df[[i]])
}

str(df)

write.csv(df,"riceDataFactored.csv",row.names = FALSE)


# Testing data
df.nrow <- nrow(df)
index.text <- sample(df.nrow,size = round(0.3*df.nrow))
df.test <- df[index.text,]

names(df.test)
dim(df.test)

# Fit the GLM model

glm.model.gaussian <- glm(yield_pa_area ~ .,data = df,family = 'gaussian')
glm.model.poisson <- glm(yield_pa_area ~ .,data = df,family = 'poisson')
glm.model.quasi <- glm(yield_pa_area ~ .,data = df,family = 'quasi')

# Cannot prepare model Inverse Gaussian
# Error: no valid set of coefficients has been found: please supply starting values
# In addition: Warning message:
#   In sqrt(eta) : NaNs produced
glm.model.inverse.gaussian <- glm(yield_pa_area ~ .,data = df,family = 'inverse.gaussian')

# Error in Model
# Error in eval(family$initialize) : y values must be 0 <= y <= 1
glm.model.binomial <- glm(yield_pa_area ~ .,data = df,family = 'binomial')



# Value from LM
predicted.gaussian = predict.lm(glm.model.gaussian,newdata = df.test[c(-18)])
predicted.poission = predict.lm(glm.model.poisson,newdata = df.test[c(-18)])
predicted.quasi = predict.lm(glm.model.quasi,newdata = df.test[c(-18)])


#Check lengths of outputs
length(predicted.gaussian)
length(predicted.poission)
length(predicted.quasi)

#
df.results = data.frame(actual=df.test$yield_pa_area,predicted.gaussian,predicted.poission,predicted.quasi)

str(df.results)
write.csv(df.results,"glm_outputs.csv")



