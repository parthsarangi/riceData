# ----------------------------------------------------------

#normalization function
normalizeMinMax <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


# Load pred dataset
df_pred = read.csv(file = "./predic_rice_data.csv")
df_pred = read.csv(file = "./predic_rice_data_2560.csv")

# Load nn rds file
neuralModel <- readRDS(file = "../neuralNet2_10.RDS")
df.col = ncol(df_pred)
# perform data normalization
df_pred.factor <- as.data.frame(lapply(df_pred[c(2,3,4)], as.integer))
df_pred.continuous <- as.data.frame(lapply(df_pred[c(1,5:df.col)], as.numeric))
df2.Year_2532.min <- min(df_pred.factor$year)
df2.Year_2532 <- df_pred.factor$year - df2.Year_2532.min
df2.Regional_min <- min(df_pred.factor$reginal_zone)
df2.Regional <- df_pred.factor$reginal_zone - df2.Regional_min
df2.Province_min <-  min(df_pred.factor$province_code)
df2.Province <- df_pred.factor$province_code - df2.Province_min
df3 <- data.frame(Year_coded=df2.Year_2532,Region_coded=df2.Regional,Province_coded=df2.Province)
df3 <- cbind(df_pred.continuous,df3)
df3.minmax <- as.data.frame(lapply(df3,normalizeMinMax))

# predict yield
library(neuralnet)
#predicted.yield = compute(neuralModel,df3.minmax[-1])
predicted.yield = compute(neuralModel,df3[-1])

# perform denorm on yield 
df_original <- read.csv("ricedata_jan.csv")

min_yield = min(df_original$output_yield)
max_yield = max(df_original$output_yield)

denormMinMax = function(x) {
  return(x*(max_yield - min_yield) + min_yield)
}

predicte_yield = unlist(predicted.yield$net.result)


denorm_yield = lapply(predicted.yield$net.result, denormMinMax)
denorm_yield


# save in file
write.csv(x = unlist(denorm_yield), file = "./predicted_yield.csv",quote = FALSE,row.names = FALSE)
#write.csv(x = unlist(denorm_yield), file = "./predicted_yield_2560.csv",quote = FALSE,row.names = FALSE)


# ------------------------------------------------------------------------------------------------------------------------
# prediction with glm model (works)
modelGLM = readRDS("glm_regression.RDS")
df_pred = read.csv(file = "predic_rice_data.csv")

str(df_pred)

predict_yield = predict.glm(modelGLM, newdata = df_pred[-1])
predict_yield = predict(modelGLM, newdata = df_pred[-1],se.fit = FALSE)

write.csv(x = predict_yield, file = "glm_predicted_output.csv",row.names = FALSE)

plot(modelGLM)



# ------------------------------------------------------------------------------------------------------------------------
# prediction with randomForests (works)
modelRandomForest = readRDS("randomForest_model.RDS")
df_pred = read.csv(file = "predic_rice_data.csv")

predict_yield = predict(modelRandomForest, newdata = df_pred[-1])

write.csv(x = predict_yield, file = "randomForest_predicted_output.csv",row.names = FALSE)

plot(modelRandomForest)


# ------------------------------------------------------------------------------------------------------------------------


