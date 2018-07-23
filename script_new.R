setwd("~/Documents/riceData")

df1 <- read.csv("riceData.csv")

#-------------------- data wrangling
# seperate factor variabales
df1.factor <- as.data.frame(lapply(df1[c(1,2,3)], as.integer))
df1.continuous <- as.data.frame(lapply(df1[c(4:18)], as.numeric))

# check levels
levels(df1.factor$Year)

# names of factor variables
names(df1.factor)
#[1] "Year"          "Reginal_Zone"  "Province_Code"

# Encoding Year
df2.Year_2532.min <- min(df1.factor$Year)
df2.Year_2532 <- df1.factor$Year - df2.Year_2532.min

df2.Regional_min <- min(df1.factor$Reginal_Zone)
df2.Regional <- df1.factor$Reginal_Zone - df2.Regional_min

df2.Province_min <-  min(df1.factor$Province_Code)
df2.Province <- df1.factor$Province_Code - df2.Province_min


df3 <- data.frame(Year_coded=df2.Year_2532,Region_coded=df2.Regional,Province_coded=df2.Province)

df3 <- cbind(df3,df1.continuous)

write.csv(df3,"riceData_coded.csv",quote = TRUE,row.names = FALSE)

n = names(df3)
f <- as.formula(paste(n[18]," ~ ",paste(n[-18],collapse = " + ")))

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

neuralNetwork <- neuralnet(f,df4.train,hidden = c(10,4),linear.output = TRUE)

computed_labels <- compute(neuralNetwork,df4.test[-18])

df4.test <- cbind(df4.test,Predicted=as.data.frame(computed_labels$net.result))

write.csv(df4.test,"testing_data.csv",row.names = FALSE,quote=TRUE)
saveRDS(neuralNetwork,"neuralnet104.RDS")
#-------------------- data wrangling



df2 <- lapply(df1[c(4:18)],as.numeric)

n = names(df2)
f <- as.formula(paste(n[15]," ~ ",paste(n[-15],collapse = " + ")))

df3 = as.data.frame(lapply(df2,  scale))
df3.matrix = as.matrix(df3)

library(nnet)
library(neuralnet)

nn.fit = neuralnet(f,data = df3,hidden=c(8,2),linear.output = TRUE,stepmax=1e6)
saveRDS(nn.fit,file = "neuralnet_8,2.RDS")


set.seed(2)
indx <- sample(round(nrow(df4)*0.3))

testingData <- df3[indx,]
actual_value = testingData[15]

predicted_val = compute(nn.fit,testingData)
final = cbind(testingData,actual_value,predicted_val$net.result)

write.csv(final,"regression.csv",quote = TRUE,row.names = FALSE)

plot(nn.fit)
