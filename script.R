setwd("~/Documents/riceData")

df1 <- read.csv("riceData.csv")

names(df1)

head(df1)

str(df1)

# fixing the factor fiels to numeric
df1$Planted_Area <- as.numeric(df1$Planted_Area)
df1$Harvested_Area <- as.numeric(df1$Harvested_Area)
df1$Production_Ton <- as.numeric(df1$Production_Ton)
df1$Sale_Price <- as.numeric(df1$Sale_Price)
df1$Highest_Price <- as.numeric(df1$Highest_Price)
df1$Lowest_Price <- as.numeric(df1$Lowest_Price)
df1$Peak_Price <- as.numeric(df1$Peak_Price)

df1$Yield_PA_Area <- as.numeric(df1$Yield_PA_Area)
df1$Compitation_Price <- as.numeric(df1$Compitation_Price)

df1$Fertilizer1_Price <- as.numeric(df1$Fertilizer1_Price)
df1$Fertilizer2_Price <- as.numeric(df1$Fertilizer2_Price)
df1$Fertilizer3_Price <- as.numeric(df1$Fertilizer3_Price)

df1$Cost <- as.numeric(df1$Cost)

str(df1)

# Saving after data processing
write.csv(df1,'ricedata_numeric.csv',row.names = FALSE,quote = TRUE)


# reading into new df
df2 <- read.csv('ricedata_numeric.csv')

names(df2)

df2_scaled <- df2[c(4:6,8:17)]

df2_scaled <- as.data.frame(scale(df2_scaled))

head(df2_scaled)

df2_final <- cbind(df2[c(1,2,3,7)],df2_scaled,df2[18])

names(df2_final)
str(df2_final)

n <- names(df2_final)

f <- as.formula(paste(n[18]," ~ ",paste(n[-18],collapse = " + ")))
n
f

nn1 <- neuralnet::neuralnet(f,data = df2_final,hidden = c(10,5),linear.output = TRUE)
plot(nn1)

saveRDS(nn1,file = "neuralNet_10_5.RDS")

write.csv(df2_final,"riceData_scaled.csv",quote = TRUE,row.names = FALSE)


# read scaled data 
df4 <- read.csv("riceData_scaled.csv")

nn1 <- readRDS("neuralNet_10_5.RDS")

nrow(df4)

set.seed(2)
indx <- sample(round(nrow(df4)*0.3))

testingData <- df4[indx,]
dim(df4)
dim(testingData)

actual_label <- testingData[18]

nrow(actual_label)

head(actual_label)

head(testingData)

library(neuralnet)


n <- names(df4)
f <- as.formula(paste(n[18]," ~ ",paste(n[-18],collapse = " + ")))

df2_final <- df4
df2_final$Year <- as.factor(df2_final$Year)
df2_final$Reginal_Zone <- as.factor(df2_final$Reginal_Zone)
df2_final$Province_Code <- as.factor(df2_final$Province_Code)
df2_final$Yield_HA_Area <- as.factor(df2_final$Yield_HA_Area)

df2_final <- as.matrix(df2_final)

nn1 <- neuralnet(f,data = df2_final,hidden = c(10,5),linear.output = TRUE)

testingData_n <- as.matrix(testingData[-18])
dim(testingData_n)

computed_labels <- compute(nn1,testingData_n)

length(computed_labels$net.result)

d <- as.data.frame(computed_labels$net.result)

df_final <- cbind(testingData,d)
names(df_final)[19] <- "computed_label"


df2_final <- as.matrix(df4[c(-1:-4)])


n <- names(df4[c(-1:-4)])
f <- as.formula(paste(n[14]," ~ ",paste(n[-14],collapse = " + ")))
nn1 <- neuralnet(f,data = df2_final,hidden = c(10,5),linear.output = TRUE)

set.seed(2)
indx <- sample(round(nrow(df4[c(-1:-4)])*0.3))
testingData <- df4[c(-1:-4)][indx,]

actual_label <- testingData[14]
testingData_n <- as.matrix(testingData[-14])

computed_labels <- compute(nn1,testingData_n)

