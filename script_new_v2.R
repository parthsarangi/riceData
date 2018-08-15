setwd('Documents/rice_prediction/')

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

df4.test <- df3.minmax[-indx,]


k=1

val_time <- data.frame(time_val = c(1))

for(i in c(1:10)){
  for(j in c(1:10)){
    print(paste0("neural ",i,j))
    start.time <- Sys.time()
    
    neuralNetwork <- neuralnet(f,df4.train,hidden = c(i,j),linear.output = TRUE,stepmax = 1e6)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    
    val_time <- rbind(val_time,as.numeric(time.taken))
    
    computed_labels <- compute(neuralNetwork,df4.test[c(1:17)])
    df4.test <- cbind(df4.test,as.data.frame(computed_labels$net.result))

  }
}

k=19

for(i in c(1:4)){
  for(j in c(1:2)){ 
    colnames(df4.test)[[k]] = paste0("neural_",i,"_",j) 
    k=k+1
  }
}

write.csv(df4.test,"testing_data.csv",row.names = FALSE,quote=TRUE)


#-----------------------End
