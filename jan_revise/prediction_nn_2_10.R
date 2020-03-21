df = read.csv("ricedata_jan.csv")

df.col = ncol(df)


for (i in c(2:3)) {
  df[[i]] = as.factor(df[[i]])
}

# Testing data
df.nrow <- nrow(df)
index.text <- sample(df.nrow,size = round(0.3*df.nrow))
df.test <- df[index.text,]
df.train <- df[-index.text,]

final.dataset <- data.frame(actual=df.test$output_yield)

df1 <- df

df1.factor <- as.data.frame(lapply(df1[c(2,3,4)], as.integer))
df1.continuous <- as.data.frame(lapply(df1[c(1,5:df.col)], as.numeric))

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

denormMinMax <- function(x) {
  return()
}


df3.minmax <- as.data.frame(lapply(df3,normalizeMinMax))
df4.train <- df3.minmax[indx,]
df4.test <- df3.minmax[-indx,]

#storing the min max values in a list
list_length <- ncol(df3)
minList <- vector(mode = "list", length = list_length)
maxList<- vector(mode = "list", length = list_length)

colN = 1
for (val in minList) {
  minList[colN] <- min(df3[[colN]])
  maxList[colN] <- max(df3[[colN]])
  colN <- colN + 1
}


