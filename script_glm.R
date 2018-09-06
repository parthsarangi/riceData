df <- read.csv("riceData.csv")
names(df)
str(df)

n <- names(df)
f = as.formula(paste(n[18],paste(n[-18],collapse = " + "),sep = " ~ "))

set.seed(2)
indx <- sample(round(nrow(df)*0.7))


dfTrain <- df[indx,]
dfTest <- df[-indx,]

m_glm <- lm(f, df)

output <- predict(object = m_glm,dfTest)

dim(output)


# Convert to Factor

df[[1]] <- as.factor(df[[1]])
df[[2]] <- as.factor(df[[2]])
df[[3]] <- as.factor(df[[3]])
str(df)

# Number of rows according to Province Code
for (i in levels(df[[3]])) {
  print(paste0("For province code >> ",i," << the rows in df are >> ",
         nrow(df[which(df$Province_Code == i),])," <<") )
}

# Number of rows according to Regional Code
for (i in levels(df[[2]])) {
  print(paste0("For Regional zone >> ",i," << the rows in df are >> ",
               nrow(df[which(df$Reginal_Zone == i),])," <<") )
}


# Number of rows according to Years
for (i in levels(df[[1]])) {
  print(paste0("For Year >> ",i," << the rows in df are >> ",
               nrow(df[which(df$Year == i),])," <<") )
}


#
# Beter to segregate data by Region code to get higher rows of data
#


# Convert to numeric

df1 <-read.csv("riceData.csv")
str(df1)

for(i in c(1:3)){
  df1[[i]] <- as.factor(df1[[i]])
}

for(i in c(4:18)){
  df1[[i]] <- as.numeric(df1[[i]])
}

str(df1)

n <- names(df1)
f = as.formula(paste(n[18],paste(n[c(-1:-3,-18)],collapse = " + "),sep = " ~ "))

set.seed(2)
indx <- sample(round(nrow(df1)*0.7))


df1Train <- df1[indx,]
df1Test <- df1[-indx,]



### Trying Province Code 750
df750 <- df[which(df$Province_Code == '750'),]

dim(df750)



glm(f,)


