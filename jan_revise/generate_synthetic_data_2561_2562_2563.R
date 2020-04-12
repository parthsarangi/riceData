df = read.csv("ricedata_jan.csv")
str(df)

df_names <- names(df)
df_names

# get uniques province list
provList <- unique(df$province_code)

#Generate output yield of 0 values
op_yield <- rep(0,each=3,times=77)
# Generate years list
yrList <- rep(c(2561:2563),each=77,times=1)
# Generate province code
regionCode <- unique(df$reginal_zone)


df_op <- cbind(output_yield=op_yield,year=yrList)
for (i in c(3:14)) {
  #i = 5
  df_op <- cbind(df_op,data.frame(rep(1,each=77,times=3)))
  names(df_op)[i] = df_names[i]
}

# replace provCode and regionalCode into df_op_sub
#df_op[[4]] <- as.list(rep(provList,each=1,times=3))
df_op[[4]] <- rep(provList,each=1,times=3)


for(provCode in provList){
  #provCode = 760
  
  # select subset of df_op where year matches
  df_new <- df_op[df_op$province_code == provCode,]
  
  # get the region zone and replace into data subset
  regCode <- unique(df[df$province_code == provCode,3])
  #df_new[[3]] <- as.list(rep(regCode,each=1,times=3))
  df_new[[3]] <- rep(regCode,each=1,times=3)

  # make subset of df with province_code
  df_sub <- df[df$province_code == provCode,]

    for(i in c(5:14)){
    meanVal <- mean(df_sub[[i]])
    sdeviation <- round(sd(x = df_sub[[i]]),digits = 2)
    # generate random values for that column
    varList <- rnorm(3,mean=meanVal,sd=sdeviation)
    names(varList) <- df_names[i]
    df_new[[i]] <- varList
    }
  df_op[df_op$province_code == provCode,] <- df_new
}

#write the generated output values to csv file
write.csv(df_op,"synthetic_random_generated_output.csv",row.names = FALSE)

