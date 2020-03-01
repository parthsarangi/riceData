ricedf = read.csv("ricedata_jan.csv")

summary(ricedf)

# subset only 5 years from ricedf
ricedf_subset <- subset(ricedf, ricedf$year >= 2555)
dim(ricedf_subset)
write.csv(x = ricedf_subset,file = "ricedata_subset.csv")

uniqueProvinces <- unique(ricedf_subset$province_code)

plot(x = ricedf_subset$year, y = ricedf_subset$planted_area)
