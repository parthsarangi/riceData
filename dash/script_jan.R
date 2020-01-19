library(dplyr)

df <- read.csv('ricedata_jan.csv')
names(df)
str(df)

province_code <- factor(df$province_code)
length(province_code)
levels(province_code)


for (pvc_code in province_code) {
  len <- length(df$output_yield[df$province_code == pvc_code])
  print(paste0("Province code :", pvc_code, " Points in data vector :", len))
}

length(df$output_yield[df$reginal_zone == 12][df$province_code == 10])

plot(df$output_yield,df$planted_area)
plot(df$output_yield,df$harvested_area)
plot(df$production_ton,df$output_yield)
plot(df$yield_ha_area,df$output_yield)

