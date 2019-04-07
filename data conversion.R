# convert csv file to rds file and save it called 'df'

df <- read.csv('df.csv')
saveRDS(df, file = "df_new.rds")
df_new <- readRDS("df_new.rds")

# remove duplicated data entry
new_uni <- uni[!duplicated(uni), ]

# create csv file
write.csv(new_uni, file = "new_uni.csv")
