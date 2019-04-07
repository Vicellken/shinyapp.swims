# convert csv file to rds file and save it called 'df'

df <- read.csv('uni_sci_year.csv')
saveRDS(df, file = "uni_sci_year.rds")
df <- readRDS("uni_sci_year.rds")

# remove duplicated data entry
new_uni <- uni[!duplicated(uni), ]

# create csv file
write.csv(df, file = "df.csv")