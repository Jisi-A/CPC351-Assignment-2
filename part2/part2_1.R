# Using  appropriate  visuals,  show  the  number  of  trips  aggregated
# for  each  month  for  all  the  13 public transport services across the
# country.
df <- read.csv("Data/ridership_headline.csv")

head(df)

sum(is.na(df))
na_count <- sum(apply(df, 1, function(row) any(is.na(row))))
na_count