# Using  appropriate  visuals,  show  the  number  of  vehicles
# registered  at  each  state  JPJ  office (including the Rakan Niaga)
# for the years of 2019, 2020, 2021, 2022, 2023, and 2024.

file_path <- "Data/"

# Get list of CSV files using relative path
file_list <-
  list.files(path = file_path, pattern = "^cars.*\\.csv$", full.names = TRUE)

print(file_list)

# Read all CSV files into separate dataframes
data_frames <- lapply(file_list, read.csv)

# Assign names to each dataframe based on the file names
names(data_frames) <- basename(file_list)

# Print the names of the dataframes
print(names(data_frames))

# Check for null values in all dataframes
null_values <- sapply(data_frames, function(df) sum(is.na(df)))

# Print the number of null values in each dataframe
print(null_values)

# Sum the number of rows in all data frames
total_rows <- sum(sapply(data_frames, nrow))

print(total_rows)

# Get the number of rows for each dataframe
num_rows <- sapply(data_frames, nrow)

# Create a bar plot of the number of rows in each CSV file
barplot(num_rows,
  main = "Number of Registered Vehicle Each Year",
  xlab = "Year",
  ylab = "Number of Registered Vehicles",
  names.arg = names(data_frames),
  las = 2,
  col = "lightblue"
)