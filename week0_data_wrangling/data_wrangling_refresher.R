## Installing neccessary packages
packages <- c("here", "janitor", "tidyverse", "sf", "terra", "tmap", "spData", "spDataLarge", "geodata", "kableExtra", "viridisLite")
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Loading libraries
library(here) # find files and folders reliably within the project of your directory
library(janitor) # clean and standardize column names
library(tidyverse) # import, manipulate, summarize, and visuallize data
library(sf) # work with spatial and geographic data!!!
library(kableExtra) # create nicely formatted tables

# Read in csv
gdw_df <- read_csv("data/gdw.csv")|>
  clean_names() # Convert variable names to lower snake case

# Show the first and last 10 rows of gdw_df and use kable() to make nice HTML tables
head(gdw_df, 10) |> kable()
tail(gdw_df, 10) |> kable() 

# Read numerical dimensions, rows, columns, and column names
dim(gdw_df)
nrow(gdw_df)
ncol(gdw_df)

names(gdw_df)


# Use indexing brackets to extract the gdw_df column containing 
# country names as a vector and data frame
country_df <- gdw_df[, "country"] # can change the variable in "" to learn other info
country_vec <- gdw_df[["country"]]

# Use group_by() and summarise() to find the number of dams by dam type in gdw_df
gdw_df |> 
  group_by(dam_type) |>
  summarise(count = n()) |>
  ungroup()

# Make a subset called sub_dam that only contains entries for dam_type == "Dam"
sub_dam <- gdw_df |> 
  filter(dam_type == "Dam")

# Re-order gdw_df by ascending order of installation year
gdw_df <- gdw_df |> 
  arrange(year_dam)

# Make a bar graph of average height of dam/barrier in meters (dam_hgt_m) by country

gdw_df |>
  group_by(country) |>
  summarize(mean_dam_hgt_m = mean(dam_hgt_m, na.rm = TRUE)) |>
  ungroup() |> 
  ggplot(aes(x = country, y = mean_dam_hgt_m)) +
    geom_bar(stat = "identity") +
    labs(x = "Country",
         y = "Average height of dam/barrier in meters") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# Make a scatterplot of height of dam/barrier versus representative maximum storage capacity 
# of reservoir in million cubic meters (cap_mcm)

ggplot(data = gdw_df,
      aes(x = cap_mcm, y = dam_hgt_m)) +
  geom_point() +
  labs(x = "Storage capacity of reservoir in million cubic meters",
       y = "Height of dam/barrier in meters") +
  theme_minimal()

