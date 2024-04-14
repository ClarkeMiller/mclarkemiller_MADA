# R code for the tidy Tuesday exercise.

#Call a bunch of libraries.
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(broom)) 
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(readxl)) 
suppressPackageStartupMessages(library(dplyr)) 
suppressPackageStartupMessages(library(tidyr)) 
suppressPackageStartupMessages(library(skimr))
suppressPackageStartupMessages(library(gt))
suppressPackageStartupMessages(library(dslabs))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(gapminder))
suppressPackageStartupMessages(library(kableExtra))
suppressPackageStartupMessages(library(openxlsx))
suppressPackageStartupMessages(library(xlsx))
library(gt)
library(lubridate)
suppressPackageStartupMessages(library(parsnip))
suppressPackageStartupMessages(library(tune))
suppressPackageStartupMessages(library(recipes))
suppressPackageStartupMessages(library(workflows))
suppressPackageStartupMessages(library(yardstick))
suppressPackageStartupMessages(library(rsample))

#Load data.
data_location <- here::here("tidytuesday-exercise", "eclipse_total_2024.csv")
Total_2024 <- read.csv(data_location)

data_location <- here::here("tidytuesday-exercise", "eclipse_annular_2023.csv")
Annular_2023 <- read.csv(data_location)


#Explore Data
Total_2024_Tab <- glimpse(Total_2024)

#Save a rds version
save_data_location <- here::here("tidytuesday-exercise","Total_2024_Tab.rds")
saveRDS(Total_2024_Tab, file = save_data_location)

Annular_2023_Tab <- glimpse(Annular_2023)

#Save a rds version
save_data_location <- here::here("tidytuesday-exercise","Annular_2023_Tab.rds")
saveRDS(Annular_2023_Tab, file = save_data_location)



#Map data from both eclipses.
suppressPackageStartupMessages(library(tmap))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(mapview))
suppressPackageStartupMessages(library(webshot))


Total_2024A <- Total_2024 %>% select(state, name, lat, lon, eclipse_3)

Annular_2023A <- Annular_2023 %>% select(state, name, lat, lon, eclipse_3)

Combined_eclipse <- rbind(Total_2024A, Annular_2023A)

Comb_Eclipse_map <- mapview(Combined_eclipse, xcol = "lon", ycol = "lat", crs = 4269, grid = FALSE)

png_file <- "Comb_Eclipse_map.png"
mapshot(Comb_Eclipse_map, file = png_file)

mapshot(
  Comb_Eclipse_map,
  file = "Comb_Eclipse_map1.png")

#Find the common cities.

cities1 <- Total_2024 %>% select(name)
cities2 <- Annular_2023 %>% select(name)

# Find the common cities using intersect()
common_cities <- intersect(cities1, cities2)

# Print the common cities
print(common_cities)

save_data_location <- here::here("tidytuesday-exercise","common_cities.rds")
saveRDS(common_cities, file = save_data_location)

cities1 <- Total_2024 %>% select(name)
cities2 <- Annular_2023 %>% select(name)


# Find the common cities using intersect()
common_cities <- intersect(cities1, cities2)

common_cities$group <- rep(1:ceiling(nrow(resulttable)/10), each = 10, length.out = nrow(resulttable))

# Spread the cities across 10 columns based on the grouping
spread_table <- spread(resulttable, key = group, value = cities)

# Print the spread table
knitr::kable(spread_table)

save_data_location <- here::here("tidytuesday-exercise","common_cities.rds")
saveRDS(spread_table, file = save_data_location)


#Find the city with the longest total eclipse time.
Total_2024B <- Total_2024

Total_2024B <- Total_2024B %>%
  mutate(eclipse_3 = hms(eclipse_3),
         eclipse_4 = hms(eclipse_4))

longest_difference <- Total_2024B %>%
  mutate(time_difference = eclipse_4 - eclipse_3) %>%
  group_by(name) %>%
  summarize(max_difference = max(time_difference, na.rm = TRUE)) %>%
  arrange(desc(max_difference)) %>%
  slice(1)

#Print the name of the city.
selected_city <- longest_difference$name

# Filter the original data set for the selected city
selected_data <- Total_2024 %>%
  filter(name == selected_city)

# Print the selected data
save_data_location <- here::here("tidytuesday-exercise","selected_data.rds")
saveRDS(selected_data, file = save_data_location)


#Making models of the data.
#Here I am setting up models to test the dependence of longitude, latitude, or both
#on the length of totality.

library(lubridate)



#Load data.
data_location <- here::here("tidytuesday-exercise", "eclipse_total_2024.csv")
Total_2024 <- read.csv(data_location)

Total_2024B <- Total_2024

Total_2024B <- Total_2024B %>%
  mutate(eclipse_3 = hms(eclipse_3),
         eclipse_4 = hms(eclipse_4))

Total_2024B <- Total_2024B %>%
  mutate(ecl_num = eclipse_4 - eclipse_3)

Total_2024B$ecl_num <- as.numeric(Total_2024B$ecl_num)

lin_mod <- linear_reg() %>% set_engine("lm")
linfit1 <- lin_mod %>% fit(ecl_num ~ lon, data = Total_2024B)
linfit2 <- lin_mod %>% fit(ecl_num ~ lat, data = Total_2024B)
linfit3 <- lin_mod %>% fit(ecl_num ~ lon + lat, data = Total_2024B)


# Print the results
lmtable1 <- tidy(linfit1)
table_file1 <- here("tidytuesday-exercise","lmtable1.rds")
saveRDS(lmtable1, file = table_file1)

# Print the results
lmtable2 <- tidy(linfit2)
table_file2 <- here("tidytuesday-exercise","lmtable2.rds")
saveRDS(lmtable2, file = table_file2)

# Print the results
lmtable3 <- tidy(linfit3)
table_file3 <- here("tidytuesday-exercise","lmtable3.rds")
saveRDS(lmtable3, file = table_file3)



#OLD

fit1 <- broom::tidy(linfit1)
save_data_location <- here::here("tidytuesday-exercise","fit1.rds")
saveRDS(fit1, file = save_data_location)

fit2 <- broom::tidy(linfit1)
save_data_location <- here::here("tidytuesday-exercise","linfit2.rds")
saveRDS(linfit2, file = save_data_location)

fit3 <- broom::tidy(linfit1)
save_data_location <- here::here("tidytuesday-exercise","linfit3.rds")
saveRDS(linfit3, file = save_data_location)



library(broom)
library(knitr)
library(kableExtra)

# Print the results
lmtable1 <- tidy(linfit1)
table_file1 <- here("tidytuesday-exercise","lmtable1.rds")
saveRDS(lmtable1, file = table_file1)

# Extract coefficients and statistics from the model object
coefficients <- coef(resulttable)
summary_info <- summary(resulttable)

# Create a data frame with relevant information
tidy_result <- data.frame(
  Coefficients = names(coefficients),
  Estimate = coefficients,
  Std.Error = summary_info$coefficients[, "Std. Error"],
  t_value = summary_info$coefficients[, "t value"],
  p_value = summary_info$coefficients[, "Pr(>|t|)"]
)

# Create a styled table using kableExtra
styled_table <- kable(tidy_result, caption = "Linear Model Fit Table.") %>%
  kable_styling(full_width = FALSE)  # Adjust styling options as needed

# Save the styled table as a figure
save_kable(styled_table, here("tidytuesday-exercise", "lmtable1_figure.png"))



#OLD
# Load your linear regression model object
resulttable1 <- readRDS(here("tidytuesday-exercise", "lmtable1.rds"))

# Extract relevant information using broom::tidy()
tidy_result <- tidy(resulttable1)

# Create a styled table using kableExtra
styled_table <- kable(tidy_result, caption = "Linear model fit table.") %>%
  kable_styling(full_width = FALSE)  # Adjust styling options as needed

# Save the styled table as a figure
save_kable(styled_table, here("tidytuesday-exercise", "lmtable1_figure.png"))





# Print the results
lmtable1 <- print(linfit1)
table_file1 <- here("tidytuesday-exercise","lmtable1.rds")
saveRDS(lmtable1, file = table_file1)


fit1 <- broom::tidy(linfit1)
save_data_location <- here::here("tidytuesday-exercise","fit1.rds")
saveRDS(fit1, file = save_data_location)


save_data_location <- here::here("tidytuesday-exercise","linfit2.rds")
saveRDS(linfit2, file = save_data_location)


save_data_location <- here::here("tidytuesday-exercise","linfit3.rds")
saveRDS(linfit3, file = save_data_location)

