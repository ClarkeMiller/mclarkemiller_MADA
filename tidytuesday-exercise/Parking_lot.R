#Parking lot.

# Compute the RMSE and R squared for model 1
metrics_1 <- linfit1 %>% 
  predict(longest_difference1) %>% 
  bind_cols(longest_difference1) %>% 
  metrics(truth = time_difference, estimate = .pred)

# Compute the RMSE and R squared for model 2
metrics_2 <- linfit2 %>% 
  predict(longest_difference1) %>% 
  bind_cols(longest_difference1) %>% 
  metrics(truth = time_difference, estimate = .pred)

# Compute the RMSE and R squared for model 3
metrics_3 <- linfit3 %>% 
  predict(longest_difference1) %>% 
  bind_cols(longest_difference1) %>% 
  metrics(truth = time_difference, estimate = .pred)

# Print the results
tidy(linfit1)
print(metrics_1)
tidy(linfit2)
print(metrics_2)
tidy(linfit3)
print(metrics_3)



#Making models of the data.
#Here I am setting up models to test the dependence of longitude, latitude, or both
#on the length of totality.

#Load data.
data_location <- here::here("tidytuesday-exercise", "eclipse_total_2024.csv")
Total_2024 <- read.csv(data_location)

Total_2024B <- Total_2024

Total_2024B <- Total_2024B %>%
  mutate(eclipse_3 = hms(eclipse_3),
         eclipse_4 = hms(eclipse_4))

longest_difference1 <- Total_2024B %>%
  mutate(time_difference = eclipse_4 - eclipse_3)



glimpse(longest_difference1)

lin_mod <- linear_reg() %>% set_engine("lm")
linfit1 <- lin_mod %>% fit(time_numeric ~ lon, data = longest_difference1)
linfit2 <- lin_mod %>% fit(time_numeric ~ lat, data = longest_difference1)
linfit3 <- lin_mod %>% fit(time_numeric ~ lon + lat, data = longest_difference1)

print(linfit1)




