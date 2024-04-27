setwd("C:/Users/kimbe/Desktop/Repo/eco395m-data-mining-project")

library(tidyverse)
library(ggplot2)
library(ggmap)
library(kableExtra)

listing_clean = read.csv("data/listing_clean.csv")



# Listing price summary
# Remove outliers

summary(listing_clean$price)
quantile(listing_clean$price, c(0.005, 0.995)) 

top_prices = listing_eda %>%
  select(id, name, price) %>%
  arrange(desc(price)) %>%
  slice(1:50)

bottom_prices = listing_eda %>%
  select(id, name, price) %>%
  arrange(price) %>%
  slice(1:50)

listing_map = filter(listing_clean, price <= 1999 & price >= 30)  # keep 99% prices
summary(listing_map$price)


# Listing price on map

register_google(key = "GOOGLE_MAPS_APIKEY", write = TRUE)

nyc_map = get_map(location = c(lon = -73.935242, lat = 40.730610), 
                  zoom = 11, 
                  maptype = "roadmap")

ggmap(nyc_map) +
  geom_point(data = listing_map, 
             aes(x = longitude, y = latitude, color = price), 
             size = 0.5, 
             alpha = 0.5) +
  scale_color_gradient(low = "blue", high = "red", name = "Price") +
  labs(title = "Airbnb Listing Price in NYC", x = "Longitude", y = "Latitude") +
  theme_minimal()


# Count number of listings by neighborhood (borough)

count_neighborhood = table(listing_map$neighbourhood_group_cleansed)
count_neighborhood = as.data.frame(count_neighborhood)
colnames(count_neighborhood) = c("Borough", "Count")
knitr::kable(count_neighborhood)


# Price by room type

ggplot(listing_map, aes(x = room_type, y = price)) +
  geom_boxplot(outlier.shape = 1, fill = "skyblue", colour = "navy") +
  labs(title = "Listing Price by Room Type", x = "Room Type", y = "Price") +
  theme_minimal() +
  theme(legend.position = "none")


# Price by minimum nights
# Above 2/3 of listings are long-term (>=30 days). Long-term listings are cheaper.
# Maximum nights much more dispersed. Some indication of 30/90 but not apparent overall.

table(listing_clean$minimum_nights)
table(listing_clean$maximum_nights)

count_min_nights = listing_map %>%
  select(price, minimum_nights) %>%
  mutate(
    min_night = case_when(
      minimum_nights < 7 ~ "Less than a week",
      minimum_nights >= 7 & minimum_nights < 30 ~ "One week to one month",
      minimum_nights >= 30 & minimum_nights < 90 ~ "One month to three months",
      minimum_nights >= 90 ~ "More than three months"
    ),
    min_night = factor(min_night, 
                       levels = c("Less than a week", 
                                  "One week to one month", 
                                  "One month to three months", 
                                  "More than three months"))
  ) %>%
  group_by(min_night) %>%
  summarize(
    count = n(), 
    mean_price = round(mean(price, na.rm = TRUE), 2), 
    median_price = median(price, na.rm = TRUE),
    .groups = 'drop'
  )

knitr::kable(count_min_nights,
             col.names = c("Minimum number of nights", "Count", "Average price", "Median price"),
             caption = "Price by minimum number of nights")



# Distribution of ratings

listing_rating = listing_map %>%
  select(starts_with("review_scores_")) %>%
  pivot_longer(
    cols = c("review_scores_accuracy", "review_scores_cleanliness", 
             "review_scores_checkin", "review_scores_communication",
             "review_scores_location", "review_scores_value"), 
    names_to = "rating_type", 
    values_to = "score"
  )

rating_labels = c(
  review_scores_accuracy = "Accuracy",
  review_scores_cleanliness = "Cleanliness",
  review_scores_checkin = "Check-in",
  review_scores_communication = "Communication",
  review_scores_location = "Location",
  review_scores_value = "Value"
)

ggplot(listing_rating, aes(x = score)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "navy") +
  facet_wrap(~ rating_type, ncol = 3, labeller = labeller(rating_type = rating_labels)) +
  labs(title = "Distribution of Ratings", x = "Rating", y = "Count") +
  theme_minimal()




# Other individual predictors (not used in writeup)

# host response time
count_host_response = listing_map %>%
  group_by(host_response_time) %>%
  summarise(count = n())

ggplot(count_host_response, aes(x = host_response_time, y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "Frequency of Host Response Times", x = "Host Response Time", y = "Frequency")

# host response rate, acceptance rate, listings count
ggplot(listing_map, aes(x = host_response_rate)) +
  geom_histogram()

ggplot(listing_map, aes(x = host_acceptance_rate)) +
  geom_histogram()

ggplot(listing_map, aes(x = host_listings_count)) +
  geom_histogram()

# number of reviews
ggplot(listing_map, aes(x = number_of_reviews)) +
  geom_histogram()

# number of rooms
table(listing_map$accommodates)
table(listing_map$bedrooms)
table(listing_map$bathrooms)
table(listing_map$beds)

# review time
ggplot(listing_map, aes(x = days_first_review)) +
  geom_histogram()

ggplot(listing_map, aes(x = days_last_review)) +
  geom_histogram()