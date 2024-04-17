library(tidyverse)
library(ggplot2)

setwd("C:/Users/kimbe/Desktop/Repo/eco395m-data-mining-project")

listing = read.csv("data/raw_data/listings2403.csv")


listing = listing %>%
  select(-c(listing_url, 
            scrape_id, 
            source, 
            picture_url, 
            host_url, 
            host_location, 
            host_thumbnail_url, 
            host_picture_url, 
            host_neighbourhood,
            neighbourhood, 
            bathrooms_text, 
            minimum_minimum_nights, 
            maximum_minimum_nights, 
            minimum_maximum_nights, 
            maximum_maximum_nights, 
            minimum_nights_avg_ntm, 
            maximum_nights_avg_ntm, 
            calendar_updated, 
            has_availability, 
            availability_30, 
            availability_60, 
            availability_90, 
            availability_365, 
            calendar_last_scraped, 
            license, 
            calculated_host_listings_count,
            calculated_host_listings_count_entire_homes,
            calculated_host_listings_count_private_rooms, 
            calculated_host_listings_count_shared_rooms, 
            number_of_reviews_ltm, 
            number_of_reviews_l30d, 
            host_since, 
            host_total_listings_count, 
            host_verifications, 
            property_type,
            reviews_per_month)
         )



# data cleaning and formatting

listing_clean = listing

listing_clean = listing_clean %>%
  mutate(id = as.character(id))

listing_clean = listing_clean %>%
  mutate(host_id = as.character(host_id))

listing_clean = listing_clean %>%
  mutate(host_response_time = na_if(host_response_time, "N/A"),
         host_response_time = na_if(host_response_time, ""),
         host_response_time = factor(host_response_time), 
         host_response_time = forcats::fct_drop(host_response_time))

listing_clean = listing_clean %>%
  mutate(host_acceptance_rate = str_remove(host_acceptance_rate, "%"), 
         host_acceptance_rate = as.numeric(host_acceptance_rate) / 100)

listing_clean = listing_clean %>%
  mutate(host_response_rate = str_remove(host_response_rate, "%"), 
         host_response_rate = as.numeric(host_response_rate) / 100)

listing_clean = listing_clean %>%
  mutate(host_is_superhost = na_if(host_is_superhost, ""),
         host_is_superhost = recode(host_is_superhost, "t" = 1, "f" = 0,),
         host_is_superhost = as.integer(host_is_superhost))

listing_clean = listing_clean %>%
  mutate(host_has_profile_pic = na_if(host_has_profile_pic, ""),
         host_has_profile_pic = recode(host_has_profile_pic, "t" = 1, "f" = 0),
         host_has_profile_pic = as.integer(host_has_profile_pic))

listing_clean = listing_clean %>%
  mutate(host_identity_verified = na_if(host_identity_verified, ""),
         host_identity_verified = recode(host_identity_verified, "t" = 1, "f" = 0),
         host_identity_verified = as.integer(host_identity_verified))

listing_clean = listing_clean %>%
  mutate(room_type = factor(room_type))

listing_clean = listing_clean %>%
  mutate(instant_bookable = na_if(instant_bookable, ""),
         instant_bookable = recode(instant_bookable, "t" = 1, "f" = 0,),
         instant_bookable = as.integer(instant_bookable))

listing_clean <- listing_clean %>%
  mutate(price = str_remove(price, "\\$"),
         price = str_replace_all(price, ",", ""),
         price = as.numeric(price))



# pick some medium frequency amenities, and create dummy variables for them

listing_temp = listing_clean %>%
  select(id, amenities)

listing_temp$amenities = lapply(listing_temp$amenities, 
                                 function(x) str_remove_all(x, "\\[|\\]|\""))

listing_temp = listing_temp %>%
  mutate(amenities = str_split(amenities, ",\\s*")) %>%
  unnest(amenities)

amenity_counts = listing_temp %>%
  group_by(amenities) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(count))

amenity_list <- c("Hair dryer", "Refrigerator", "Microwave", 
                  "Washer", "Pets allowed")

listing_clean$amenities <- lapply(listing_clean$amenities, function(x) {
  amenities_list <- str_remove_all(x, "\\[|\\]|\"")
  str_split(amenities_list, ",\\s*")[[1]]
})

for (amenity in amenity_list) {
  column_name = gsub(" ", "_", amenity) 
  listing_clean[[column_name]] = sapply(listing_clean$amenities, 
                                        function(x) as.integer(amenity %in% x))
}



# calculate time between last_scrape and first_review/last_review











# feature engineering
# key words from "name", "description", "neighborhood_review", "host_about"
# pick amenities
# calculate time between last_scrape and first/last review
