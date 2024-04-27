library(tidyverse)
library(lubridate)
library(tm)
library(tidytext)
library(geosphere)


setwd("C:/Users/kimbe/Desktop/Repo/eco395m-data-mining-project")

listing2403 = read.csv("data/raw_data/listings2403.csv")
listing2402 = read.csv("data/raw_data/listings2402.csv")
listing2401 = read.csv("data/raw_data/listings2401.csv")
listing2312 = read.csv("data/raw_data/listings2312.csv")
listing2311 = read.csv("data/raw_data/listings2311.csv")
listing2310 = read.csv("data/raw_data/listings2310.csv")

listing = bind_rows(listing2403, listing2402, listing2401, 
                    listing2312, listing2311, listing2310)

listing = listing %>% mutate(id = as.character(id))
listing = distinct(listing, id, .keep_all = TRUE)

write.csv(listing, file = "data/listing.csv", row.names = FALSE, 
          quote = TRUE, fileEncoding = "UTF-8")



# Select variables
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


# Do not include hotel rooms
listing = filter(listing, room_type!="Hotel room")



# Data cleaning and formatting

listing_clean = listing

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

listing_clean = listing_clean %>%
  mutate(price = str_remove(price, "\\$"),
         price = str_replace_all(price, ",", ""),
         price = as.numeric(price))



# Feature engineering

# Find popular amenities and create dummy variables for them

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

amenity_list = c("Refrigerator", "Microwave", "Washer", "Pets allowed", 
                 "Extra pillows and blankets")

listing_clean$amenities = lapply(listing_clean$amenities, function(x) {
  amenities_list = str_remove_all(x, "\\[|\\]|\"")
  str_split(amenities_list, ",\\s*")[[1]]
})

for (amenity in amenity_list) {
  column_name = gsub(" ", "_", amenity) 
  listing_clean[[column_name]] = sapply(listing_clean$amenities, 
                                        function(x) as.integer(amenity %in% x))
}

listing_clean = listing_clean %>%
  select(-amenities) %>%
  rename(refrigerator = Refrigerator,
         microwave = Microwave,
         washer = Washer, 
         pets_allowed = Pets_allowed, 
         extra_pillows_and_blankets = Extra_pillows_and_blankets)


# Calculate time between last_scrape and first_review/last_review
listing_clean = listing_clean %>%
  mutate(
    last_scraped = ymd(last_scraped),
    first_review = ymd(first_review),
    last_review = ymd(last_review),
    days_first_review = as.integer(last_scraped - first_review),
    days_last_review = as.integer(last_scraped - last_review)
  ) %>%
  select(-c(first_review, last_review))


# Length of text from "neighborhood_overview", "host_about"

listing_clean = listing_clean %>%
  mutate(len_neighborhood_overview = nchar(neighborhood_overview),
         len_host_about = nchar(host_about)) %>%
  select(-c(neighborhood_overview, host_about))


# Extract common keywords in "name", and create dummies for them

text_corpus = Corpus(VectorSource(listing_clean$name))

text_corpus = tm_map(text_corpus, content_transformer(tolower))
text_corpus = tm_map(text_corpus, removePunctuation)
text_corpus = tm_map(text_corpus, removeNumbers)
text_corpus = tm_map(text_corpus, content_transformer(stripWhitespace))
text_corpus = tm_map(text_corpus, removeWords, stopwords("en"))

text_corpus = tm_filter(text_corpus, 
                        function(x) length(unlist(strsplit(as.character(x), " "))) > 0)

dtm = TermDocumentMatrix(text_corpus)
m = as.matrix(dtm)
word_freqs = sort(rowSums(m), decreasing = TRUE)
word_freqs_df = data.frame(word = names(word_freqs), freq = word_freqs)
#head(word_freqs_df, 20)

listing_clean = listing_clean %>%
  mutate(
    cozy_name = as.integer(str_detect(name, regex("cozy", ignore_case = TRUE))),
    spacious_name = as.integer(str_detect(name, regex("spacious", ignore_case = TRUE)))
  )



# Extract common keywords in "description", and create dummies for them

text_corpus = Corpus(VectorSource(listing_clean$description))

text_corpus = tm_map(text_corpus, content_transformer(tolower))
text_corpus = tm_map(text_corpus, removePunctuation)
text_corpus = tm_map(text_corpus, removeNumbers)
text_corpus = tm_map(text_corpus, content_transformer(stripWhitespace))
text_corpus = tm_map(text_corpus, removeWords, stopwords("en"))

text_corpus = tm_filter(text_corpus, 
                        function(x) length(unlist(strsplit(as.character(x), " "))) > 0)

dtm = TermDocumentMatrix(text_corpus)
m = as.matrix(dtm)
word_freqs = sort(rowSums(m), decreasing = TRUE)
word_freqs_df = data.frame(word = names(word_freqs), freq = word_freqs)
#head(word_freqs_df, 20)

listing_clean = listing_clean %>%
  mutate(
    located_desc = as.integer(str_detect(name, regex("located", ignore_case = TRUE))),
    restaurants_desc = as.integer(str_detect(name, regex("restaurants", ignore_case = TRUE))),
    walk_desc = as.integer(str_detect(name, regex("walk", ignore_case = TRUE))),
    new_desc = as.integer(str_detect(name, regex("new", ignore_case = TRUE)))
  )


# Calculate distance to Downtown, Central Park, Empire State Building

listing_loc = listing %>%
  select(id, latitude, longitude)

listing_loc = listing_loc %>%
  mutate(dt_lat = 40.706821, 
         dt_lon = -74.0091,
         park_lat = 40.78133976473782,
         park_lon = -73.96662084465494,
         empire_lat = 40.74842927376084,
         empire_lon = -73.985322454067
         )

listing_loc = listing_loc %>%
  mutate(
    dist_dt = pmap_dbl(list(longitude, latitude, dt_lon, dt_lat), 
                       ~ distm(matrix(c(..1, ..2), nrow = 1),
                               matrix(c(..3, ..4), nrow = 1),
                               fun = distHaversine)),
    dist_park = pmap_dbl(list(longitude, latitude, park_lon, park_lat), 
                         ~ distm(matrix(c(..1, ..2), nrow = 1),
                                 matrix(c(..3, ..4), nrow = 1),
                                 fun = distHaversine)),
    dist_empire = pmap_dbl(list(longitude, latitude, empire_lon, empire_lat), 
                          ~ distm(matrix(c(..1, ..2), nrow = 1),
                                  matrix(c(..3, ..4), nrow = 1),
                                  fun = distHaversine))
  )

listing_clean = listing_clean %>%
  left_join(listing_loc[, c("id", "dist_dt", "dist_park", "dist_empire")], by = "id")



# Remove NA in Price
listing_clean = filter(listing_clean, is.na(price)==FALSE)

write.csv(listing_clean, file = "data/listing_clean.csv", 
          row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8")
