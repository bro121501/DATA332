library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(here)
library(skimr)
library(kableExtra)
library(fastmap)
library(knitr)
library(maps)
library(ggmap)
library(sp)
library(jsonlite)

rm(list = ls()) #Clears variables

setwd('H:\\Desktop Folder\\R Shit\\hosptial') #Sets working directory
#Must change \ to \\ otherwise get an error

#Visit.txt and visit.xlsx include the same information. 
billing <- read_excel("data/Billing.xlsx")
patient <- read_excel("data/Patient.xlsx")
visit <- read_excel("data/Visit.xlsx")

joined_data <- left_join(visit, patient, by = "PatientID")
joined_data <- left_join(joined_data, billing, by = "VisitID")

joined_data <- separate(joined_data, VisitDate, into = c("year", "month", "day"), sep = "-")

joined_data_combines <- joined_data %>%
  mutate(Reason = str_replace(Reason, "Allergic reaction follow-up", "Follow-up"))

joined_data_combines <- joined_data_combines %>%
  mutate(Reason = str_replace(Reason, "Bronchitis follow-up", "Follow-up"))

joined_data_combines <- joined_data_combines %>%
  mutate(Reason = str_replace(Reason, "Cyst removal follow-up", "Follow-up"))

joined_data_combines <- joined_data_combines %>%
  mutate(Reason = str_replace(Reason, "Dermatitis follow-up", "Follow-up"))

joined_data_combines <- joined_data_combines %>%
  mutate(Reason = str_replace(Reason, "Hypothyroidism follow-up", "Follow-up"))

joined_data_combines <- joined_data_combines %>%
  mutate(Reason = str_replace(Reason, "Laceration follow-up", "Follow-up"))

joined_data_combines <- joined_data_combines %>%
  mutate(Reason = str_replace(Reason, "Migraine follow-up", "Follow-up"))

joined_data_combines <- joined_data_combines %>%
  mutate(Reason = str_replace(Reason, "Spotted fever rickettsiosis follow-up", "Follow-up"))

joined_data_combines <- joined_data_combines %>%
  mutate(Reason = str_replace(Reason, "UTI follow-up", "Follow-up"))

joined_data_combines <- joined_data_combines %>%
  mutate(Reason = str_replace(Reason, "Laceration of left hand", "Laceration"))

joined_data_combines <- joined_data_combines %>%
  mutate(Reason = str_replace(Reason, "Laceration of right calf", "Laceration"))

joined_data_combines <- joined_data_combines %>%
  mutate(Reason = str_replace(Reason, "Laceration of right foot", "Laceration"))

joined_data_combines <- joined_data_combines %>%
  mutate(Reason = str_replace(Reason, "Hypertension monitoring", "Hypertension"))

joined_data_combines <- joined_data_combines %>%
  mutate(Reason = str_replace(Reason, "Hypotension monitoring", "Hypotension"))

joined_data_combines <- joined_data_combines %>%
  mutate(Reason = str_replace(Reason, "Rhinitis follow-up", "Follow-up"))

joined_data_combines <- joined_data_combines %>%
  mutate(Reason = str_replace(Reason, "Annual wellness visit - dermatitis", "Annual wellness visit"))

joined_data_combines <- joined_data_combines %>%
  mutate(Reason = str_replace(Reason, "Fracture of left fifth metacarpal", "Fracture"))

joined_data_combines <- joined_data_combines %>%
  mutate(Reason = str_replace(Reason, "Fracture of right tibia", "Fracture"))

visit_by_month <- joined_data_combines %>%
  group_by(Reason, month) %>%
  summarize(count = n())

ggplot(visit_by_month, aes(x = count, y = Reason, fill = month)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Visits by Reason and Month", x = "Count", y = "Reason of Visit", fill = "Month") +
  scale_fill_brewer(palette = "Set1", name = "Month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

walk_in <- joined_data_combines %>%
  group_by(Reason, WalkIn) %>%
  summarize(count = n())

ggplot(walk_in, aes(x = count, y = Reason, fill = WalkIn)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Visits by Reason and Walk in", x = "Count", y = "Reason of Visit", fill = "Walk In") +
  scale_fill_brewer(palette = "Set1", name = "Walk In") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#!!!ALL TAKEN FROM CHATGPT BECAUSE THERE ARE TOO MANY ZIPCODES TO PROPERLY GRAPH!!!
#RUN THIS AND IT TAKES A COUPLE SECONDS. THEN THE REST OF THE CODE SHOULD WORK
#TOOK 33 SECONDS ON A RYZEN 9 7950X, 32 GB 6000 RAM, 3080TI OC! YOUR TIME MAY VARY
#BASED ON INTERNET SPEED, BACKGROUND TASK, ECT
{
start_time <- Sys.time()
api_key <- "AIzaSyCt3fuklEaPfMAogxfbm2mZFpuqVh_N2p0"
register_google(key = api_key)

# Assuming 'joined_data_combines' is your data frame containing zip codes

zip_codes <- joined_data_combines$Zip  # Sample 200 zip codes

# Function to get latitude and longitude for a given zip code
get_lat_lon <- function(zip_code) {
  url <- sprintf("https://maps.googleapis.com/maps/api/geocode/json?address=%s&key=%s", zip_code, api_key)
  response <- jsonlite::fromJSON(url)
  if (response$status == "OK" && length(response$results$geometry$location$lat) > 0 && length(response$results$geometry$location$lng) > 0) {
    return(c(response$results$geometry$location$lat, response$results$geometry$location$lng))
  } else {
    return(NA)
  }
}

# Fetch latitude and longitude for each zip code
lat_lon_data <- lapply(zip_codes, function(zip) {
  return(get_lat_lon(zip))
})

# Create a data frame from the latitude and longitude data
zip_df <- data.frame(do.call(rbind, lat_lon_data))
colnames(zip_df) <- c("latitude", "longitude")
zip_df$Zip <- joined_data_combines$Zip

atlanta_lat <- 33.7490
atlanta_lon <- -84.3880

# Function to determine the direction based on differences in latitude and longitude
get_direction <- function(lat_diff, lon_diff) {
  if (lat_diff > 0) {
    if (lon_diff > 0) {
      return("Northeast")
    } else if (lon_diff < 0) {
      return("Northwest")
    } else {
      return("North")
    }
  } else if (lat_diff < 0) {
    if (lon_diff > 0) {
      return("Southeast")
    } else if (lon_diff < 0) {
      return("Southwest")
    } else {
      return("South")
    }
  } else {
    if (lon_diff > 0) {
      return("East")
    } else if (lon_diff < 0) {
      return("West")
    } else {
      return("At Atlanta")
    }
  }
}

# Calculate the direction for each zip code relative to Atlanta
joined_data_combines$direction <- mapply(function(lat, lon) {
  lat_diff <- lat - atlanta_lat
  lon_diff <- lon - atlanta_lon
  return(get_direction(lat_diff, lon_diff))
}, zip_df$latitude, zip_df$longitude)
end_time <- Sys.time()
}

elapsed_time <- end_time - start_time
print(elapsed_time)

visit_by_city <- joined_data_combines %>%
  group_by(Reason, direction) %>%
  summarize(count = n())

ggplot(visit_by_city, aes(x = count, y = Reason, fill = direction)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Visits by Reason Location Relative to Atlanta", x = "Count", y = "Reason of Visit", fill = "Location") +
  scale_fill_brewer(palette = "Set1", name = "Location") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

invoice <- joined_data_combines %>%
  group_by(Reason, InvoicePaid) %>%
  summarize(sum_col = sum(InvoiceAmt))

ggplot(invoice, aes(x = sum_col, y = Reason, fill = InvoicePaid)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Invoice Amount Based on Reason and Paid", x = "Count", y = "Reason of Visit", fill = "Paid") +
  scale_fill_brewer(palette = "Set1", name = "Paid") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

south <- joined_data_combines %>%
  filter(direction == "Southeast") %>%
  filter(direction == "Southeast") %>%
  group_by(Reason, InvoicePaid) %>%
  summarize(count = n())

ggplot(south, aes(x = count, y = Reason, fill = InvoicePaid)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Invoice Amount Based on Reason and Paid", x = "Count", y = "Reason of Visit", fill = "Paid") +
  scale_fill_brewer(palette = "Set1", name = "Paid") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
