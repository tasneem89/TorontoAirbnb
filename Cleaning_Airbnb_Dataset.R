#Remove all variables stored previously 
rm(list = ls())
#Loading libraries
library(dplyr)
library(plyr)
library(naniar)
library(caret)
library(ggplot2)
library(corrplot)
library(modeest)
library(viridis)
library(hrbrthemes)
library(GGally)
library(stringr)
library(relaimpo)
library(glmnet)
library(e1071)
library(randomForest)

#setting working directory
setwd("C:/Users/a.elnazir/Desktop/Tasneem Alhilwa/Ryerson/Project/Airbnb Dataset")

#Loading the airbnb dataset
airbnb = read.csv("listings.csv", na.strings = c("", "NA"))

#Data Cleaning
#Remove irrelevant columns
airbnb = subset(airbnb, select = -c(listing_url,scrape_id,last_scraped,name,picture_url,host_id,host_url,host_name,host_response_time,host_acceptance_rate,host_response_rate,host_thumbnail_url,host_picture_url,first_review,host_neighbourhood,host_total_listings_count,last_review,neighbourhood_group_cleansed,host_about,calendar_updated,has_availability,availability_30,availability_60,availability_90,availability_365,bathrooms,calendar_last_scraped,calculated_host_listings_count,host_since,calculated_host_listings_count_entire_homes,calculated_host_listings_count_private_rooms,calculated_host_listings_count_shared_rooms,license,minimum_nights,maximum_nights,maximum_minimum_nights,minimum_maximum_nights,maximum_maximum_nights,minimum_nights_avg_ntm,maximum_nights_avg_ntm,number_of_reviews_ltm,number_of_reviews_l30d))

#Check for missing values
sum(is.na(airbnb))
vis_miss(airbnb, sort_miss = T)

#Fill/remove missing values
airbnb$neighborhood_overview[is.na(airbnb$neighborhood_overview)] =0

airbnb$description[is.na(airbnb$description)] <-0

airbnb$bedrooms[is.na(airbnb$bedrooms) & (airbnb$room_type =="Private room")] <-1
airbnb$bedrooms[is.na(airbnb$bedrooms) & (airbnb$room_type =="Hotel room")] <-1
airbnb$bedrooms[is.na(airbnb$bedrooms) & (between(airbnb$accommodates,1,3))] <-1
airbnb$bedrooms[is.na(airbnb$bedrooms) & (between(airbnb$accommodates,4,6))] <-2
airbnb$bedrooms[is.na(airbnb$bedrooms) & (between(airbnb$accommodates,7,8))] <-3
airbnb$bedrooms[is.na(airbnb$bedrooms) & (between(airbnb$accommodates,9,12))] <-4
airbnb$bedrooms[is.na(airbnb$bedrooms) & (between(airbnb$accommodates,13,16))] <-5

airbnb$beds[is.na(airbnb$beds) & (airbnb$bedrooms ==1)] <-round(mean(airbnb$beds[airbnb$bedrooms==1], na.rm=T))
airbnb$beds[is.na(airbnb$beds) & (airbnb$bedrooms ==2)] <-round(mean(airbnb$beds[airbnb$bedrooms==2], na.rm=T))
airbnb$beds[is.na(airbnb$beds) & (airbnb$bedrooms ==3)] <-round(mean(airbnb$beds[airbnb$bedrooms==3], na.rm=T))
airbnb$beds[is.na(airbnb$beds) & (airbnb$bedrooms ==7)] <-round(mean(airbnb$beds[airbnb$bedrooms==7], na.rm=T))

airbnb$reviews_per_month[is.na(airbnb$reviews_per_month)] <-0

airbnb$review_scores_rating[is.na(airbnb$review_scores_rating)] <-0
airbnb$review_scores_accuracy[is.na(airbnb$review_scores_accuracy)] <-0
airbnb$review_scores_cleanliness[is.na(airbnb$review_scores_cleanliness)] <- 0
airbnb$review_scores_checkin[is.na(airbnb$review_scores_checkin)] <- 0
airbnb$review_scores_communication[is.na(airbnb$review_scores_communication)] <-0
airbnb$review_scores_location[is.na(airbnb$review_scores_location)] <- 0
airbnb$review_scores_value[is.na(airbnb$review_scores_value)] <-0

airbnb$host_has_profile_pic[is.na(airbnb$host_has_profile_pic)] = mfv(airbnb$host_has_profile_pic, na_rm = T)

airbnb$host_identity_verified[is.na(airbnb$host_identity_verified)] = mfv(airbnb$host_identity_verified, na_rm = T)

airbnb$host_is_superhost[is.na(airbnb$host_is_superhost)] = mfv(airbnb$host_is_superhost, na_rm = T)

airbnb$host_location[is.na(airbnb$host_location)] = mfv(airbnb$host_location, na_rm = T)

airbnb$host_listings_count[is.na(airbnb$host_listings_count)] <- median(airbnb$host_listings_count,na.rm=T)

airbnb$bathrooms_text[is.na(airbnb$bathrooms_text)] = mfv(airbnb$bathrooms_text, na_rm = T)

#Fix data types as necessary
airbnb$price = as.numeric(gsub(",", "", substring(airbnb$price, 2)))

#Check for outliers in price (drop price =0  and >$1000)
airbnb = airbnb[!airbnb$price ==0 & airbnb$price<=500 ,]

