#Feature Engineering:
airbnb = subset(airbnb, select = -c(neighbourhood))

#Replace 'about' variables with length of characters
airbnb$description = nchar(airbnb$description)
airbnb$neighborhood_overview = nchar(airbnb$neighborhood_overview)
airbnb$neighborhood_overview[airbnb$neighborhood_overview ==1] = 0

#change host_location from city to local Y/N
airbnb$host_location[grep("Toronto",airbnb$host_location)] = "Local"
airbnb$host_location[!airbnb$host_location=="Local"] = "Not local"


#New feature for number of host_verifications counts number of verficiation methods
a = strsplit(airbnb$host_verifications, ",")
b = c()
for (i in 1:length(a)){
  b[i] = length(a[[i]])
}
airbnb$host_verifications = b

#Dealing with property_type: group logically for listings>100
airbnb$property_type[airbnb$property_type == "Entire home/apt"] = "Entire apartment"

airbnb$property_type[airbnb$property_type == "Entire villa"]= "Entire house"
airbnb$property_type[airbnb$property_type == "Entire place"]= "Entire house"
airbnb$property_type[airbnb$property_type == "Tiny house"]= "Entire house"
airbnb$property_type[airbnb$property_type == "Entire cottage"]= "Entire house"

airbnb$property_type[airbnb$property_type == "Private room in serviced apartment"]= "Private room in apartment"
airbnb$property_type[airbnb$property_type == "Private room in loft"]= "Private room in apartment"

airbnb$property_type[airbnb$property_type == "Entire guesthouse"]= "Entire guest suite"

#Add new feature that shows whether bathroom is private vs. shared
airbnb$bathroom_pri_shared = as.factor(ifelse(grepl("shared",airbnb$bathrooms_text), "shared","private"))

#Then change bathroom_text column to contain only number of bathrooms available
#create function
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

airbnb$bathrooms_text = as.numeric(numextract(airbnb$bathrooms_text))
airbnb$bathrooms_text[is.na(airbnb$bathrooms_text)] <- 0.5

#New feature for number of amenities
d = strsplit(airbnb$amenities, ",")
f = c()
for (i in 1:length(d)){
  f[i] = length(d[[i]])
}  
airbnb$num_amenities = f

#One-hot encoding categorical variables
dmy = dummyVars("~host_location+host_is_superhost+host_has_profile_pic+host_identity_verified+neighbourhood_cleansed+property_type+room_type+instant_bookable+bathroom_pri_shared", data = airbnb, drop2nd = T)
hot_encoding = data.frame(predict(dmy, newdata = airbnb))
hot_encoding = subset(hot_encoding, select = -c(host_locationNot.local,host_is_superhostf,host_has_profile_picf,host_identity_verifiedf,instant_bookablef,bathroom_pri_shared.shared))
airbnb_hot_encoding = cbind(airbnb,hot_encoding)

#Remove any column not used in modeling
airbnb_final = subset(airbnb_hot_encoding, select = -c(amenities,host_location,host_is_superhost,host_has_profile_pic,host_identity_verified,neighbourhood_cleansed,property_type,room_type,instant_bookable,bathroom_pri_shared))

#Load amenities dataset from python
amenities_table = read.csv("amenities_python.csv", na.strings = c("", "NA"))
amenities_table = subset(amenities_table, select =-c(1,2))
#change table from T/F to 0/1
amenities_table[amenities_table == "False"] <- 0
amenities_table[amenities_table == "True"] <- 1
#change whole dataframe to numeric
amenities_table[] <- lapply(amenities_table, as.numeric)
amenities_table = amenities_table[amenities_table$price <=500,]

#Top 30 Amenities
amenity_summary = summary(lm(price~., data=amenities_table))

mod_summary_sign <- amenity_summary$coefficients[ , 4]  # Pull out p-values
mod_summary_stars <- NA               # Named vector with significance stars
mod_summary_stars[mod_summary_sign < 0.1] <- "."
mod_summary_stars[mod_summary_sign < 0.05] <- "*"
mod_summary_stars[mod_summary_sign < 0.01] <- "**"
mod_summary_stars[mod_summary_sign < 0.001] <- "***"
mod_summary_stars[is.na(mod_summary_stars)] <- "n.s."
names(mod_summary_stars) <- names(mod_summary_sign)

mod_summary_stars[mod_summary_stars == "***"]

amenity_top = subset(amenities_table, select = c("Crib","Private.living.room","Essentials","Essentials.1","Wifi.1","Lock.on.bedroom.door.1","Baby.safety.gates","First.aid.kit","Sound.system","Smoke.alarm.1","Washer.1","Paid.parking.on.premises","Hot.water.1","LG.refrigerator","Hangers.1","Dishes.and.silverware.1"))

##Combine top amenities with dataset
airbnb_final = cbind(airbnb_final,amenity_top)



