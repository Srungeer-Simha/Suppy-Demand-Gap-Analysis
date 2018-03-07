######################################### loading libraries ##################################################

library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)

############################################ loading data ####################################################

Cab <- read.csv("Cab request data.csv", stringsAsFactors = F)

########################################### Data Cleaning ####################################################

#1 Checking for unnecessary rows

head(Cab) # No header rows
tail(Cab) # No footer or total/subtotal rows


#2 Checking for unnecessary columns

str(Cab) # All columns contain relevant information

Cab$Pickup.point <- as.factor(Cab$Pickup.point) #Pickup point has only two unique values so better to store as factor
Cab$Status <- as.factor(Cab$Status) #Status has only three unique values so better to store as factor

str(Cab) # Pickup point and status are now factors


#3 Checking for missing values

sum(is.na(Cab)) #plenty of missing values

Cab[which(is.na(Cab$Driver.id)), ] #Driver ID and drop time is NA when there are no cars available; valid NA
Cab[which(is.na(Cab$Drop.timestamp)), ] #drop time NA when trip is cancelled or there are no cars available; valid NA


#4 Fixing date and times

# both / and - have been used to represent dates. Standardise format to "-"

Cab$Request.timestamp <- str_replace_all(Cab$Request.timestamp, pattern = "/", replacement = "-")
Cab$Drop.timestamp <- str_replace_all(Cab$Drop.timestamp, pattern = "/", replacement = "-")

# Some entries have information on seconds whereas others don't. Lets standardise format
# 19 characters are requires to fully represent %d,%m,%Y %H:%M:%S

Cab$Request.timestamp[which(nchar(Cab$Request.timestamp) != 19)] <- paste(Cab$Request.timestamp[which(nchar(Cab$Request.timestamp) != 19)], ":00", sep = "")
Cab$Drop.timestamp[which(nchar(Cab$Drop.timestamp) != 19)] <- paste(Cab$Drop.timestamp[which(nchar(Cab$Drop.timestamp) != 19)], ":00", sep = "")

# Converting into date-time format
# using POSIXct instead of POSIXlt as it is compatible with dplyr

Cab$Request.timestamp <- as.POSIXct(Cab$Request.timestamp, format = "%d-%m-%Y %H:%M:%S")
Cab$Drop.timestamp <- as.POSIXct(Cab$Drop.timestamp, format = "%d-%m-%Y %H:%M:%S")


#5. Check for duplicate rows

sum(duplicated(Cab)) #Return 0 i.e there are no duplicate rows

############################################# Derived Metrics ###############################################

#1. Extracting request.dates from request.timestamp

Cab$request.dates <- format(Cab$Request.timestamp, "%d")
Cab$request.dates <- as.integer(Cab$request.dates)

# All the data is in July 2016, so extracting month, quarter and year is not necessary


#2. Extracting request.hours from request.timestamp

Cab$request.hours <- format(Cab$Request.timestamp, "%H")
Cab$request.hours <- as.integer(Cab$request.hours)


#3. Classifying request.hours into time slots

library(mosaic)

Cab <- mutate(Cab, time.slot = derivedFactor(
              "late night" = Cab$request.hours %in% c(0:3),
              "morning" = Cab$request.hours %in% c(4:7),
              "late morning" = Cab$request.hours %in% c(8:12),
              "afternoon" = Cab$request.hours %in% c(13:16),
              "evening" = Cab$request.hours %in% c(17:20),
              "night" = Cab$request.hours %in% c(21:23)
              ))


#4. Checking breadth of data

# days of the week

summary(factor(weekdays(Cab$Request.timestamp))) # Only weekday information is present

# No, of days

summary(factor(format(Cab$Request.timestamp, format = "%d"))) # 5 days worth of data 

# No. of drivers

n_distinct(Cab$Driver.id, na.rm = T) # Rides information of 300 drivers in present

######################################### Exploratory Data Analysis #########################################

#1. Distribution of request by pickup points 

# creating bar plot with pick up point on x axis and frequency on y axis. bar plot is used as categorical 
# variables are being plotted

ggplot(Cab, aes(x = Pickup.point)) + 
      geom_bar() + 
      geom_text(stat = "count", aes(label =..count..), vjust = -1) + 
      scale_y_continuous(limits = c(0,4000)) + 
      theme_light()

#Observations -  No of requests from city and airport are more or less the same


#2. Distribution of trip status by pickup points 

# creating bar plot with status on x axis and frequency on y axis. color is used to show split by pickup point
# bar plot is used as categorical  variables are being plotted

ggplot(Cab, aes(x = Status, fill = Pickup.point)) +
      geom_bar() + 
      geom_text(stat = "count", aes(label =..count..), position = position_stack(vjust = 0.5)) +
      theme_light()

#Observations:
# A. Frequency of "no cars available" is quite high, almost as many no of completed trips
# B. Most cancellations occur when travelling to the airport from the city
# C. "no cars available" occurs mostly when travelling to the city from the airport


#3. Distributing cancelled trips by time slots

# creating bar plot with time slot on x axis and frequency of cancelled trips on y axis
# color is used to show split by pickup point
# bar plot is used as categorical  variables are being plotted

Cab %>%
filter(Status == "Cancelled") %>%
ggplot(aes(x = time.slot, fill = Pickup.point)) + 
      geom_bar() + ylab("frequency of cancelled trips") +
      geom_text(stat = "count", aes(label =..count..), position = position_stack(vjust = 0.5)) +
      theme_light()

#Observations - Almost all the cancellations occur in the "morning" and "late morning" time slots while going to the airport


#4. Distributing no cars available by time slots

# creating bar plot with time slot on x axis and frequency of no cars available trips on y axis
# color is used to show split by pickup point
# bar plot is used as categorical  variables are being plotted

Cab %>%
filter(Status == "No Cars Available") %>%
ggplot(aes(x = time.slot, fill = Pickup.point)) + 
      geom_bar() + ylab("frequency of no cars available") +
      geom_text(stat = "count", aes(label =..count..), position = position_stack(vjust = 0.5)) +
      theme_light()

#Observations - Almost all the cancellations occur in the "evening" and "night" time slots while going to the city


#5. Finding time slots with the most supply demand gap

# Defining a new data frame containing supply demand information by time slot and pick up point

supply_demand <- Cab %>%
                 group_by(time.slot, Pickup.point) %>%
                 summarise(demand = n(), supply = length(Request.id[which(Status == "Trip Completed")])) %>%
                 mutate(gap = demand - supply)


# creating bar plot with time slot on x axis and gap y axis. color is used to show split by pickup point
# bar plot is used as categorical  variables are being plotted
# reorder used to arrange plot in decreasing order of gap

ggplot(supply_demand, aes(x = reorder(time.slot, -gap), y = gap, fill = Pickup.point)) +
      geom_col() + 
      xlab("Time Slot") + ylab("Suppy-demand gap") +
      geom_text(aes(label = gap), position = position_stack(vjust = 0.5)) +
      theme_light()

#Observations -  
# A. For trips from Airport to city, supply demand gap is highest in evenings and nights
# B. For trips from city to airport, supply demand gas is highest in mornings and late mornings
# C. These observations concur with earlier observations from analysis of "cancelled" and "no cars available" trips
# D. Overall gap is highest in the evenings

############################################### Hypothesis ##################################################

## Understanding why there are lots of cancellations in the morning and late morning time slots when 
## travelling to the airport 

# 1. Number of drivers available in this time slot is less

# creating bar plot with time slot on x axis and no. of cars available in the city on y axis
# bar plot is used as categorical  variables are being plotted

Cab %>%
filter(Status == "Trip Completed" & Pickup.point == "City") %>%
group_by(time.slot) %>%
summarise(driver.count = n_distinct(Driver.id, na.rm = TRUE)) %>%
ggplot(aes(x = time.slot, y = driver.count)) + 
      geom_col() + ylab(" No. of drivers available in the city") +
      geom_text(aes(label = driver.count), vjust = -1) +
      scale_y_continuous(limits = c(0,250)) +
      theme_light()

#Observations - Driver availability is actually the highest in the morning and late morning slots


# 2. Drivers are cancelling trips from city in mornings and late mornings as they can't return trips

# estimating travel times during mornings and late mornings

Cab %>%
filter(Status == "Trip Completed" & Pickup.point == "City") %>%
filter(time.slot == "morning" || time.slot == "late morning") %>%
summarise(travel_time = median(Drop.timestamp - Request.timestamp))

#Obervation - median travel time from city to airport is 53 minutes so time slot doesn't change

# Demand at city and airport in morning and late morning time slot

# creating bar plot with time slot (morning or late morning) on x axis and demand on y axis
# color is used to show split by pick up point
# bar plot is used as categorical  variables are being plotted

supply_demand %>% 
filter(time.slot == "morning" || time.slot == "late morning") %>%
ggplot(aes(x = time.slot, y = demand, fill = Pickup.point)) +
      geom_col(position = "dodge") +
      geom_text(aes(label = demand), position = position_dodge(width = 0.9), vjust = -0.5) +
      theme_light()

#Observation - 
# A. demand at the airport is far lesser than demand at the city in these time slots
# B. This could explain why drivers are cancelling trips, as they will not be able to get return trips back


## Understanding why there are no cars available at the airport during evening and night

# 1. There are less number of drivers at the airport in these time slots

Cab %>%
filter(time.slot == "evening" | time.slot == "night") %>%
group_by(Pickup.point) %>%
summarise(driver.count = n_distinct(Driver.id, na.rm = TRUE))

#Observation - No. of drivers available at the city and airport during these time slots is the same


# 2. There are a lot of flights landing in the evening and nights

# creating bar plot with time slot on x axis and supply demand on y axis
# color is used to represent demand/supply
# bar plot is used as categorical  variables are being plotted
# Reshaping supply_demand data frame using gather command to enable plotting

supply_demand[ ,-5] %>%           
gather(key = "Supply.demand", value = "value", c("supply", "demand")) %>%
filter(Pickup.point == "Airport") %>%
ggplot(aes(x = time.slot, y = value, fill = Supply.demand)) +
       geom_col(position = "dodge") + ylab("supply/demand at the aiport") +
       geom_text(aes(label = value), position = position_dodge(width = 0.9), vjust = -0.5) +
       theme_light()

#Observation - 
# 1. Demand is far higher than the supply during evening and night at the airport whereas supply is more or less the same throught the day
# 2. This means number of cars at the airport is not the problem but rather a large number of people arriving at the airport in these time slots is.


########################################## Possible Solutions ##############################################

##1. Fixing issue of cancellations during morning and late mornings time slot

#A. Make a dedicated airport to city fleet which charges higher than regular rates, this will cover the downtime
#A. drivers face due to lack of return trips at the airport during these time slots

#B. Same solution can be implemented by rate multiplier when people travel from city to airport during these time slots

##2. Fixing issue of no cars being available at the airport during evening and night

#A. If a dedicated airport to city fleet is implemented, Cab can artificially increase the no of cars 
#A. present at the airport during the high demand night and evening time slots

#B. Increase cab search radius and add a rate multiplier so that cabs in vicinity of the airport are encouraged
#B. to travel to the airport for pickups

