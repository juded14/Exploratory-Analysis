#Doing some exploratory analysis on the superstore dataset to see what kind of questions can we answer.
#We'll load the superstore dataset.
superstore <- read.csv("C:/Git/Tools for Data Analysis/superstore.csv")

#Load the packages we are going to need to answer some questions and create additional feature.
library("tidyverse")
library("lubridate")

#I usually like to see which variables I'm working with.
names(superstore)

#I want to make sure none are the variables aren't in a weird format
class(superstore$State)
class(superstore$Region)
class(superstore$Postal.Code)
class(superstore$City)

#I prefer the Postal code to be listed as a factor.
superstore$Postal.Code <- as.factor(superstore$Postal.Code)

#Which region has the most orders?
region_orders <- superstore %>% 
  select(Region) %>% 
  group_by(Region) %>% 
  summarise(region_orders = n()) %>% 
  arrange(desc(region_orders))

#Working with the lubridate package, converting order & shipment to dates makes it easier to work with.
class(superstore$Order.Date)
class(superstore$Ship.Date)
superstore$Order.Date <- mdy(superstore$Order.Date)
superstore$Ship.Date <- mdy(superstore$Ship.Date)

#I'm interested to see how the travel times for each order.
superstore <- superstore %>% 
  mutate(travel_time = Ship.Date - Order.Date)

# I wanted to reorder the "travel_time" to be closer to Order.date & Ship.Date via left_join, but I'm having difficulty at this time.
# I'm going to save the re-order for a later time while I do research.

# I want to see the average travel time per region.
superstore %>% 
  select(Region, travel_time) %>% 
  group_by(Region) %>% 
  summarise(avg_travel = mean(travel_time)) %>% 
  arrange(desc(avg_travel))

# It looks like Central region has the highest travel time.
# Maybe later we can check to see if it's because those Central orders are furthest from the shipping plant.
superstore %>% 
  filter(Region == "Central", travel_time) %>% 
  group_by(State) %>% 
  summarise(cent.state_tot_orders = n(),
            cent.avg_travel = mean(travel_time, na.rm = TRUE)) %>% 
    arrange(desc(cent.avg_travel))

superstore %>% 
  filter(Region == "Central") %>% 
  group_by(State) %>% 
  summarise(central_travel_time = mean(travel_time)) %>% 
  arrange(desc(central_travel_time))

#It's weird that average travel time is different when I add order totals for each state, while trying
#to figure out the average travel time for each state. The only difference between the two pieces of code
#is that I added an additional summary to the tibble.
#I tried using na.rm = TRUE. I also changed the travel_time variable to as.numeric & as.integer as well as some other changes. Nothing works.
#I don't know why the average travel time for the central region changes. Looking to do more research.
superstore$travel_time <- as.numeric(superstore$travel_time)

?as.integer()

?as.double.difftime()
