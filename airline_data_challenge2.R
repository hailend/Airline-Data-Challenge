#airline_data_challenge.R
#include the necessary libraries
library(tidyverse)
library(ggplot2)
library(readxl)
library(data.table)
library(plotly)
#------------------ DATA-----
#---------------------------load data -----------------------------------------
#function to load data into R
read_dat <- function(file_name){
  read_csv(file_name)
}
#call function to import data
#airport_codes <- read_dat("Airport_Codes.csv")
airport_codes <- fread("Airport_Codes.csv")
flights <- fread("Flights.csv")
tickets <- fread("tickets.csv")

#------------------------- Data Quality checks --------------------------------
#-------- preview data and feature data types
as_tibble(airport_codes)
as_tibble(flights)
as_tibble(tickets)
glimpse(airport_codes) #airport_codes datasets
glimpse(flights) #flights dataset
#The distance airtime features is given as a character vector. These need to be converted into numeric
flights$AIR_TIME <- as.numeric(flights$AIR_TIME)
flights$DISTANCE <- as.numeric(flights$DISTANCE)
flights$OCCUPANCY_RATE <- as.numeric(flights$OCCUPANCY_RATE)
#Check for missing values (NAs) and invalid values on the flights dataset
sum(is.na(flights$ORIGIN_AIRPORT_ID))
sum(is.na(flights$DEST_AIRPORT_ID))
sum(is.na(flights$ORIGIN))
sum(is.na(flights$DESTINATION))
sum(is.na(flights$DEP_DELAY)) #NAs should be eliminated from consideration
sum(is.na(flights$ARR_DELAY)) #NAs should be eliminated from consideration
sum(is.na(flights$CANCELLED))
sum(is.na(flights$DISTANCE)) 
sum(is.na(flights$OCCUPANCY_RATE) | flights$OCCUPANCY_RATE <= 0)

#find rows where NAs are available and check to see if the data is available 
#in other rows, but the same routes 

#Flights dataset - DISTANCE 
ind <- which(is.na(flights$DISTANCE) | flights$DISTANCE <= 0) #indices where distance data is missing or invalid
inv_dist <- data.frame(ORIGIN = flights$ORIGIN[ind], DESTINATION = flights$DESTINATION[ind]) %>%
  unique() #extract routes where distance data is missing or invalid
#Extract valid distance from other rows of the same routes
v_dist <- flights[-ind, ] %>% 
  filter(inv_dist[, 1] == ORIGIN & inv_dist[, 2] == DESTINATION) %>%
  select(ORIGIN, DESTINATION, DISTANCE) %>% unique() %>% 
  arrange(ORIGIN, DESTINATION)
#It can be seen that distance info for all missing or invalid data is already available from other rows
new_dist <- left_join(inv_dist, v_dist, by = c("ORIGIN", "DESTINATION")) 
new_dist
#It can be seen that RSW to MDW has two different distance values. Therefore, 
#the second (incorrect) should be eliminated
new_dist <- new_dist[-2, ] #remove the wrong distance
#Substitute the distance values into the missing values into the flights dataset
new_df <- data.frame(ORIGIN = flights$ORIGIN[ind], DESTINATION = flights$DESTINATION[ind]) #collect missing values
dist <- left_join(new_df, new_dist, by = c("ORIGIN", "DESTINATION")) #join distance data to routes
flights$DISTANCE[ind] <- dist$DISTANCE #substitute distance data back to the flights database
sum(is.na(flights$DISTANCE) | flights$DISTANCE <= 0) #check for invalid or missing distance                              
#The DISTANCE vector represents the one-way distance between origin and destination
#airports. For roundtrip analysis, distance values must be doubled.
flights$DISTANCE <- 2 * flights$DISTANCE 
#----------------

#FUNCTION 2 - fix missing values

#-----------

#Flights dataset - OCCUPANCY RATE
#similarly substitute missing values in the OCCUPANCY_RATE vector
ind <- which(is.na(flights$OCCUPANCY_RATE) | flights$OCCUPANCY_RATE <= 0) #indices where distance data is missing or invalid
inv_o_rate <- data.frame(ORIGIN = flights$ORIGIN[ind], 
                         DESTINATION = flights$DESTINATION[ind], 
                         OP_CARRIER = flights$OP_CARRIER[ind]) %>%
  unique()             #extract routes and carriers where fare data is missing or invalid
#Extract valid fares from other tickets of the same routes and carriers
v_o_rate <- flights[-ind, ] %>% 
  filter(inv_o_rate[, 1] == ORIGIN & inv_o_rate[, 2] == DESTINATION & inv_o_rate[, 3] == OP_CARRIER) %>%
  select(ORIGIN, DESTINATION, OP_CARRIER, OCCUPANCY_RATE) %>% unique() %>% 
  arrange(ORIGIN, DESTINATION, OP_CARRIER)
nrow(v_o_rate)
#It can be seen that valid occupancy-rate data for all missing or invalid data is already available from other rows
new_o_rate <- left_join(inv_o_rate, v_o_rate, by = c("ORIGIN", "DESTINATION", "OP_CARRIER")) %>%
  group_by(ORIGIN, DESTINATION, OP_CARRIER) %>%
  summarise(ave_OCCUPANCY_RATE = mean(OCCUPANCY_RATE))
as_tibble(new_o_rate)
#Substitute the distance values into the missing values into the flights dataset
new_df <- data.frame(ORIGIN = flights$ORIGIN[ind], DESTINATION = flights$DESTINATION[ind],
                     OP_CARRIER = flights$OP_CARRIER[ind]) #collect missing values
o_rate <- left_join(new_df, new_o_rate, by = c("ORIGIN", "DESTINATION", "OP_CARRIER")) #join distance data to routes
flights$OCCUPANCY_RATE[ind] <- o_rate$OP_CARRIER #substitute occ-rate data back to the flights database
sum(is.na(flights$OCCUPANCY_RATE) | flights$OCCUPANCY_RATE <= 0) #check for invalid or missing distance                              

#--------Tickets dataset
glimpse(tickets)
#ITIN_Fare is listed as character and needs to be converted into numeric for analysis
tickets$ITIN_FARE <- as.numeric(tickets$ITIN_FARE)
#check for missing or invalid values on important features that are used for analysis
sum(is.na(tickets$ORIGIN))
sum(is.na(tickets$DESTINATION))
sum(is.na(tickets$ROUNDTRIP))
sum(is.na(tickets$ITIN_FARE) | tickets$ITIN_FARE <= 0)
#Find rows where NAs are available and check to see if the data is available 
#in other rows, but the same routes and carriers 
ind <- which(is.na(tickets$ITIN_FARE) | tickets$ITIN_FARE <= 0) #indices where distance data is missing or invalid
inv_fare <- data.frame(ORIGIN = tickets$ORIGIN[ind], 
                       DESTINATION = tickets$DESTINATION[ind], 
                       REPORTING_CARRIER = tickets$REPORTING_CARRIER[ind]) %>%
  unique()             #extract routes and carriers where fare data is missing or invalid
#Extract valid fares from other tickets of the same routes and carriers
v_fare <- tickets[-ind, ] %>% 
  filter(inv_fare[, 1] == ORIGIN & inv_fare[, 2] == DESTINATION & inv_fare[, 3] == REPORTING_CARRIER) %>%
  select(ORIGIN, DESTINATION, REPORTING_CARRIER, ITIN_FARE) %>% unique() %>% 
  arrange(ORIGIN, DESTINATION, REPORTING_CARRIER)
nrow(v_fare)
#Only a very small percentage of of the missing values are found this way. 
#Assumption: Use only route information and average fare information to these routes
#to fill the missing fare data, instead of removing the whole rows
ave_fares <- tickets %>% group_by(ORIGIN, DESTINATION) %>%
  summarise(fare = mean(ITIN_FARE, na.rm = TRUE)) 
as_tibble(ave_fares)
new_fare <- left_join(inv_fare, ave_fares, by = c("ORIGIN", "DESTINATION")) 
as_tibble(new_fare)
#Check for any missing or invalid values
new_fare %>% filter(is.na(fare)) %>% nrow()
#The number of missing values are now greatly reduced.
#Substitute the fare values in place of the missing data
new_df <- data.frame(ORIGIN = tickets$ORIGIN[ind], 
                     DESTINATION = tickets$DESTINATION[ind],
                     REPORTING_CARRIER = tickets$REPORTING_CARRIER[ind]) #collect missing rows
fare <- left_join(new_df, new_fare, by = c("ORIGIN", "DESTINATION", "REPORTING_CARRIER")) #join distance data to routes
tickets$ITIN_FARE[ind] <- fare$fare #substitute distance data back to the tickets database
#check for invalid or missing fare
sum(is.na(tickets$ITIN_FARE) | tickets$ITIN_FARE <= 0)                              
#A few hundred routes don't have any valid fare data reported and must be removed
#from consideration since cost analysis for these routes can not be performed.
tickets <- tickets %>% filter(!is.na(ITIN_FARE) & !(ITIN_FARE <= 0))
sum(is.na(tickets$ITIN_FARE) | tickets$ITIN_FARE <= 0)  #check missing values  
nrow(tickets) #No missing or invalid fare values
#Checking the range of fares reveals that some fare values may not be accurate
#(are too high or too low)
range(na.omit(as.numeric(tickets$ITIN_FARE)))
#Removing fares The distribution of fares can be shown
tickets %>% filter(ITIN_FARE <= 5000) %>% #use fares < $30,000
  ggplot(aes(ITIN_FARE)) + geom_histogram(binwidth = 50, color = "black")
#The very high prices are very small in number and chances are they are incorrect
#values. Therefore, fares above $5000 are ignored in this analysis. Prices less 
#than $100 are also assumed to be incorrect and, therefore, are ignored. 
tickets <- tickets %>% filter(ITIN_FARE <= 5000 | ITIN_FARE > 100)

#------------------------------------------
#The datasets were too big for the computer used to do the analysis. 
#Repeated attempts to join the datasets resulted in an "out of memory" error. 
#Therefore, samples of flights and tickets datasets were taken for analysis instead.
#The sample sizes are equal to 1/3 of the flights and tickets datasets, respectively
#The code provided here works similarly if used in a machine with the required memory, 

#n_flights <- 1:nrow(flights)
#n_tickets <- 1:nrow(tickets)
#set.seed(1, sample.kind = "Rounding")
#ind_flights <- sample(n_flights, size = 0.25*nrow(flights), replace = FALSE)
#set.seed(1, sample.kind = "Rounding")
#ind_tickets <- sample(n_tickets, size = 0.25*nrow(tickets),  replace = FALSE)
#flights <- flights[ind_flights, ]
#tickets <- tickets[ind_tickets, ]

#---------------- Join Datasets----------------------
#Looking into the features and the metadata of each dataframe, we can see that
#they are suited to be joined by airport available in all the three dataframes.
#---------------------------------

#FUNCTION 3 - join datasets

#---------------------------------
#select only important features
levels(as_factor(airport_codes$TYPE)) #levels of airport types
airport_codes <- airport_codes %>%
  filter(TYPE %in% c("medium_airport", "large_airport")) %>% #select only medium and large airports
  mutate(origin_IATA_code = IATA_CODE) %>%  #new features
  select(TYPE, origin_IATA_code)
levels(as_factor(airport_codes$TYPE)) #levels of airport types
#select flights that are not canceled and rename a few columns in the df for joining
flights <- flights %>%
  select(-c(FL_DATE, TAIL_NUM, OP_CARRIER_FL_NUM, AIR_TIME)) %>%
  filter(CANCELLED == 0) %>%
  rename(origin_IATA_code = ORIGIN, dest_IATA_code = DESTINATION) 
#select tickets that are round-trip and rename a few columns in the df for joining  
tickets <- tickets %>%
  select(-c(YEAR, QUARTER, PASSENGERS)) %>%
  filter(ROUNDTRIP == 1) %>%
  rename(origin_IATA_code = ORIGIN, dest_IATA_code = DESTINATION)

#join airport_codes and flights data frames by origin and destination airport codes 
flights <- left_join(flights, airport_codes, by = "origin_IATA_code") #
#join airport_codes and tickets data frames by origin and destination airport codes 
tickets <- left_join(tickets, airport_codes, by = "origin_IATA_code") #type col---not right 
airport_codes <- airport_codes %>%
  rename(dest_IATA_code = origin_IATA_code)
#join airport_codes and flights data frames by destination airport codes 
flights <- left_join(flights, airport_codes, by = "dest_IATA_code") %>%
  rename(origin_airport_type = TYPE.x, des_airport_type = TYPE.y) 
#join airport_codes and tickets data frames by destination airport codes 
tickets <- left_join(tickets, airport_codes, by = "dest_IATA_code") %>% #type col---not right 
  rename(origin_airport_type = TYPE.x, des_airport_type = TYPE.y) 

#---------------------------------
#Join airport codes and flights databases
#extract fares for each route. Fares per carrier were averaged
t1 <- tickets %>% rename(OP_CARRIER = REPORTING_CARRIER) %>%
  group_by(origin_IATA_code, dest_IATA_code, OP_CARRIER) %>%  
  summarise(fare = mean(ITIN_FARE)) %>%
  arrange(desc(fare))
  #select(ITIN_FARE)
flights2 <- inner_join(flights, t1, by = c("origin_IATA_code", "dest_IATA_code", "OP_CARRIER")) %>%
  group_by(origin_IATA_code, dest_IATA_code, OP_CARRIER)  

#----------------------------- ANALYSIS ----------------
#------------------------- Top 10 busiest round trip flights--------------------
#Top 10 busiest round-trip routes 
top10_busiest_routes <- flights %>%
  group_by(origin_IATA_code, dest_IATA_code) %>%
  summarize(num_routes = n()) %>% arrange(desc(num_routes)) %>% #count the # of rows with same routes and arrange in desc order
  head(12)  
top10_busiest_routes #show the top busiest airports

#--------------------------

#FUNCTION 4 - top_n_values

#---------------------------
#----------------------- Top Profitable Roundtrip Routes ------------------
#--------------------- Cost Analysis ------------------
#The 10 most profitable round-trip routes
#Known parameters
OM_rate <- 8 #O&M cost per mile
misc_rate <- 1.18 # miscellaneous (depreciation, insurance, and other) costs per mile
medium_airport_use_cost <- 5000 #medium airport cost
large_airport_use_cost <- 10000 #large airport cost
dep_delay_rate <- 75 #departure delay fee per minute for each delay greater than 15 min 
arr_delay_rate <- 75 #arrival delay fee per minute for each delay greater than 15 min
free_delay_minutes <- 15 #maximum free delay possible in minutes
plane_capacity <- 200 #capacity of each plane
baggage_rate <- 70 #fee for each checked bag for a round trip
p_bag_passengers <- 0.5 #proportion of passengers with checked-in bags

#-------------------------------------
flights2$OCCUPANCY_RATE <- as.numeric(flights2$OCCUPANCY_RATE) #convert occupancy rate into numeric
#create "cost" df and include features from flights and other calculated features
#Calculated cost features:
#   OM cost: fule, maintenance, crew
#   misc_cost: depreciation, insurance, other
#   airport_use_cost: cost for using airports
#   dep_delay_cost: departure delay cost
#   arr_delay_cost: arrival delay cost
cost <- flights2 %>%
  mutate(origin_airport_use_cost = ifelse(origin_airport_type == "medium_airport", medium_airport_use_cost, large_airport_use_cost), 
         dest_airport_use_cost = ifelse(des_airport_type == "medium_airport", medium_airport_use_cost, large_airport_use_cost),
         OM_cost = 2 * DISTANCE * OM_rate, 
         dep_delay_penalty_min = ifelse(DEP_DELAY - free_delay_minutes > 0, DEP_DELAY - free_delay_minutes, 0),
         arr_delay_penalty_min = ifelse(ARR_DELAY - free_delay_minutes > 0, ARR_DELAY - free_delay_minutes, 0),
         dep_delay_cost = dep_delay_rate * dep_delay_penalty_min,
         arr_delay_cost = arr_delay_rate * arr_delay_penalty_min, 
         misc_cost = round(2 * DISTANCE * misc_rate))

#Join the fare data to the cost df to calculate revenue and keep 
#the resulting cost and revenue data together
cost_revenue <- cost %>%
  mutate(n_passengers = round(plane_capacity * OCCUPANCY_RATE),              #n_passengers = # of passengers
         baggage_fee = baggage_rate * p_bag_passengers * n_passengers,
         total_revenue = round(baggage_fee + n_passengers * fare),
         total_cost = round(origin_airport_use_cost + dest_airport_use_cost + 
                              OM_cost + dep_delay_cost + arr_delay_cost + misc_cost),
         profit = total_revenue - total_cost) %>%
  arrange(desc(profit))
#Unite origin and dest airports to create a route feature that contains the names of 
#both the departure and destination airports  
cost_revenue <- cost_revenue %>% 
  unite(origin_IATA_code, dest_IATA_code, col = "route", sep = "-")
as_tibble(cost_revenue) #show preview

#Combining cost and fare_dat results in missing values since not all routes are
#available in both flights and tickets databases. 
#The best practice would be to combine the flights, tickets, and airport_codes
#databases at the beginning of the analysis and eliminate routes that do not exist in 
#all the three databases.  
#The number of distinct routes for the tickets and flights db can be shown as
tickets %>% group_by(origin_IATA_code, dest_IATA_code) %>% 
  select(origin_IATA_code, dest_IATA_code) %>% unique() %>% nrow()
flights %>% group_by(origin_IATA_code, dest_IATA_code) %>% 
  select(origin_IATA_code, dest_IATA_code) %>% unique() %>% nrow()
#Delete rows containing NAs which are
#generated because of the combination of the tickets database (from fare_dat df) and
#a portion of the flights dataset (the cost df).
cost_revenue <- cost_revenue %>% drop_na()
#------------------------------------------------------
#-------------------- Visualize------------------
#Number of data points are important since some flights may seem 
#profitable but may have only a small number of data to be reliable.
#First we find the number of times there were flights in each routes is computed.
n_routes <- cost_revenue %>%
  group_by(route) %>%
  summarise(n = n(), profit) %>%
  arrange(desc(profit))
#The frequency of flights in each route ranges as follows:
range(n_routes$n)
#The frequency could be visually inspected as follows (only the left portion of the histogram is shown)
n_routes %>% select(route, n) %>% distinct() %>% arrange(n) %>% head(50) %>%
  ggplot(aes(x = reorder(route, n), n)) + geom_bar(stat = "identity") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#Routes where frequency of flights was below 20 were eliminated from consideration.
s <- 20 #minimum sample size
top_profitable_routes <- n_routes %>% filter(n >= s)  #disregard data with small sample size
#The top 15 routes based on average profitability
top_10_routes <- n_routes %>% group_by(route) %>%
  summarize(ave_profit = mean(profit)) %>% 
  arrange(desc(ave_profit)) %>%
  head(15)
top_10_routes
#Show the distribution of profits of the top most profitable routes 
top_profitable_routes %>% 
  filter(route  %in% top_10_routes$route) %>%
  arrange(desc(profit)) %>% 
  ggplot(aes(x = reorder(route, -profit), profit)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#The boxplots show the top profitble routes and their median and ranges of profits 
#It can be clearly seen that the route SLC-TWF seems that it is significantly  
#profitable than the rest. The main factor of high revenue is high fare. Let's
#look at the fare for this route. 
t2 <- cost_revenue %>% filter(route == "SLC-TWF") 
range(t2$fare) #Range of fare for the flight
#The fare is too high considering the flight is relatively short. 
#This, therefore is considered an outlier resulted because of incorrect fare entry
#in the dataset.The route information should therefore, be eliminated from consideration.
cost_revenue <- cost_revenue %>% filter(!(route == "SLC-TWF")) #remove outlier
top_profitable_routes <- top_profitable_routes %>% filter(!(route == "SLC-TWF"))
n_routes <- n_routes %>% filter(!(route == "SLC-TWF"))
#show updated boxplot of the top profitable routes
top_profitable_routes %>% 
  filter(route  %in% top_10_routes$route) %>%
  arrange(desc(profit)) %>% 
  ggplot(aes(x = reorder(route, -profit), profit)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#The top 5 most profitable routes are: 
#
#Although they have high variability in amount of profit, it can be seen that 
#these routes are the most profitable

#Too low frequency of flights may indicate a low demand from the public
#planes may end up not flying that often of the route is chosen, which will delay
#breakeven. On the other hand, routes with high frequency may suffer from low
#occupancy rate. We must investigate to make sure that the top performing routes are 
#actually recommended
top_profitable_routes %>% 
  filter(route %in% top_10_routes$route) %>% select(route, n) %>% distinct() 

#How many times is the flights

#--------------------------------------
cost_revenue %>% ggplot(aes(DISTANCE, OCCUPANCY_RATE)) + geom_smooth()





#cost, revenue, and profit as a function of distance
cost_revenue %>% select(DISTANCE, total_cost, total_revenue, profit) %>%
  ggplot(aes(x = DISTANCE, profit)) + 
  geom_point() +
  geom_point(aes(x = DISTANCE, y = total_cost)) +
  geom_point(aes(x = DISTANCE, y = total_revenue)) +
  legend(20, 20, legend = c("Profit", "Total Cost", "Total Revenue"))

cost_revenue %>% ggplot(aes(DISTANCE)) + geom_boxplot()
r1 <- cost_revenue %>% filter(DISTANCE > 500 & DISTANCE < 2000) %>%
  ggplot(aes(DISTANCE, profit)) + geom_smooth()
r2 <- cost_revenue %>% filter(DISTANCE > 500 & DISTANCE < 2000) %>% 
  ggplot(aes(DISTANCE, total_cost)) + geom_smooth()
r3 <- cost_revenue %>% filter(DISTANCE > 500 & DISTANCE < 2000) %>%
  ggplot(aes(DISTANCE, total_revenue)) + geom_smooth()
gridExtra::grid.arrange(r1, r2, r3, nrow = 1)

cost_revenue %>% select(DISTANCE, total_cost, total_revenue, fare) %>%
  ggplot(aes(DISTANCE, fare)) + geom_smooth()
cost_revenue %>% select(DISTANCE, total_cost, total_revenue, profit) %>%
  ggplot(aes(DISTANCE, total_revenue)) + geom_smooth()
cost_revenue %>% select(DISTANCE, total_cost, total_revenue, profit) %>%
  ggplot(aes(DISTANCE, total_cost)) + geom_smooth()




#-----------------Visualize-------------
#------1 - Fare and profit
cost_revenue %>% 
  group_by(route) %>%
  summarize(fare, profit) %>% unique() %>%
  ggplot(aes(fare, profit)) +
  geom_point()
#ave_fare vs occupancy rate  
cost_revenue %>% 
  group_by(route) %>%
  summarize(ave_occupancy_rate, profit) %>% unique() %>%
  arrange(desc(profit))
  as_tibble()
  
  ggplot(aes(ave_occupancy_rate, profit)) +
  geom_point()
#ave_occupancy rate vs profit for top profitable carriers
cost_revenue %>% 
  group_by(OP_CARRIER) %>% 
  arrange(desc(profit)) %>%
  ggplot(aes(OP_CARRIER, profit)) +
  geom_boxplot()
#distance vs cost --- DELETE THIS
cost_revenue %>% group_by(distance) %>% 
  select(total_cost) %>%
  unique() %>%
  ggplot(aes(distance, total_cost)) +
  geom_point()
# profit of different routes  
cost_revenue %>%
  group_by(route) %>%
  summarise(ave_profit = mean(profit)) %>%
  arrange(desc(ave_profit)) %>% 
  head(30) %>%
  ggplot(aes(x = reorder(route, ave_profit), ave_profit)) +
  geom_bar(stat = "identity") +
  coord_flip()

#Profit bar chart
cost_revenue %>% 
  group_by(route) %>% 
  #arrange(desc(profit)) %>%
  ggplot(aes(OP_CARRIER, profit)) +
  geom_boxplot()
ggplotly(p2, tooltip = "all")


#---------------------




#...
#List of top profitable routes
top_profitable_routes <- top_%>% select(c(origin_IATA_code, dest_IATA_code))  
as_tibble(top_p_routes)
#select data for all instances of top routes from the dataframe
cost_revenue %>% 
  filter(route %in% top_p_routes$route) %>% 
  ggplot(aes(origin_IATA_code, profit)) +
  geom_boxplot()














































