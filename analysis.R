library(dplyr)
library(ggplot2)
library(tidyverse) 
library(usmap)
library(stringr)

incar_df <- read.csv("incarceration_trends.csv")

pop_state <- select(incar_df, "year", "state", "total_pop")

#Function that takes in state and displays year, state, total population
state_pop <- function(State){
  state_df <- pop_state[pop_state$state == State,]
}


#Function average US Jail population for input state 
avg_pop_state <- function(State){
  state_df <- pop_state[pop_state$state == State,]
  avg_pop <- mean(state_df$total_pop, na.rm = TRUE)
  return(avg_pop)
}

#Calc value 1: creates df that presents the average of each state population/(min max states)
avg_state_pop <- summarise_at(group_by(incar_df, state), vars(total_pop), funs(mean(.,na.rm=TRUE)))
round_avg_state_pop<- avg_state_pop %>%
  mutate_if(is.numeric,
            round,
            digits = 0)
View(round_avg_state_pop)
max_round <- max(round_avg_state_pop$total_pop)
View(max_round)
min_round <- min(round_avg_state_pop$total_pop)
View(min_round)
print("The state that has the max average US total jail population is DC, with 632,125 persons and the state with the minimum average is SD with 11,265 persons.")


#map that shows average jail population
my_data = data("round_avg_state_pop")
plot_usmap(data = round_avg_state_pop, values = "total_pop") +
  labs(title = "Average US Jail Population")

#Function that takes in state and displays year, state, race population
race_state_pop <- select(incar_df, "year", "state", "aapi_pop_15to64","black_pop_15to64","latinx_pop_15to64", "native_pop_15to64", "white_pop_15to64")
race_st_pop <- na.omit(race_state_pop)
race_pop <- function(State){
  race_df <- race_st_pop[race_st_pop$state == State,]
}


#Calc value 2: creates df that presents the average of each race group per state (15 to 64)
avg_state <- summarise_at(group_by(race_st_pop, state), vars(aapi_pop_15to64,black_pop_15to64,latinx_pop_15to64, native_pop_15to64, white_pop_15to64), funs(mean(.,na.rm=TRUE)))
round_avg_state<- avg_state %>%
  mutate_if(is.numeric,
            round,
            digits = 0)
View(round_avg_state)

max_aapi <- max(round_avg_state$aapi_pop_15to64)
View(max_aapi)
min_aapi <- min(round_avg_state$aapi_pop_15to64)
View(min_aapi)
print("For AAPI (15 to 64) SD had the minimum with 79 and HI with 102,928 as the max average population.")

max_black <- max(round_avg_state$black_pop_15to64)
View(max_black)
min_black <- min(round_avg_state$black_pop_15to64)
View(min_black)
print("For Black (15 to 64) MT had the minimum with 56 and DC with 224,791 as the max average population.")

max_lat <- max(round_avg_state$latinx_pop_15to64)
View(max_lat)
min_lat <- min(round_avg_state$latinx_pop_15to64)
View(min_lat)
print("For Latinx (15 to 64) ND had the minimum with 147 and CA with 136,439 as the max average population.")

max_nat <- max(round_avg_state$native_pop_15to64)
View(max_nat)
min_nat <- min(round_avg_state$native_pop_15to64)
View(min_nat)
print("For Native (15 to 64) WV had the minimum with 44 and AZ with 10,716 as the max average population.")

max_white <- max(round_avg_state$white_pop_15to64)
View(max_white)
min_white <- min(round_avg_state$white_pop_15to64)
View(min_white)
print("For White (15 to 64) SD had the minimum with 6,628 and MA with 247,833 as the max average population.")


#Calc value 3: The range of how Black US jail population change (2018-1970)?
black_range <- max(blackpop_4_df$black_pop_15to64) - min(blackpop_4_df$black_pop_15to64)
print(black_range)

#Calc value 4 : The range of how White US jail population change (2018-1970)?
white_range <- max(whitepop_4_df$white_pop_15to64) - min(whitepop_4_df$white_pop_15to64)
print(white_range)

#df: average population comparing White/Black age 15 to 64
black_white_avg <- select(round_avg_state, state, black_pop_15to64, white_pop_15to64)
View(black_white_avg)


#dataframe with POC only 
POC_df <- select(round_avg_state, state, aapi_pop_15to64,black_pop_15to64,latinx_pop_15to64, native_pop_15to64)
View(POC_df)

#adding all other races beside white together (shows POC vs White)
colnms=c("aapi_pop_15to64","black_pop_15to64","latinx_pop_15to64", "native_pop_15to64")
POC_comnb <- POC_df$avg_poc_poplt<-rowSums(POC_df[,colnms])

POC_avg <- select(POC_df, state, avg_poc_poplt)
View(POC_avg)

#map that shows average POC jail population
my_data = data("POC_avg")
plot_usmap(data = POC_avg, values = "avg_poc_poplt") +
  labs(title = "Average US POC Jail Population")

#average white population
avg_white <- select(round_avg_state, state, white_pop_15to64)

#map that shows average White jail population
my_data = data("avg_white")
plot_usmap(data = avg_white, values = "white_pop_15to64") +
  labs(title = "Average US White Jail Population")

#population of the U.S jail population
population_df <- select(incar_df, year, total_pop)

#add total population to every 4 years

pop_4_df <- do.call(rbind,lapply(c('1970', '1974', '1978', '1982', '1986', '1990', '1994', '1998', '2002', '2006', '2010', '2014', '2018'),
                             function(x) {
                               x1 <- population_df[grep(x, population_df$year, ignore.case=TRUE),]
                               data.frame(year= x, total_pop=sum(x1$total_pop))}))


#Trends over time: Plot of US Jail Population (1970-2018)
plot(pop_4_df$year, pop_4_df$total_pop, 
     main = "Total US Jail Population",
     xlab = "Year",
     ylab = "Population",
     type = "l")


#total black population 15-64  
blackpopulation_df <- select(incar_df, year, black_pop_15to64)

#Calc value 5: Total Black population every 4 years 1990-2018
blackpop_4_df <- do.call(rbind,lapply(c('1990', '1994', '1998', '2002', '2006', '2010', '2014', '2018'),
                                  function(x) {
                                    x1 <- blackpopulation_df[grep(x, blackpopulation_df$year, ignore.case=TRUE),]
                                    data.frame(year= x, black_pop_15to64=sum(x1$black_pop_15to64))}))

#white population every 4 years
whitepop_4_df <- do.call(rbind,lapply(c('1990', '1994', '1998', '2002', '2006', '2010', '2014', '2018'),
                                      function(x) {
                                        x1 <- whitepopulation_df[grep(x, whitepopulation_df$year, ignore.case=TRUE),]
                                        data.frame(year= x, white_pop_15to64=sum(x1$white_pop_15to64))}))


#Calc value 6: Total White population 15-64 every 4 years 1990-2018
whitepopulation_df <- select(incar_df, year, white_pop_15to64)

#Trends over time: US Jail Population for White and Black (15 to 65) between 1990-2018 (2 Separate plots)

plot(blackwhitepop$year, blackwhitepop$black_pop_15to64, 
     main = "Total US Black Jail Population (Age 15-64)",
     xlab = "Year",
     ylab = "Population",
     type = "l")

plot(blackwhitepop$year, blackwhitepop$white_pop_15to64, 
     main = "Total US White Jail Population (Age 15-64)",
     xlab = "Year",
     ylab = "Population",
     type = "l")


#look at relationship between type of city and total US jail population
city_df <- select(incar_df, total_pop, urbanicity)
View(city_type_df)

#sum according to type of city
city_type_df <- do.call(rbind,lapply(c('rural', 'small/mid','suburban', 'urban'),
                                      function(x) {
                                        x1 <- city_df[grep(x, city_df$urbanicity, ignore.case=TRUE),]
                                        data.frame(urbanicity= x, total_pop=sum(x1$total_pop))}))

#Variable Comparison Chart: city type and total us jail population
ggplot(city_df, aes(x=urbanicity, y=total_pop)) + geom_point()
# Change the point size, and shape
ggplot(city_df, aes(x=urbanicity, y=total_pop)) +
  geom_point(size=2, shape=15)+
  ggtitle("City Type and Total US Jail Population")+
  xlab("Type of City")+
  ylab("Total US Jail Population") 

land_area_df <- select(incar_df, total_pop, land_area)
View(land_area_df)

ggplot(land_area_df, aes(x=land_area, y=total_pop)) + geom_point()
# Change the point size, and shape
ggplot(land_area_df, aes(x=land_area, y=total_pop)) +
  geom_point(size=1, shape=10)+
  ggtitle("Land Area and Total US Jail Population")+
  xlab("Land Area (Square Miles")+
  ylab("Total US Jail Population") 

max_land <- max(land_area_df$total_pop)
View(max_land)

