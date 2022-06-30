library(tidyverse)
#retrieving and reading data from csv
details <- read_csv("C:/Users/Software Engineer/Desktop/commom/R_assignment/sem2/assignment 7/StormEvents_details-ftp_v1.0_d2000_c20220425.csv")

colnames(details)

#	Limit the dataframe to have the following columns
# BEGIN_DATE_TIME,END_DATE_TIME,EPISODE_ID,EVENT_ID,STATE,STATE_FIPS,
# CZ_NAME,CZ_TYPE,CZ_FIPS,EVENT_TYPE,SOURCE,BEGIN_LAT,BEGIN_LON,END_LAT,END_LON

new_col_names <- c("BEGIN_DATE_TIME","END_DATE_TIME","EPISODE_ID","EVENT_ID","STATE","STATE_FIPS","CZ_NAME",
                  "CZ_TYPE","CZ_FIPS","EVENT_TYPE","SOURCE","BEGIN_LAT","BEGIN_LON","END_LAT","END_LON")
new_details <-details[new_col_names]

View(new_details)
#Arrange the data by the state name 
library(dplyr)
new_details <- arrange(new_details,STATE)

View(new_details)

#	Change state and county names to title case 
names(new_details) <- str_to_title(names(new_details),locale='en')
View(new_details)

#	Limit to the events listed by county FIPS (CZ_TYPE of “C”) and then remove the CZ_TYPE column 
new_details <- new_details %>% filter(Cz_type=='C') %>% subset(select = -c(Cz_type))

#	Pad the state and county FIPS with a “0” new_details

State_fips <- str_pad(new_details$State_fips,width=3,side = "left",pad = "0")
Cz_fips <- str_pad(new_details$Cz_fips,width=3,side = "left",pad = "0")

new_details <- new_details %>% unite(col='fips',c('State_fips','Cz_fips'),sep=" ", remove = TRUE)

#	Change all the column names to lower case 
new_details <- new_details %>% rename_all(tolower)

#states data from base R 
data("state")
us_state_info<-data.frame(state=state.name, region=state.region, area=state.area)
us_state_info

#the number of events per state in the year of your birth
events<- data.frame(table(new_details$state))
newset<-rename(events, c("state"="Var1"))


#merging
us_state_info <-us_state_info %>% mutate_all(toupper) 
merged <- merge(x=newset,y=us_state_info,by.x="state", by.y="state")
merged

#plot
library(ggplot2) 
storm_plot <- ggplot(merged, aes(x = area, y = Freq)) +
  geom_point (aes (color = region)) + 
  labs (x = "Land area (square miles)",
        y="# of storm events in 2000")

storm_plot
