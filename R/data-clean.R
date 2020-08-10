
library(dplyr)
library(data.table)
library(sf)
library(here)

#---------------------------------------------------
# 1st get the county-level case data
#covid.county <- read.csv('data/us-county.csv')
covid.county <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
#----------------------------------------------------

#----------------------------------------------------
# Next get the county boundaries from Census TIGER Lines and change the county field name
county.lines <- st_read('data/CA_Counties/CA_Counties_TIGER2016.shp') %>%
  mutate(county=NAME)

ca.lines <- county.lines %>% filter(STATEFP=='06')
str(ca.lines)
#-----------------------------------------------------

#------------------------------------------------------
# Next get 2019 county population and fix the names
CA.pop <- read.csv('data/CA-county-pop.csv')
CA.pop$county <- trimws(gsub("County, California","",CA.pop$County))
#------------------------------------------------------


#--------------------------------------------------------------------
# Next, roll the case counts up to the most recent 14-day window
# the daily data are cumulative. I think the easiest way to do this is 
# to do a 14-period difference

# first thing I'm going to do is expand the data set because not every county
# is in the data for every day
first.day <- min(as.Date(covid.county$date))
last.day <- max(as.Date(covid.county$date))

ca.cases <- covid.county %>% filter(state=='California')

# create a dataframe containing all days and all California counties
ca.county.alldays <- data.frame(rbindlist(lapply(unique(CA.pop$county),function(x){
  return(data.frame(county=x,
                    state=unique(ca.cases$state[ca.cases$county==x]),
                    fips=unique(ca.cases$fips[ca.cases$county==x]),
                    date=seq.Date(from=first.day,to=last.day,by="day"),cases=0,deaths=0))
})))

# I'm rearranging the data to be new cases each day rather than cumulative cases becauses
# it's a little easier for me to deal with
ca.cases <- ca.cases %>% 
  mutate(date=as.Date(date,format="%Y-%m-%d")) %>%
  arrange(county,date) %>% 
  group_by(county) %>%
  mutate(daily_new_cases=cases-lag(cases),
         daily_new_cases=ifelse(row_number()==1,cases,daily_new_cases),
         daily_new_deaths=deaths-lag(deaths),
         daily_new_deaths=ifelse(row_number()==1,deaths,daily_new_deaths)) 


ca.cases <- ca.county.alldays %>% left_join(ca.cases,by=c('county','state','fips','date')) %>%
  mutate(daily_new_cases=ifelse(is.na(daily_new_cases),cases.x,daily_new_cases),
         daily_new_deaths=ifelse(is.na(daily_new_deaths),deaths.x,daily_new_deaths)) %>%
  select(county,date,state,fips,cases.y,deaths.y,daily_new_cases,daily_new_deaths) 
#---------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------
# now we create the cumulative cases and cumulative deaths again
ca.cases <- ca.cases %>% arrange(county,date) %>% group_by(county) %>%
  mutate(cum_daily_cases=cumsum(daily_new_cases),
         cum_daily_death=cumsum(daily_new_deaths),
         new_2wk=cum_daily_cases-lag(cum_daily_cases,14),
         new_2wk_deaths=cum_daily_death-lag(cum_daily_death,14),
         change_from_last_2wks_cases=new_2wk-lag(new_2wk,14),
         diff=ifelse(change_from_last_2wks_cases<0,"2 Week New Cases Decreasing",
                     ifelse(change_from_last_2wks_cases>0,"2 Week New Cases Increasing","No change")))
#------------------------------------------------------------------------------------------


#--------------------------------------------------------------
# join the case counts to population data

#the map is going to be a point-in-time map so we need the 
# data frame to be a single date

ca.cases <- ca.cases %>% left_join(CA.pop,by=c('county')) %>%
  mutate(cases_per_100k=new_2wk/(Pop/100000),
         deaths_per_100k=new_2wk_deaths/(Pop/100000)) %>%
  arrange(county,date) %>% group_by(county) %>%
  filter(row_number()==n())

ca.lines <- county.lines %>% filter(STATEFP=='06') %>% 
  left_join(ca.cases,by=c('county'))
#----------------------------------------------------------
