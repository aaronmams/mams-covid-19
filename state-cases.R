
library(here)
library(dplyr)
library(data.table)
library(sf)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(forcats)

# 1st get the county-level case data
#covid.county <- read.csv('data/us-county.csv')
covid.county <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

#roll up to states
covid.state <- covid.county %>% group_by(state,date) %>%
                 summarise(cases=sum(cases,na.rm=T),
                           deaths=sum(deaths,na.rm=T))

# get state populations
state.pop <- read.csv('/Users/aaronmamula/Documents/R projects/mams-covid-19/data/state_pop.csv')

# join state pop and create the daily new cases and 5-day moving avg of new cases
covid.state <- covid.state %>% left_join(state.pop,by=c('state')) %>%
                mutate(date=as.Date(date)) %>% group_by(state) %>%
                 mutate(daily.new=cases-lag(cases),
                        daily.new1=lag(daily.new,1),
                        daily.new2=lag(daily.new,2),
                        daily.new3=lag(daily.new,3),
                        daily.new4=lag(daily.new,4),
                        daily.new5=lag(daily.new,5),
                        daily.death=deaths-lag(deaths),
                        daily.death1=lag(daily.death,1),
                        daily.death2=lag(daily.death,2),
                        daily.death3=lag(daily.death,3),
                        daily.death4=lag(daily.death,4),
                        daily.death5=lag(daily.death,5),
                        ma5=(daily.new+daily.new1+daily.new2+daily.new3+daily.new4)/5,
                        ma5.death=(daily.death+daily.death1+daily.death2+daily.death3+daily.death4)/5) 

# find "Month 1" for each state
# month 1 says, "what is 30 days from the date where each state got their 100th case"
month1 <- covid.state %>% group_by(state) %>%
          arrange(state,date) %>% 
           mutate(cases.yesterday=lag(cases),
                   day1=ifelse(cases>=100 & cases.yesterday<=100,1,0)) %>%
          filter(day1==1) %>%
          filter(row_number()==1) %>%
          mutate(date1=as.Date(date),month1.date=date1+30) %>%
          select(state,pop,month1.date) %>%
          rename(date=month1.date) 

# get # of cases in "Month 1"
month1 <- month1 %>% inner_join(covid.state,by=c('state','date')) %>%
            select(-pop.y) %>% rename(pop=pop.x) %>%
             rename(month1=date) %>%
              select(state,pop,month1,cases,ma5,ma5.death)

today <- covid.state %>% filter(date=="2020-06-27") %>%
          rename(today=date) %>%
          select(state,today,cases,pop,ma5,ma5.death)

#combine and flatten
df <- month1 %>% inner_join(today,by=c('state','pop')) %>%
       mutate(cases.start=ma5.x/(pop/100000),
              cases.today=ma5.y/(pop/100000),
              death.start=ma5.death.x/(pop/100000),
              death.today=ma5.death.y/(pop/100000))

df.labels <- df %>% filter(state %in% c("Arizona","California","Texas","New York","Washington","Alabama",
                                        "Connecticut","Florida","South Carolina","Nebraska",
                                        "Iowa","Oregon"))

# plot and label some bellweather states
ggplot(df,aes(x=cases.start,y=cases.today)) + geom_point(aes(size=pop/100000),alpha=0.5)  + 
  geom_label_repel(data=df.labels,aes(label=state),
                   size = 2.5,
                   box.padding = 0.75,
                   segment.color='grey50') +
#  geom_text(data=df.labels,aes(label=state),hjust=0, vjust=0,size=3) + 
  geom_abline(intercept=0,slope=1) + xlim(c(0,55))
  
# plot the difference
plot.df <- df %>% mutate(diff=cases.today-cases.start,
                         diff.death=death.today-death.start) %>%
          filter(! state %in% c("Puerto Rico","Guam")) %>%
           arrange(diff)

plot.df <- plot.df %>%
              mutate(state = factor(state, levels=plot.df$state))

ggplot(plot.df,aes(x=state,y=diff)) + geom_bar(stat='identity',color="tomato") + 
   coord_flip() + theme_tufte() +
  xlab("") + ylab("") + ggtitle("Change in New Coronavirus Cases per Day",
                     subtitle="new cases as of June 27th relative to day 30")

# plot the difference in deaths
plot.deaths <- plot.df %>%  
  arrange(diff.death) 
  
plot.deaths <- plot.deaths %>% mutate(state=factor(state, levels=plot.deaths$state))

ggplot(subset(plot.deaths,!state%in%c("Puerto Rico","Guam")),aes(x=state,y=diff.death)) + geom_bar(stat='identity',color="tomato") + 
  coord_flip() + theme_fivethirtyeight() +
  xlab("") + ylab("") + ggtitle("Change in New Coronavirus Deaths per Capita per Day ",
                                subtitle="new deaths as of June 27th relative to day 30")

########################################################################################################
#######################################################################################################
########################################################################################################
########################################################################################################
#######################################################################################################
########################################################################################################
# The Stoplight Plot needs things organized a little differently:


#roll up to states
covid.state <- covid.county %>% group_by(state,date) %>%
  summarise(cases=sum(cases,na.rm=T),
            deaths=sum(deaths,na.rm=T))

# get state populations
state.pop <- read.csv('/Users/aaronmamula/Documents/R projects/mams-covid-19/data/state_pop.csv')

# join state pop and create the daily new cases and 5-day moving avg of new cases
covid.state <- covid.state %>% left_join(state.pop,by=c('state')) %>%
  mutate(date=as.Date(date)) %>% group_by(state) %>%
  mutate(daily.new=cases-lag(cases),
         daily.new1=lag(daily.new,1),
         daily.new2=lag(daily.new,2),
         daily.new3=lag(daily.new,3),
         daily.new4=lag(daily.new,4),
         daily.new5=lag(daily.new,5),
         daily.death=deaths-lag(deaths),
         daily.death1=lag(daily.death,1),
         daily.death2=lag(daily.death,2),
         daily.death3=lag(daily.death,3),
         daily.death4=lag(daily.death,4),
         daily.death5=lag(daily.death,5),
         ma5=(daily.new+daily.new1+daily.new2+daily.new3+daily.new4)/5,
         ma5.death=(daily.death+daily.death1+daily.death2+daily.death3+daily.death4)/5) 

# find "Month 1" for each state
# month 1 says, "what is 30 days from the date where each state got their 100th case"
month1 <- covid.state %>% group_by(state) %>%
  arrange(state,date) %>% 
  mutate(cases.yesterday=lag(cases),
         day1=ifelse(cases>=100 & cases.yesterday<=100,1,0)) %>%
  filter(day1==1) %>%
  filter(row_number()==1) %>%
  mutate(date1=as.Date(date),month1.date=date1+60) %>%
  select(state,pop,month1.date) %>%
  rename(date=month1.date) 

# get # of cases in "Month 1"
month1 <- month1 %>% inner_join(covid.state,by=c('state','date')) %>%
  select(-pop.y) %>% rename(pop=pop.x) %>%
  rename(month1=date) %>%
  select(state,pop,month1,cases,ma5,ma5.death)

today <- covid.state %>% filter(date=="2020-06-27") %>%
  rename(today=date) %>%
  select(state,today,cases,pop,ma5,ma5.death)

#combine and flatten
df <- month1 %>% inner_join(today,by=c('state','pop')) %>%
  mutate(cases.start=ma5.x/(pop/100000),
         cases.today=ma5.y/(pop/100000),
         death.start=ma5.death.x/(pop/100000),
         death.today=ma5.death.y/(pop/100000))


# plot the difference
plot.df <- df %>% mutate(diff=death.today-death.start) %>% 
  arrange(diff)
plot.df <- plot.df %>%
  mutate(state=factor(state, levels=plot.df$state))

ggplot(subset(plot.df,!state%in%c("Puerto Rico","Guam")),aes(x=state,y=diff)) + geom_bar(stat='identity',color="tomato") + 
  coord_flip() + theme_fivethirtyeight() +
  xlab("") + ylab("") + ggtitle("Change in New Coronavirus Deaths per Day",
                                subtitle="new deaths as of June 27th relative to day 60")

#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
# create a positive/negative plot of new deaths by week

#roll up to states and keep weekly data
covid.state <- covid.county %>% group_by(state,date) %>%
  summarise(cases=sum(cases,na.rm=T),
            deaths=sum(deaths,na.rm=T)) %>%
  mutate(date=as.Date(date))

weekly <- data.frame(date=as.Date(c("2020-05-08","2020-05-15","2020-05-22","2020-05-29",
                                  "2020-06-05","2020-06-12","2020-06-19","2020-06-25"))) %>%
         inner_join(covid.state,by=c('date'))

day100 <- covid.state %>% group_by(state) %>%
  arrange(state,date) %>%
  mutate(cases.yesterday=lag(cases),
         day1=ifelse(cases>=100 & cases.yesterday<= 100,1,0)) %>%
  filter(day1==1) %>%
  filter(row_number()==1) %>%
  select(state,date) %>%
  rename(date100=date)


# get state populations
state.pop <- read.csv('/Users/aaronmamula/Documents/R projects/mams-covid-19/data/state_pop.csv')

#join state pop
weekly <- weekly %>% left_join(state.pop,by=c('state')) %>%
            left_join(day100,by=c('state')) %>%
         group_by(state) %>%
         arrange(state,date) %>%
         mutate(new.deaths=deaths-lag(deaths),
                diff.new=new.deaths-lag(new.deaths),
                diff.new.pc=diff.new/(pop/100000),
                gain.loss=ifelse(diff.new>0,0,1)) %>%
         filter(! state %in% c('Puerto Rico', 'Guam','Northern Mariana Islands','Virgin Islands')) %>%
         filter(date > '2020-05-15')

weekly <- weekly %>% arrange(date100)
weekly <- weekly %>% mutate(state=factor(state,levels=rev(unique(weekly$state))))

ggplot(weekly,aes(x=date,y=state,fill=factor(gain.loss))) + 
  geom_point(aes(shape=factor(gain.loss)),size=2.5) +
  theme_fivethirtyeight() +
    scale_shape_manual(values=c(24,25)) +
  scale_fill_manual(values=c('firebrick4','limegreen')) +
  scale_x_date(position='top',breaks=c(as.Date('2020-05-22'),
                                       as.Date('2020-05-29'),
                                       as.Date('2020-06-05'),
                                       as.Date('2020-06-12'),
                                       as.Date('2020-06-19'),
                                       as.Date('2020-06-25'))) +
  xlab("") + ylab("") +
  theme(legend.position="none") +
  ggtitle("Weekly Advance/Decline in New Covid Deaths",
          subtitle="Inverted green triangles indicate weekly deaths declined from previous week\n(states ordered by date of 100th reported case)")
  


