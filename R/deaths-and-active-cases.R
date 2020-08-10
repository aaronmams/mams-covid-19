library(here)
library(dplyr)
library(data.table)
library(sf)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(forcats)
library(zoo)

#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
# Difference-in-Differences Monthly New Cases and Deaths

# 1st get the county-level case data
#covid.county <- read.csv('data/us-county.csv')
covid.county <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

#roll up to states
covid.state <- covid.county %>% group_by(state,date) %>%
  summarise(cases=sum(cases,na.rm=T),
            deaths=sum(deaths,na.rm=T)) %>%
  mutate(date=as.Date(date,format="%Y-%m-%d"))

covid.state <- covid.state %>% filter(date %in% c(as.Date("2020-05-01",format="%Y-%m-%d"),
                                                  as.Date("2020-06-01",format="%Y-%m-%d"),
                                                  as.Date("2020-07-01",format="%Y-%m-%d")))


# get state populations
state.pop <- read.csv('/Users/aaronmamula/Documents/R projects/mams-covid-19/data/state_pop.csv')

covid.state <- covid.state %>% left_join(state.pop,by=c('state')) %>%
                 mutate(cases.pc=cases/pop,
                        deaths.pc=deaths/pop) %>%
                group_by(state) %>% 
                arrange(state,date) %>% 
                mutate(month.new.case = cases.pc-lag(cases.pc),
                       month.new.death = deaths.pc-lag(deaths.pc),
                       case.change=(month.new.case-lag(month.new.case))*100000,
                       death.change=(month.new.death-lag(month.new.death))*100000)

plot.df <- covid.state %>% filter(date==as.Date("2020-07-01"))
df.label <- plot.df %>% 
              filter(state %in% c("Arkansas","California","Massachusetts","Illinois","New Jersey","Arizona","South Carolina",
                                  "Florida","Texas","Michigan","New York"))
              

  ggplot(plot.df,
       aes(x=case.change,y=death.change,size=pop/1000000)) + geom_point(color="tomato",alpha=0.6) +
       ylim(-40,20) + xlim(-500,1000) + geom_hline(yintercept=0,color="grey") +
       geom_vline(xintercept=0,color="grey") +
    theme_fivethirtyeight() +
    theme(axis.title=element_text()) + ylab("Change in New Deaths (per 100k residents)")+
    xlab("Change in New Cases (per 100k residents)") +
         labs(size="Population (millions)") +
     geom_label_repel(data=df.label,aes(label=state),
                   size = 3.5,
                   box.padding = 0.75,
                   segment.color='grey50') +
    ggtitle("Difference-in-Differences: New Covid Cases and Deaths",
            subtitle="change measured: (7/1-6/1)-(6/1-5/1)")
       
##########################################################################################################
##########################################################################################################
##########################################################################################################
##########################################################################################################
# Deaths per Active Case
  
deaths <- covid.state <- covid.county %>% group_by(state,date) %>%
    summarise(cases=sum(cases,na.rm=T),
              deaths=sum(deaths,na.rm=T)) %>%
    mutate(date=as.Date(date,format="%Y-%m-%d"),
           deaths.per.case=deaths/cases)
  
deaths.active.case <- deaths %>% group_by(state) %>% 
  arrange(state,date) %>% 
  mutate(new.cases=cases-lag(cases),
         new.deaths=deaths-lag(deaths),
         rolling.28.cases=rollsum(new.cases,28,align="right",fill=NA),
         death.per.case.lag = new.deaths/rolling.28.cases,
         dpc.ma5=(death.per.case.lag+
                    lag(death.per.case.lag,1)+
                    lag(death.per.case.lag,2)+
                    lag(death.per.case.lag,3)+
                    lag(death.per.case.lag,4))/5)  
  

plot.df <- deaths.active.case %>% filter(state %in% c("Arizona","Texas","Florida",
                                          "New York","Michigan") & 
                               date >= as.Date("2020-05-01") &
                               date <= as.Date("2020-06-29"))

ggplot(plot.df,aes(x=date,y=dpc.ma5,color=state)) + geom_line() + geom_point() +
  scale_color_viridis_d() + theme_fivethirtyeight() +
  labs(color="") + 
  ggtitle("New Covid Deaths per Active Case",
          subtitle="deaths are smoothed (MA5) single day deaths;\nactive cases are cumulative new cases in the prior 28 days")

  
# bar chart of the most recent death/case data
plot.df <- deaths.active.case %>% filter( date == as.Date("2020-06-30") & 
                                !state%in%c("Virgin Islands","New Jersey",
                                            "Puerto Rico","Guam","Northern Mariana Islands",
                                            "New York") &
                                  cases>1000) %>%
  arrange(new.cases) 

plot.df <- plot.df %>%
  mutate(state=factor(state,levels=plot.df$state))

ggplot(plot.df,aes(x=state,y=dpc.ma5)) + geom_bar(stat='identity',fill='tomato') + 
  coord_flip() +
  ggtitle("Covid Deaths per Active Case by US State (6/30/2020)",
          subtitle="states with at least 1,000 total cases;\nstates ordered by new cases/day")+ 
    theme_fivethirtyeight() 
