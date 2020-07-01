
library(here)
library(dplyr)
library(data.table)
library(sf)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(lubridate)

# 1st get the county-level case data
#covid.county <- read.csv('data/us-county.csv')
covid.county <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

#######################################################
#######################################################
######################################################
# A Detailed Look at Oregon
#######################################################
#######################################################
######################################################
oregon <- covid.county %>% filter(state=="Oregon") %>%
            mutate(date=as.Date(date,format="%Y-%m-%d"))


######################################################
first.phase.1 <- c("Baker", "Benton", "Clatsop", "Columbia", "Coos", "Crook", 
                   "Curry", "Deschutes", "Douglas", "Gilliam", "Grant", "Harney", 
                   "Hood River", "Jackson", "Jefferson", "Josephine", "Klamath", "Lake", "Lane", 
                   "Lincoln", "Linn", "Malheur", "Morrow", "Sherman", "Tillamook", "Umatilla", 
                   "Union", "Wallowa", "Wasco", "Wheeler", "Yamhill")

oregon <- oregon %>% 
                  mutate(phase1.date=ifelse(county %in% first.phase.1,"2020-05-15",
                                        ifelse(county %in% c("Clackamas","Marion","Polk"),"2020-05-23",
                                        ifelse(county=="Washington","2020-06-01",
                                        ifelse(county=="Multnomah","2020-06-19",NA))))) %>%
                  mutate(phase2.date=ifelse(county %in% c("Benton","Curry","Douglas","Grant",
                                                          "Jackson","Jefferson","Klamath","Lake","Linn",
                                                          "Morrow","Union","Wallowa","Wasco",
                                                          "Wheeler","Lane"),"2020-06-05",
                                      ifelse(county %in% c("Baker","Clatsop","Columbia","Coos",
                                                           "Crook", "Deschutes",
                                                           "Gilliam","Harney","Josephine",
                                                           "Malheur","Sherman","Umatilla","Yamhill"),"2020-06-06",
                                      ifelse(county == "Tillamook","2020-06-08",
                                      ifelse(county %in% c("Polk","Hood River","Marion"),"2020-06-19",NA))))) 
                         


# create daily new cases and deaths
oregon <- oregon %>% mutate(county.group=ifelse(county=="Multnomah","Multnomah",
                                         ifelse(county=="Washington","Washington",
                                         ifelse(county=="Clackamas","Clackamas",
                                         ifelse(county=="Lane","Lane",
                                         ifelse(county=="Marion","Marion","Rest of Oregon")))))) %>% 
            group_by(county.group,date) %>%
            summarise(cases=sum(cases),deaths=sum(deaths)) %>%
            arrange(county.group,date) %>% 
            mutate(new.cases=cases-lag(cases),
                   new.deaths=deaths-lag(deaths),
                   casesL1=lag(new.cases,1),
                   casesL2=lag(new.cases,2),
                   casesL3=lag(new.cases,3),
                   casesL4=lag(new.cases,4),
                   cases.ma5=(new.cases+casesL1+casesL2+casesL3+casesL4)/5,
                   deathsL1=lag(new.deaths,1),
                   deathsL2=lag(new.deaths,2),
                   deathsL3=lag(new.deaths,3),
                   deathsL4=lag(new.deaths,4),
                   deaths.ma5=(new.deaths+deathsL1+deathsL2+deathsL3+deathsL4)/5)

# order factor levels
plot.df <- oregon %>% 
  mutate(county.group=factor(county.group,
                    levels=c("Multnomah","Washington","Clackamas","Lane","Marion","Rest of Oregon")))

ggplot(subset(plot.df,date>"2020-04-15"),
       aes(x=date,y=deaths.ma5,colour=county.group,fill=county.group)) + 
  geom_bar(stat='identity',position="stack") +
  theme_tufte()+
    scale_fill_viridis_d()

# plot weekly
dates <- c("2020-04-13","2020-04-20","2020-04-27",
           "2020-05-04","2020-05-11","2020-05-18","2020-05-25",
           "2020-06-08","2020-06-15","2020-06-22","2020-06-29")
dates <- as.Date(dates,format="%Y-%m-%d")
weekly <- oregon %>% mutate(date=as.Date(date,format="%Y-%m-%d")) %>%
   filter(date %in% dates) %>% group_by(county.group) %>%
    arrange(county.group,date) %>%
     mutate(new.deaths=deaths-lag(deaths))
  
weekly <- weekly %>% 
  mutate(county.group=factor(county.group,
                             levels=c("Multnomah","Washington","Clackamas","Lane","Marion","Rest of Oregon"))) %>%
  filter(date>"2020-04-13")
ggplot(weekly,
       aes(x=factor(date),y=new.deaths,fill=county.group)) + 
  geom_bar(stat='identity',position="stack") +
  annotate("rect", fill = "forestgreen", alpha = 0.4, 
           xmin = "2020-05-18", xmax = "2020-05-25",
           ymin = -Inf, ymax = Inf) +
  annotate("text",x="2020-05-18",y=20,
           label="MAY 15-23",
           fontface=2,hjust=0) +
  annotate("text",x="2020-05-18",y=17,
           label="32 counties\nenter\nPhase 1\nReopening",
           fontface=1,hjust=0)  + 
  theme_tufte()+
  scale_fill_viridis_d(option='inferno') +
  theme(axis.text.x=element_text(angle=45)) +
  xlab("") + 
  ylab("") +
  labs(fill="County") +
  ggtitle("Weekly New Covid Deaths by Oregon County", subtitle="counties ordered by population")

####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################

# 1st get the county-level case data
#covid.county <- read.csv('data/us-county.csv')
covid.county <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

#######################################################
#######################################################
######################################################
# A Detailed Look at Oregon
#######################################################
#######################################################
######################################################
oregon <- covid.county %>% filter(state=="Oregon") %>%
  mutate(date=as.Date(date,format="%Y-%m-%d"))

county.pop <- read.csv('/Users/aaronmamula/Documents/R projects/mams-covid-19/data/oregon-county-pop.csv')
county.pop <- county.pop %>% mutate(county=trimws(gsub("County","",county)))

oregon <- oregon %>% left_join(county.pop,by=c('county')) %>%
            mutate(dayofweek=wday(date))

friday <- oregon %>% filter(dayofweek==5) %>%
            group_by(county) %>%
             arrange(county,date) %>%
              mutate(new.cases=cases-lag(cases),
                     new.cases=ifelse(is.na(new.cases),1,new.cases),
                     new.cases.pc = new.cases/(population/100000),
                     new.cases.fct = ifelse(new.cases.pc==0,0,
                                     ifelse(new.cases.pc>0 & new.cases.pc <= 5,"1-5",
                                     ifelse(new.cases.pc>5 & new.cases.pc <= 10,"6-10",
                                     ifelse(new.cases.pc>10 & new.cases.pc <= 15, "11-15",
                                     ifelse(new.cases.pc>15 & new.cases.pc <= 20, "16-20",
                                     ifelse(new.cases.pc>20, ">20",NA))))))) %>%
              mutate(new.cases.pc.trunc=ifelse(new.cases.pc<50,new.cases.pc,50))
              
#recode factor levels
friday <- friday %>% group_by(county) %>% arrange(county,new.cases)
friday <- friday %>% mutate(new.cases.fct=factor(new.cases.fct,
                                                 levels=c(NA,"0","1-5","6-10","11-15","16-20",">20")))
# reorder counties by population
friday <- friday %>% arrange(population)
friday <- friday %>% mutate(county=factor(county,levels=unique(friday$county)))

ggplot(friday,aes(x=date,y=county,fill=new.cases.pc.trunc)) + geom_tile() +
    scale_fill_viridis_c(option="plasma") +
   theme_fivethirtyeight() +
    xlab("") + 
    ylab("") + 
    labs(fill="new cases\nper 100,000 residents") +
    ggtitle("Weekly New Covid Cases by Oregon County",
            subtitle="counties ordered by population\nnew cases measured Friday-Friday")
    
  
    scale_fill_steps(low = "#132B43",
    high = "#56B1F7",
    space = "Lab",
    na.value = "grey50",
    guide = "coloursteps",
    aesthetics = "fill"
  )


