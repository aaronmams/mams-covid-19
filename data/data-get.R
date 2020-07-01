
states <- read.csv('data/us-state.csv')
county <- read.csv('data/us-county.csv')

covid.states <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
covid.county <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

# check to see if the new stuff is better than what I already have


write.csv(covid.states,file='data/us-states.csv')
write.csv(covid.county,file='data/us-county.csv')
```