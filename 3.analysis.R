library(dplyr)
library(ggplot2)
setwd()
mortality <- data.frame(read.csv('stmf.csv'))


yearly_deathrate_by_country <- subset(mortality,Year != 2022) %>% group_by(CountryCode,Year) %>% dplyr::summarize(RTotal=sum(RTotal)) %>% as.data.frame()
yearly_deathrate_by_country

ggplot(data=yearly_deathrate_by_country,mapping=aes(x=Year,y=RTotal,colour=CountryCode))+geom_point()+geom_smooth()

yearly_deathrate_USA_NLD <- subset(yearly_deathrate_by_country,(CountryCode == 'USA' | CountryCode == 'NLD') & Year >= 2015)
yearly_deathrate_USA_NLD 

ggplot(data=yearly_deathrate_USA_NLD,mapping=aes(x=Year,y=RTotal,colour=CountryCode))+geom_point()+geom_smooth()
