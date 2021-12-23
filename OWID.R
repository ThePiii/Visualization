'OWID'
library(ggplot2)
library(nCov2019)
library(forcats)
library(dplyr)
library(reshape)
library(ggrepel)

setwd('D:/Study/研究生/数据处理与可视化/课设')
covid = read.csv('owid-covid-data.csv', header=TRUE)

vaccine = covid[c(3, 4, 35:50)]
people_fully_vaccinated = vaccine[c(1, 2, 4, 5)]
people_fully_vaccinated_reshape = melt.data.frame(people_fully_vaccinated, id.vars=c('location','date'))
china_vaccinated = filter(people_fully_vaccinated_reshape, location=='China', value>0)
ggplot(china_vaccinated, aes(x=date, y=value, group=variable), group_by(variable)) +   # 9月15日全程接种人次过10e
  geom_line(aes(color=variable)) +
  geom_point(aes(color=variable), size = 2) +
  geom_label_repel(aes(label = paste(value)))

china_fully_vaccinated = filter(china_vaccinated, variable=='people_fully_vaccinated')

# 人口密度
populationdensity = data.frame(covid[c(3, 4, 5, 48, 49, 50)])
populationdensity = filter(populationdensity, as.Date(date)==time, population_density < 500)
ggplot(populationdensity, aes(x=population_density, y=log(total_cases),color=log(total_cases), size=population_density)) + geom_point()

# stringency index
stringency = data.frame(covid[c(3, 4, 5, 6, 48)])
stringency_China = filter(stringency, date<=as.Date('2021-11-09'), location=='China')
stringency_China = mutate(stringency_China, log(total_cases))
stringency_China_reshape = melt.data.frame(stringency_China, id.vars=c('date', 'location'))
stringency_China_reshape = filter(stringency_China_reshape, variable%in% c('log(total_cases)', 'stringency_index'))
stringency_China_num = stringency_China[c(3, 4, 5)]
stringency_China_num = scale(stringency_China_num)

stringency_China = cbind(stringency_China[c(1, 2)], stringency_China_num)
stringency_China_reshape = melt.data.frame(stringency_China, id.vars=c('date', 'location'))
ggplot(stringency_China_reshape, aes(x=date, y=value, group=variable, color=variable), group_by=variable) + geom_line()

# newcases-stringencyIndex
stringency = data.frame(covid[c(3, 4, 6, 48)])
stringency_China = filter(stringency, date<=as.Date('2021-11-09'), location=='China')
stringency_China_reshape = melt.data.frame(stringency_China, id.vars=c('date', 'location'))
ggplot(stringency_China_reshape, aes(x=as.Date(date), y=value, group=variable, color=variable), group_by=variable) + geom_line()
ggplot(stringency_China, aes(x=date, y =stringency_index, group = location)) + geom_line()

stringency = data.frame(covid[c(3, 4, 48)])
stringency_world = filter(stringency, date <= as.Date('2020-05-01'), location %in% c('China','United State','United Kingdom','Brazil','India','Japan','United States')) %>%
  group_by(location)

ggplot(stringency_world, aes(x=as.Date(date), y=stringency_index, group=location, color=location), group_by(location)) + geom_line(size=1)+
  labs(title = 'StringencyIndex',
       x = '日期',
       y = NULL)
cases = data.frame(covid[c(3,4,5)])
cases_world = filter(cases, date <= as.Date('2020-12-31'), location %in% c('China','United State','United Kingdom','Brazil','India','Japan','United States')) %>%
  group_by(location)
ggplot(cases_world, aes(x=as.Date(date), y=total_cases, group=location, color=location), group_by(location)) + geom_line(size=1)+
  labs(title = 'TotalCases',
       x = '日期',
       y = NULL)

# USA
USA_covid = filter(covid, location == 'United States', as.Date(date)<as.Date('2021-07-01'))
USA_covid = USA_covid[c(3:5,8)]