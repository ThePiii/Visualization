library(ggplot2)
library(nCov2019)
library(forcats)
library(dplyr)
library(reshape)
library(ggrepel)
library(GGally)

setwd('D:/Study/研究生/数据处理与可视化/课设')
covid = read.csv('owid-covid-data.csv', header=T)

res = query()
history = res$historical
global = history['global']
latest = res$latest

time = as.Date('2021-11-30')

'世界趋势'
world_trend = 
  global %>% 
  group_by(date) %>%
  summarise(date = date,
            cases = sum(cases)) %>%
  distinct()

ggplot(world_trend, aes(date, cases), size = 2, subset(date<=time)) + geom_point(aes(color=cases))

ggplot(world_trend, aes(date, cases), size = 2, subset(date<=time)) + geom_point(aes(color=cases)) + annotate("rect", xmin=as.Date('2020-01-22'), xmax=as.Date('2020-09-30'), ymin=0, ymax=34012348, alpha=.1, fill="blue")

ggplot(world_trend, aes(date, cases), size = 2, subset(date<=time)) + geom_point(aes(color=cases)) + 
  annotate("rect", xmin=as.Date('2020-01-22'), xmax=as.Date('2020-09-30'), ymin=0, ymax=34012348, alpha=.1, fill="blue") +
  annotate('segment', x=as.Date('2021-06-01'), xend=as.Date('2021-11-15'), y=240000000, yend = 262797494 ,color='blue', size=2, arrow=arrow())+
  geom_text(aes(x=as.Date('2021-04-30'), y=230000000), color='black', size=5, label=('2.6亿人'))

plot(res$latest, region="Global" ,date = "2021-11-30", type="cases")

tmp <- history["global"] %>%
  group_by(country) %>%
  arrange(country,date) %>%
  mutate(diff = cases - lag(cases, default =  first(cases))) %>%
  filter(country %in% c("Australia", "Japan", "Italy", "Germany",  "China")) 

ggplot(tmp,aes(date, log(diff+1), color=country)) + geom_line() +
  labs(y="Log2(daily increase cases)") + 
  theme(axis.text = element_text(angle = 15, hjust = 1)) +
  scale_x_date(date_labels = "%Y-%m-%d") + 
  theme_minimal()

'中国'
China = filter(global, country=='China', date<=as.Date('2021-08-04'))
China_reshape = melt.data.frame(China, id.vars=c('country', 'date'))

ggplot(China_reshape, aes(date, value), group_by(variable)) + geom_point(aes(color=variable))
ggplot(China_reshape, aes(date, value), group_by(variable)) + geom_point(aes(color=variable)) + 
  annotate("rect", xmin=as.Date('2020-01-22'), xmax=as.Date('2020-04-30'), ymin=0, ymax=100000, alpha=.1, fill="blue") +
  geom_text(aes(x=as.Date('2020-03-01'), y=105000), color='black', size=5, label=('爆发期'))

'湖北'
province = history$province
Hubei = filter(province, province=='hubei', date<=as.Date('2020-04-30'))
ggplot(Hubei, aes(x = date, y = cases),) + geom_point(aes(color=cases))

Hubei_reshape = melt.data.frame(Hubei, id.vars=c('country','province','date'))
ggplot(Hubei_reshape, aes(x=date, y=value, group=variable), group_by(variable)) +
  geom_point(aes(color=variable), size=1.5) +
  geom_line(aes(color=variable), size=1) +
  annotate("rect", xmin=as.Date('2020-04-15'), xmax=as.Date('2020-04-30'), ymin=0, ymax=75000, alpha=.1, fill="blue") + # 数据更新
  labs(title='湖北累计疫情情况',
       x = '日期',
       y = '人数')

Hubei_daily = mutate(Hubei, new_cases=c(0,diff(cases)), new_deaths=c(0,diff(deaths)), new_recovered=c(0,diff(recovered)))
ggplot(Hubei_daily, aes(x=date, y=new_cases, color=new_cases)) + geom_line() +       # 14840是临床诊断的标准变了
  labs(title='湖北疫情初期每日新增',
       x = '日期',
       y = '每日新增人数')

# Hubei_tmp = data.frame('date' = Hubei$date, 'res_cases' = Hubei$cases-Hubei$deaths-Hubei$recovered, 'recovered' = Hubei$recovered)
# Hubei_tmp_reshape = melt.data.frame(Hubei_tmp, id.vars='date')
# ggplot(Hubei_tmp_reshape, aes(x=date, y=value, group=variable), group_by(variable)) + geom_line()

'OWID'
vaccine = covid[c(3, 4, 35:50)]
people_fully_vaccinated = vaccine[c(1, 2, 4, 5)]
people_fully_vaccinated_reshape = melt.data.frame(people_fully_vaccinated, id.vars=c('location','date'))
china_vaccinated = filter(people_fully_vaccinated_reshape, location=='China', value>0)
ggplot(china_vaccinated, aes(x=date, y=value, group=variable), group_by(variable)) +   # 9月15日全程接种人次过10e
  geom_line(aes(color=variable)) +
  geom_point(aes(color=variable), size = 2) +
  geom_label_repel(aes(label = paste(value))) +
  labs(title='中国疫苗接种情况',
       x = '日期',
       y = '接种人次')

china_fully_vaccinated = filter(china_vaccinated, variable=='people_fully_vaccinated')

'偷'
x <- res$latest
y <- res$historical

country_list =  x["global"]$country[1:10]

y[country_list]  %>%
  subset( date > as.Date("2020-10-01") ) %>%
  group_by(country) %>%
  arrange(country,date) %>%
  mutate(increase = cases - lag(cases, default =  first(cases))) -> df

ggplot(df, aes(x=date, y=increase, color=country  ))+
  geom_smooth() + 
  geom_label_repel(aes(label = paste(country,increase)), 
                   data = df[df$date == max(df$date), ], hjust = 1) + 
  labs(x=NULL,y=NULL)+ 
  theme_bw() + theme(legend.position = 'none') 

covid_1130 = filter(covid, date=='2021-11-30')
head(covid_1130)

# 大嘤
UK = filter(global, country=='UK', date<=as.Date('2021-08-01'))
UK_reshape = melt.data.frame(UK, id.vars=c('date','country'))
ggplot(UK_reshape,aes(x=date, y=value, group=variable), group_by(variable)) + geom_point(aes(color=variable))+
  labs(title='UK',
       x = 'Date',
       y = 'Infected')

UK = filter(global, country=='UK', date<=time)
UK_reshape = melt.data.frame(UK, id.vars=c('date','country'))
ggplot(UK_reshape,aes(x=date, y=value, group=variable), group_by(variable)) + geom_point(aes(color=variable))+
  labs(title='UK',
       x = 'Date',
       y = 'Infected')

# 疫苗与治疗药
vaccine = res$vaccine
therapeutics = res$therapeutics
sum_vac = summary(vaccine)

ggplot(sum_vac, aes(x=phase, y=candidates)) + 
  geom_histogram(stat='identity', aes(fill=phase))+
  stat_density(geom='line', position='identity', size=1.5, aes(color='phase'))

phase = c('Phase1','Phase1/2', 'Phase2','Phase2/3','Phase3')
candidates = c(13, 9, 2, 3, 10)
sum_vac = data.frame(phase, candidates)

sum_the = summary(therapeutics)
ggplot(sum_the, aes(x=phase, y=candidates)) + 
  geom_histogram(stat='identity', aes(fill=phase))+
  stat_density(geom='line', position='identity', size=1.5, aes(color='phase'))

# 死亡率
global_death_rate = global %>% 
  group_by(date) %>%
  summarise(date = date,
            death_rate = sum(deaths)/sum(cases)) %>%
  filter(date<=time) %>%
  distinct()

ggplot(global_death_rate, aes(x=date, y=death_rate, color = death_rate)) + geom_line()

hubei_death_rate = Hubei %>%
  group_by(date) %>%
  summarise(date = date,
            death_rate = sum(deaths)/sum(cases)) %>%
  distinct()
ggplot(hubei_death_rate, aes(x=date, y=death_rate, color = death_rate)) + geom_line()

# 相关关系
tmp = latest$table
ggpairs(tmp[c(2:4, 9:10)])

# 医疗水平
medical = c('Iceland', 'Norway','Netherlands','Luxembourg','Australia', 'Finland', 'Switzerland','Sweden','Italy','Andorra')
medical10 = filter(tmp, country %in% medical)
wanted = cbind(medical10$country, medical10$recovered/medical10$cases)
wanted = rbind(wanted, c('average', sum(tmp$recovered)/sum(tmp$cases)))
wanted_df = data.frame(wanted)
ggplot(wanted_df, aes(x=X1, y = X2)) + geom_bar(stat='identity')

# 时间点
timepoint = as.Date(c('2020-03-31', '2020-08-30', '2020-12-31', '2021-06-30', '2021-08-1'))
panel = filter(history$table, date %in% timepoint) %>%
  group_by(date) %>%
  summarise(cases = sum(cases),
            deaths = sum(deaths),
            recovered = sum(recovered))

# 好偷
Y <- res$latest
plot(Y, region="Global" ,date = "2021-12-08", type="active")
