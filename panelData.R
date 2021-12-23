# 截面数据
res = query()
history = res$historical
global = history['global']
latest = res$latest
detail = latest$detail

SubDetail<-subset(detail, country %in% c("Canada","UK","USA","New Zealand","Iceland","Japan","Luxembourg","Netherlands","Switzerland","Russia","France","India","China","Mexico","Brazil","Australia","S.Korea","Germany","Afghanistan","Iran","Israel","Singapore","South Africa","Italy","Algeria","Argentina"))

ggpairs(latest$table[,c(2,3,9,10)])

cor(SubDetail$casesPerOneMillion,SubDetail$deathsPerOneMillion)

ggplot(SubDetail, aes(x=casesPerOneMillion, y=deathsPerOneMillion, color=country, size=deathsPerOneMillion)) + geom_point()+
  geom_text(aes(x=120000, y=0), color = 'black', size = 5, label='Corr=0.6561257')

# 由于较高的线性相关性，可以对cases和deaths作线性拟合

ggplot(latest$table,aes(x=cases, y=deaths))+geom_point()+geom_smooth(method='lm') 

# 尽管散点大致在一条直线上，可以看出有很多点明显地偏离了拟合值。
ggplot(SubDetail, aes(x=reorder(countryInfo,testsPerOneMillion), y=testsPerOneMillion)) +   geom_bar(stat="identity", fill="lightblue", colour="black")

var(SubDetail$testsPerOneMillion)/mean(SubDetail$testsPerOneMillion)

