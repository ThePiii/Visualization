USA<-subset(histable,country=="USA")
p1 = ggplot(USA,aes(x=date, y=cases))+geom_point(color=2)+labs(title='USA',
                                                       x = 'Date',
                                                       y = 'Cases')

UK<-subset(histable,country=="UK")
p2 = ggplot(UK,aes(x=date, y=cases))+geom_point(color=3) + labs(title='UK',
                                                         x = 'Date',
                                                         y = 'Cases')
Ger = subset(histable, country=="Germany")
Ger = subset(Ger, date<=as.Date('2021-07-01'))
p3 =ggplot(Ger,aes(x=date, y=cases-deaths-recovered))+geom_point(color=4)+ geom_line(size=1) +labs(title='Germany',
                                                                                       x = 'Date',
                                                                                       y = 'Cases')
p3
p5 =ggplot(Ger,aes(x=date, y=cases-deaths-recovered))+geom_point(color=4)+ geom_line(size=1) +labs(title='Germany',
                                                                                                   x = 'Date',
                                                                                                   y = 'Cases')
p4 = ggplot(subset(histable,country=="Australia"),aes(x=date, y=cases))+geom_point(color=5)+labs(title='Australia',
                                                                                          x = 'Date',
                                                                                          y = 'Cases')

multiplot(p1, p2, p3, p4, cols=2)

multiplot(p3, p5, cols=1)
# 画在一张图看不清楚
developed = filter(histable, country %in% c('USA', 'UK', 'Germany','Australia'))
developed_reshape = melt.data.frame(developed[c(1:3)], id.vars=c('date','country'))
ggplot(developed_reshape, aes(x=date, y=value, group=country, color=country), group_by(country)) + geom_point()

# 部分阿美
SubUSA<-subset(USA,date<"2021-07-01")
ggplot(SubUSA,aes(x=date, y=cases, color=cases))+geom_point() +
  annotate("rect", xmin=as.Date('2021-02-01'), xmax=as.Date('2021-07-01'), ymin=0, ymax=35000000, alpha=.1, fill="blue") +
  labs(title='USA', x='Date',y='Cases')

SubUK<-subset(UK,date<"2021-06-15")
ggplot(SubUK,aes(x=date, y=cases-deaths-recovered, color=cases))+geom_point() + geom_line() +
  labs(title='UK', x='Date',y='Active')
