install.packages('remotes')
install.packages('forcats')
install.packages('shadowtext')
install.packages("ggplotify")
install.packages('ggrepel')
install.packages('cowplot')

library(remotes)
remotes::install_github("YuLab-SMU/nCov2019")

library(ggplot2)
library(nCov2019)
library(forcats)
library(dplyr)


#------------------------------------------------
res = query()
history = res$historical
global = history['global']
Taiwan = filter(global, country=='Taiwan')
China = filter(global, country=='China')
province = history$province
Shanghai = filter(province, province == 'shanghai')
plot(Shanghai$cases)

vaccine <- res$vaccine
vaccine['all']
therapeutics = res$therapeutics
head(vaccine$table$institutions)

latest =res$latest
head(latest$detail)

#------------------------------------------------
str(China$date)
ggplot(China, aes(x = date, y = deaths)) + geom_point()  # 为什么要对武汉市新冠肺炎确诊病例数、确诊病例死亡数进行订正？

Hubei = filter(province, province=='hubei')
ggplot(Hubei, aes(x = date, y = cases)) + geom_point()
hubei_new = mutate(hubei)

USA = filter(global, country=='USA')
cases_deaths = c(USA$cases, USA$deaths)
ggplot(USA, aes(x = date, y = cases)) + geom_point()
