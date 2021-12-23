Sys.setlocale('LC_CTYPE', 'Chinese')

install.packages('remotes')
install.packages('forcats')
install.packages('shadowtext')
install.packages("ggplotify")
install.packages('ggrepel')
install.packages('cowplot')

library(remotes)
remotes::install_github("GuangchuangYu/nCov2019")

library(ggplot2)
library(nCov2019)
library(forcats)
library(dplyr)

x <- get_nCov2019()

head(x)

x['global',]

summary(x, by='today')

# 中国近一个月的疫情情况
ggplot(summary(x, by='today'), aes(as.Date(date, "%m.%d"), as.numeric(cum_confirm))) +
  geom_col(fill='firebrick') + theme_minimal(base_size = 14) +
  xlab('日期') + ylab('人数') + 
  labs(caption = paste("accessed date:", time(x)))


# 上海
d = x['上海',]
d$cum_confirm=as.numeric(d$cum_confirm)
d$name = fct_reorder(d$name, d$cum_confirm)

ggplot(d, aes(name, cum_confirm)) + 
  geom_col(fill='steelblue') + coord_flip() +
  geom_text(aes(y = cum_confirm+2, label=cum_confirm), hjust=0) +
  theme_minimal(base_size=14) + 
  scale_y_continuous(expand=c(0,10)) +
  xlab(NULL) + ylab(NULL)

# 每日新增
head(x[by='today'], 10)

# 历史数据
x = load_nCov2019()
d = x['global']
d = d[d$country != 'China',]
n <- d %>% filter(time == time(x)) %>%
  top_n(10, cum_confirm) %>% 
  arrange(desc(cum_confirm)) %>%
  pull(country)
require(ggplot2)
require(ggrepel)
ggplot(filter(d, country %in% n), 
       aes(time, cum_confirm, color=country)) + 
  geom_line() +
  geom_text_repel(aes(label=country), 
                  function(d) d[d$time == time(x),]) +
  theme_minimal(base_size=14) + 
  theme(legend.position = "none") +
  xlab(NULL) + ylab(NULL)

pp = lapply(n[1:4], function(cc) plot(x, region=cc))
cowplot::plot_grid(plotlist=pp)
