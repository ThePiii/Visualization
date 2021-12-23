seir<-function(time, state, pars){ 
  with(as.list(c(state, pars)),{ 
    dS <-- S * beta * I/N 
    dE <- S * beta * I/N - E * k 
    dI <- E * k - I * (mu + gamma) 
    dR <- I * gamma
    dN <- dS + dE + dI + dR 
    
    list(c(dS,dE,dI,dR,dN)) 
  }) 
} 
N <- 3E8 # 总人口
I0 <- 89 # 期初感染数
E0 <- 0 # 期初潜伏数
RM0 <- 0 # 期初移除数
S0 = N - I0 - RM0 # 期初易感人数
init<-c(S = S0, E = E0, I = I0, R = RM0, N = N)	
time <- seq(0, 150, 1) 
pars<-c( 
  beta = 0.55,	#有效接触率
  k = 1,	#潜伏到感染的转化率 
  gamma = 0.2,	#RECOVERY 
  mu=0.02	#感染期死亡率 
) 
res.seir<-as.data.frame(lsoda(y = init, times = time, func = seir, parms = pars)) 

ggplot(res.seir) +
  geom_line(aes(x = time, y = S, col = '2 易感'))+
  geom_line(aes(x = time, y = E, col = '3 潜伏'))+
  geom_line(aes(x = time, y = I, col = '4 感染'))+
  geom_line(aes(x = time, y = R, col = '5 移除'))+
  geom_line(aes(x = time, y = N, col = '1 人口'))+
  theme_light(base_family = 'Kai') +
  scale_colour_manual("",
                      values=c(
                        "2 易感" = "cornflowerblue", "3 潜伏" = "orange",
                        "4 感染" = "darkred", "5 移除" = "forestgreen", 
                        "1 人口" = "black"
                      )
  ) +
  scale_y_continuous('')
