library(nCov2019)
library(dplyr)
res <- query()
history <- res$historical
province <- history$province
Hubei <- filter(province, province == "hubei")
SIR_data <- filter(Hubei, between(date, as.Date("2020-01-22"), as.Date("2020-04-30")))


SIR_data <- mutate(SIR_data, new_cases = c(0, diff(cases)))
SIR_data <- mutate(SIR_data, new_deaths = c(0, diff(deaths)))
SIR_data <- mutate(SIR_data, new_recovered = c(0, diff(recovered)))
"
SIR模型
大多数如传染病如天花、流感、肝炎、麻疹等治愈后均有很强的免疫力,退出了传染系统。
Susceptible易感染者；Infected感染者；Removed移除者（恢复者）
模型假设：1）总人数N不变，人群分为上三类，即N = S + I + R

"

#------------------------------------------------
library(deSolve)
# beta = lambda, gama = miu

SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -lambda * S * I # nolint
    dI <- lambda * S * I - miu * I
    dR <- miu * I
    list(c(dS, dI, dR))
  })
}

N <- 55000000
I <- SIR_data$cases - SIR_data$deaths - SIR_data$recovered
as.Date("2020-04-30") - as.Date("2020-01-22")
t <- 0:99
new_cases_df <- data.frame(t, I)
ggplot(new_cases_df, aes(x = t, y = I)) +
  geom_point(aes(color = I), size = 2) +
  geom_line(aes(color = I), size = 1.5) +
  labs(
    title = "湖北省百日疫情情况",
    x = "天数",
    y = "受感染人数"
  )

init <- c(S = (1 - I[1] / N - SIR_data$recovered[1] / N), I = I[1] / N, R = SIR_data$recovered[1] / N)
lambda <- 0.29132
miu <- 1 / 14
xigma <- lambda / miu
a <- c(lambda, miu)
para <- setNames(a, c("lambda", "miu"))
T <- 0:99

fit <- data.frame(ode(y = init, times = T, func = SIR, parms = para))

SIR_reshape <- melt.data.frame(fit, id.vars = c("time"))

ggplot(SIR_reshape, aes(x = time, y = value, group = variable), group_by(variable)) +
  geom_line(aes(color = variable), size = 1.5) +
  labs(
    title = "SIR模型拟合",
    x = "天数",
    y = "比率"
  )

ggplot(fit, aes(x = S, y = I)) +
  geom_line(color = "grey", size = 1.2) +
  labs(title = "I-S图（相轨线）") +
  annotate("segment", x = 0.8, xend = 0.99, y = 0, yend = 0, color = "red", size = 1, arrow = arrow()) +
  geom_text(aes(x = 0.9, y = 0.02), color = "black", size = 5, label = "初始点1") +
  geom_text(aes(x = 0.2451894, y = 0.42), color = "black", size = 5, label = "S=1/σ") +
  annotate("rect", xmin = 0, xmax = 0.2451894, ymin = 0, ymax = 0.42, alpha = .1, fill = "blue") +
  annotate("segment", x = 0.3, xend = 0.2451894, y = 0.36, yend = 0.4, color = "red", size = 1, arrow = arrow()) +
  geom_text(aes(x = 0.28, y = 0.35), color = "black", size = 5, label = "初始点2")


"
结果：拟合的并不好。
易感人数比S从1逐渐降至0；感染人数I先增长，达到巅峰后逐步下降至0；移除人数比R则是从0缓慢增长至1。
然而实际情况下新冠病毒的感染人群增加主要是因为隔离人群在隔离期间确诊，且移除比例R是在变化的，随着时间推移，防疫越来越严格而逐渐缩小，因此会出现较大的误差。
"

'
col= 1:3
matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col)
points(t, I/N)
title("SIR model 2019-nCoV", outer = TRUE, line = -2)
'