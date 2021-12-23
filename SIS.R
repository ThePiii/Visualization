SIS <- function(time, state, parameters) {
  par = as.list(c(state, parameters))
  with(par, {
    dI = lambda* S * I - miu * I
    dS = -lambda* S * I + miu * I
    list(c(dS, dI))
  })
}
library(deSolve)
init = c(S=0.99, I =0.01)
T = 0:99
lambda=0.08
miu = 0.01
par = c(lambda, miu)
para=setNames(par, c('lambda','miu'))
fit= data.frame(ode(y = init, times =T, func = SIS, parms =para))
ggplot(fit, aes(x=time, y=I)) + geom_line() + labs(title='SISmodel', x='time', y='Infective')
