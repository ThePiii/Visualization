SI <- function(time, state, parameters) {
  par = as.list(c(state, parameters))
  with(par, {
    dI = lambda* S * I
    dS = -lambda* S * I
    list(c(dS, dI))
  })
}
library(deSolve)
init = c(S=0.99, I =0.01)
T = 0:99
lambda=0.08
para=setNames(lambda, 'lambda')
fit= data.frame(ode(y = init, times =T, func = SI, parms =para))
ggplot(fit, aes(x=time, y=I)) + geom_line() + labs(title='SImodel', x='time', y='Infective')
