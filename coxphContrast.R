##small example
library(survival)

set.seed=101
delay <- c(rbinom(10,20,0.5),rep(1,5))
logic <- rbinom(15,1,0.7)
subpop <- rep(c(1,2,3),5)

dat <- data.frame(delay=delay,logic=logic,subpop=subpop)

mod <- coxph(Surv(delay,logic)~factor(subpop))
summary(mod)

#fixed effect
mod2 <- coxph(Surv(delay,logic)~factor(subpop)-1)
summary(mod2)

#random effect
mod3 <- coxph(Surv(delay,logic)~frailty(subpop,df=2))



