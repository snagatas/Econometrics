# example to calculate regression parapmeters
# of ice data 

dat =read.csv("data2_ice.csv")

ice=dat$ice
temp=dat$temp

# (1) estimate using lm function
result=lm(ice~temp)
summary(result)

# (2) estimate using derivation
beta_h=cov(temp,ice)/var(temp)
alph_h= mean(ice) -beta_h*mean(temp)
round(rbind(alph_h, beta_h ) ,3)

# (3) using optimazation
x <- temp;
y <- ice;
bar <- function(v) {sum((y-v[1]- v[2]*x)^2)}
ans=optim(c(0,0),bar)
round(ans$par,3)
