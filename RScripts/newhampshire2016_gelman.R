# Data at https://osf.io/z2fwu/
library(haven)
library(rstanarm)
library(tidyverse)

nh2012 <- read_dta("NH2012Data_vars1_4OSF.dta")
nh2016 <- read_dta("BallotNH2016_replicate_vars4OSF.dta")

t1 <- as.character(nh2012$Township)
t2 <- as.character(nh2016$Township)
n <- nrow(nh2016)
match1 <- rep(NA, n)

for (i in 1:n){
  match1[i] <- match(t2[i], t1)
}

rvote2016 <- as.numeric(nh2016$Votescast_Trump)
dvote2016 <- as.numeric(nh2016$Votescast_Clinton)
rfirst2016 <- as.numeric(nh2016$Nameorder_Trump==1)
dfirst2016 <- as.numeric(nh2016$Nameorder_Clinton==1)
rvote2012 <- as.numeric(nh2012$RomneyandRyanr_nh2012pres[match1])
dvote2012 <- as.numeric(nh2012$ObamaandBidend_nh2012pres[match1])
r2016 <- rvote2016/(rvote2016 + dvote2016)
r2012 <- rvote2012/(rvote2012 + dvote2012)
ok <- rvote2016 + dvote2016 > 200
nh <- data.frame(rvote2016, dvote2016, rfirst2016, dfirst2016, r2016, r2012)

# Diff

fit_1 <- stan_glm(r2016 ~ I(rfirst2016 - dfirst2016), data=nh, subset = (rvote2016 + dvote2016 > 200), refresh=0)
print(fit_1, digits=2)
fit_2 <- stan_glm(r2016 ~ I(rfirst2016 - dfirst2016) + r2012, data=nh, subset = (rvote2016 + dvote2016 > 200), refresh=0)
print(fit_2, digits=2)

# Clinton

fit_3 <- stan_glm(r2016 ~ dfirst2016, data=nh, subset = (rvote2016 + dvote2016 > 200), refresh=0)
print(fit_3, digits=2)
fit_4 <- stan_glm(r2016 ~ dfirst2016 + r2012, data=nh, subset = (rvote2016 + dvote2016 > 200), refresh=0)
print(fit_4, digits=2)

# Trump

fit_5 <- stan_glm(r2016 ~ rfirst2016, data=nh, subset = (rvote2016 + dvote2016 > 200), refresh=0)
print(fit_5, digits=2)
fit_6 <- stan_glm(r2016 ~ rfirst2016 + r2012, data=nh, subset = (rvote2016 + dvote2016 > 200), refresh=0)
print(fit_6, digits=2)

