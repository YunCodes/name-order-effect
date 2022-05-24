library(tidyverse)
library(janitor)
library(haven)
library(RStata)
library(miceadds)
library(ritest)

# PART 2: 2012 General
nh_2012 <- read_dta("Replication/NH2012General/NH2012Data_vars1_4OSF.dta")
nh_2012_2 <- read_dta("Replication/NH2012General/NH2012Data_vars1_alternativecoding4OSF.dta")

# There are four .do files in the the folder.
# First, replicate "_FINAL4OSF.do" file


# PART 2 - B1: randomness checks
# (1) By total votes cast within a race within a township

# As the p-value is less than the significance level 0.05, 
# we can conclude that there are significant differences between the groups highlighted with “*"

# Presidential
aov(totalvotescastPres ~ NameOrderofDemocrat, 
    data = subset(nh_2012, 
                  !is.na(Total_reg) & !totalvotescastPres == 0)) %>% summary()
aov(totalvotescastPres ~ NameOrderofRepublican, 
    data = subset(nh_2012, 
                  !is.na(Total_reg) & !totalvotescastPres == 0)) %>% summary()
aov(totalvotescastPres ~ NameOrderofOther, 
    data = subset(nh_2012, 
                  !is.na(Total_reg) & !totalvotescastPres == 0)) %>% summary()

# Governer
aov(totalvotescastGuber ~ NameOrderofDemocrat, 
    data = subset(nh_2012, 
                  !is.na(Total_reg) & !totalvotescastGuber == 0)) %>% summary()
aov(totalvotescastGuber ~ NameOrderofRepublican, 
    data = subset(nh_2012, 
                  !is.na(Total_reg) & !totalvotescastGuber == 0))  %>% summary()
aov(totalvotescastGuber ~ NameOrderofOther, 
    data = subset(nh_2012, 
                  !is.na(Total_reg) & !totalvotescastGuber == 0)) %>% summary()

# CD 1 & 2
aov(totalvotescastCD1 ~ NameOrderofDemocrat, 
    data = subset(nh_2012, 
                  !is.na(Total_reg) & !totalvotescastCD1 == 0)) %>% summary()
aov(totalvotescastCD1 ~ NameOrderofRepublican, 
    data = subset(nh_2012, 
                  !is.na(Total_reg) & !totalvotescastCD1 == 0)) %>% summary()
aov(totalvotescastCD1 ~ NameOrderofOther, 
    data = subset(nh_2012, 
                  !is.na(Total_reg) & !totalvotescastCD1 == 0)) %>% summary()

aov(totalvotescastCD2 ~ NameOrderofDemocrat, 
    data = subset(nh_2012, 
                  !is.na(Total_reg) & !totalvotescastCD2 == 0)) %>% summary()
aov(totalvotescastCD2 ~ NameOrderofRepublican, 
    data = subset(nh_2012, 
                  !is.na(Total_reg) & !totalvotescastCD2 == 0)) %>% summary()
aov(totalvotescastCD2 ~ NameOrderofOther, 
    data = subset(nh_2012, 
                  !is.na(Total_reg) & !totalvotescastCD2 == 0)) %>% summary()

# (2) By registered voters within a township (for all races) 
aov(Total_reg ~ NameOrderofDemocrat, 
    data = subset(nh_2012, 
                  !is.na(Total_reg) & !totalvotescastPres == 0)) %>% summary()
aov(Total_reg ~ NameOrderofRepublican, 
    data = subset(nh_2012, 
                  !is.na(Total_reg) & !totalvotescastPres == 0)) %>% summary()
aov(Total_reg ~ NameOrderofOther, 
    data = subset(nh_2012, 
                  !is.na(Total_reg) & !totalvotescastPres == 0)) %>% summary()

# (3) By registered Republican/Democrat/Undeclared - voters within a township (for all races) 
aov(Democrat ~ NameOrderofDemocrat,
    data = subset(nh_2012, 
                  !totalvotescastPres == 0)) %>% summary()
aov(Democrat ~ NameOrderofRepublican,
    data = subset(nh_2012, 
                  !totalvotescastPres == 0)) %>% summary()
aov(Democrat ~ NameOrderofOther,
    data = subset(nh_2012, 
                  !totalvotescastPres == 0)) %>% summary()

aov(Republican ~ NameOrderofDemocrat,
    data = subset(nh_2012, 
                  !totalvotescastPres == 0)) %>% summary()
aov(Republican ~ NameOrderofRepublican,
    data = subset(nh_2012, 
                  !totalvotescastPres == 0)) %>% summary()
aov(Republican ~ NameOrderofOther,
    data = subset(nh_2012, 
                  !totalvotescastPres == 0)) %>% summary()

aov(Undeclared ~ NameOrderofDemocrat,
    data = subset(nh_2012, 
                  !totalvotescastPres == 0)) %>% summary()
aov(Undeclared ~ NameOrderofRepublican,
    data = subset(nh_2012, 
                  !totalvotescastPres == 0)) %>% summary()
aov(Undeclared ~ NameOrderofOther,
    data = subset(nh_2012, 
                  !totalvotescastPres == 0)) %>% summary()

# PART 2 - C: analysis

# RACE=Presidential, METHOD 1 = OLS: first versus later
lm(dem_votesharePres ~ DemNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   data = nh2012)
lm(rep_votesharePres ~ RepNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   data = nh2012)
lm(lib_votesharePres ~ OtherNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   data = nh2012) 
lm(con_votesharePres ~ OtherNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   data = nh2012)
# why the last two regressions? 

#RACE=Presidential, METHOD 1a = OLS: second versus first & third versus first
lm(dem_votesharePres ~ DemNameOrder2 + DemNameOrder3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   data = nh2012)
lm(rep_votesharePres ~ RepNameOrder2 + RepNameOrder3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   data = nh2012)
lm(lib_votesharePres ~ OtherNameOrder2 + OtherNameOrder3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   data = nh2012)
lm(con_votesharePres ~ OtherNameOrder2 + OtherNameOrder3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   data = nh2012)

#RACE=Presidential, METHOD 1b = OLS: linear
lm(dem_votesharePres ~ DemNameOrderCentered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   data = nh2012)
lm(rep_votesharePres ~ RepNameOrderCentered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   data = nh2012)
lm(lib_votesharePres ~ OtherNameOrderCentered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   data = nh2012)
lm(con_votesharePres ~ OtherNameOrderCentered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   data = nh2012)

#RACE=Presidential, METHOD 1c = OLS: nonlinear-quadratic
lm(dem_votesharePres ~ DemNameOrderCentered + DemNameOrderCenteredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   data = nh2012)
lm(rep_votesharePres ~ RepNameOrderCentered + RepNameOrderCenteredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   data = nh2012)
lm(lib_votesharePres ~ OtherNameOrderCentered + OtherNameOrderCenteredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   data = nh2012)
lm(con_votesharePres ~ OtherNameOrderCentered + OtherNameOrderCenteredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   data = nh2012)

#RACE=Gubernatorial, METHOD 1 = OLS: first versus later
lm(dem_voteshareGuber ~ DemNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber,
   data = nh2012)
lm(rep_voteshareGuber ~ RepNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber,
   data = nh2012)
lm(lib_voteshareGuber ~ OtherNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber,
   data = nh2012)

#RACE=Gubernatorial, METHOD 1a = OLS: second versus first & last versus first
lm(dem_voteshareGuber ~ DemNameOrder2 + DemNameOrder3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber,
   data = nh2012)
lm(rep_voteshareGuber ~ RepNameOrder2 + RepNameOrder3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber,
   data = nh2012)
lm(lib_voteshareGuber ~ OtherNameOrder2 + OtherNameOrder3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber,
   data = nh2012)

#RACE=Gubernatorial, METHOD 1b = OLS: linear
lm(dem_voteshareGuber ~ DemNameOrderCentered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber,
   data = nh2012)
lm(rep_voteshareGuber ~ RepNameOrderCentered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber,
   data = nh2012)
lm(lib_voteshareGuber ~ OtherNameOrderCentered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber,
   data = nh2012)

#RACE=Gubernatory, METHOD 1c = OLS: nonlinear - quadratic
lm(dem_voteshareGuber ~ DemNameOrderCentered + DemNameOrderCenteredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber,
   data = nh2012)
lm(rep_voteshareGuber ~ RepNameOrderCentered + RepNameOrderCenteredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber,
   data = nh2012)
lm(lib_voteshareGuber ~ OtherNameOrderCentered + OtherNameOrderCenteredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber,
   data = nh2012)

#RACE=CDs, METHOD 1 = OLS: first versus later
lm(dem_voteshareCD1 ~ DemNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh2012)
lm(rep_voteshareCD1 ~ RepNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh2012)
lm(lib_voteshareCD1 ~ OtherNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh2012)

lm(dem_voteshareCD2 ~ DemNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   data = nh2012)
lm(rep_voteshareCD2 ~ RepNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   data = nh2012)
lm(lib_voteshareCD2 ~ OtherNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   data = nh2012)

#RACE=CDs, METHOD 1a = OLS: second versus first & last versus first
lm(dem_voteshareCD1 ~ DemNameOrder2 + DemNameOrder3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh2012)
lm(rep_voteshareCD1 ~ RepNameOrder2 + RepNameOrder3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh2012)
lm(lib_voteshareCD1 ~ OtherNameOrder2 + OtherNameOrder3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh2012)

lm(dem_voteshareCD2 ~ DemNameOrder2 + DemNameOrder3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   data = nh2012)
lm(rep_voteshareCD2 ~ RepNameOrder2 + RepNameOrder3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   data = nh2012)
lm(lib_voteshareCD2 ~ OtherNameOrder2 + OtherNameOrder3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   data = nh2012)

#RACE=CDs, METHOD 1b = OLS: linear
lm(dem_voteshareCD1 ~ DemNameOrderCentered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh2012)
lm(rep_voteshareCD1 ~ RepNameOrderCentered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh2012)
lm(lib_voteshareCD1 ~ OtherNameOrderCentered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh2012)

lm(dem_voteshareCD2 ~ DemNameOrderCentered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   data = nh2012)
lm(rep_voteshareCD2 ~ RepNameOrderCentered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   data = nh2012)
lm(lib_voteshareCD2 ~ OtherNameOrderCentered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   data = nh2012)

#RACE=CDs, METHOD 1c = OLS: nonlinear - quadratic
lm(dem_voteshareCD1 ~ DemNameOrderCentered + DemNameOrderCenteredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh2012)
lm(rep_voteshareCD1 ~ RepNameOrderCentered + RepNameOrderCenteredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh2012)
lm(lib_voteshareCD1 ~ OtherNameOrderCentered + OtherNameOrderCenteredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh2012)

lm(dem_voteshareCD2 ~ DemNameOrderCentered + DemNameOrderCenteredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   data = nh2012)
lm(rep_voteshareCD2 ~ RepNameOrderCentered + RepNameOrderCenteredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   data = nh2012)
lm(lib_voteshareCD2 ~ OtherNameOrderCentered + OtherNameOrderCenteredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   data = nh2012)

# Now look at the second "randomization_inference" file
#RACE=Presidential, METHOD 1 = OLS: first versus later
ritest(
  lm(dem_votesharePres ~ DemNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres, 
     data = nh2012),
  'DemNameOrder1', reps=1e4, seed=1234
)
ritest(
  lm(rep_votesharePres ~ RepNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres, 
     data = nh2012),
  'RepNameOrder1', reps=1e4, seed=1234
)
ritest(
  lm(lib_votesharePres ~ OtherNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres, 
     data = nh2012),
  'OtherNameOrder1', reps=1e4, seed=1234
)
ritest(
  lm(con_votesharePres ~ OtherNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres, 
     data = nh2012),
  'OtherNameOrder1', reps=1e4, seed=1234
)

#RACE=Gubernatorial, METHOD 1 = OLS: first versus later
ritest(
  lm(dem_voteshareGuber ~ DemNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber, 
     data = nh2012),
  'DemNameOrder1', reps=1e4, seed=1234
)
ritest(
  lm(rep_voteshareGuber ~ RepNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber, 
     data = nh2012),
  'RepNameOrder1', reps=1e4, seed=1234
)
ritest(
  lm(lib_voteshareGuber ~ OtherNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber, 
     data = nh2012),
  'OtherNameOrder1', reps=1e4, seed=1234
)

#RACE=CDs, METHOD 1 = OLS: first versus later
ritest(
  lm(dem_voteshareCD1 ~ DemNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1, 
     data = nh2012),
  'DemNameOrder1', reps=1e4, seed=1234
)
ritest(
  lm(rep_voteshareCD1 ~ RepNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1, 
     data = nh2012),
  'RepNameOrder1', reps=1e4, seed=1234
)
ritest(
  lm(lib_voteshareCD1 ~ OtherNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1, 
     data = nh2012),
  'OtherNameOrder1', reps=1e4, seed=1234
)

ritest(
  lm(dem_voteshareCD2 ~ DemNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2, 
     data = nh2012),
  'DemNameOrder1', reps=1e4, seed=1234
)
ritest(
  lm(rep_voteshareCD2 ~ RepNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2, 
     data = nh2012),
  'RepNameOrder1', reps=1e4, seed=1234
)
ritest(
  lm(lib_voteshareCD2 ~ OtherNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2, 
     data = nh2012),
  'OtherNameOrder1', reps=1e4, seed=1234
)

# Look at the third "weights_FINAL4OSF" file
#RACE=Presidential, METHOD 1 = OLS: first versus later
lm(dem_votesharePres ~ DemNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres, 
   weights=weightPres,
   data = nh2012)
lm(rep_votesharePres ~ RepNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres, 
   weights=weightPres,
   data = nh2012)
lm(lib_votesharePres ~ OtherNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres, 
   weights=weightPres,
   data = nh2012)
lm(con_votesharePres ~ OtherNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres, 
   weights=weightPres,
   data = nh2012)

#RACE=Presidential, METHOD 1a = OLS: second versus first & third versus first
lm(dem_votesharePres ~ DemNameOrder2 + DemNameOrder3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   weights=weightPres,
   data = nh2012)
lm(rep_votesharePres ~ RepNameOrder2 + RepNameOrder3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   weights=weightPres,
   data = nh2012)
lm(lib_votesharePres ~ OtherNameOrder2 + OtherNameOrder3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   weights=weightPres,
   data = nh2012)
lm(con_votesharePres ~ OtherNameOrder2 + OtherNameOrder3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   weights=weightPres,
   data = nh2012)

#RACE=Presidential, METHOD 1b = OLS: linear
lm(dem_votesharePres ~ DemNameOrderCentered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   weights=weightPres,
   data = nh2012)
lm(rep_votesharePres ~ RepNameOrderCentered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   weights=weightPres,
   data = nh2012)
lm(lib_votesharePres ~ OtherNameOrderCentered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   weights=weightPres,
   data = nh2012)
lm(con_votesharePres ~ OtherNameOrderCentered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   weights=weightPres,
   data = nh2012)

#RACE=Presidential, METHOD 1c = OLS: nonlinear-quadratic
lm(dem_votesharePres ~ DemNameOrderCentered + DemNameOrderCenteredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   weights=weightPres,
   data = nh2012)
lm(rep_votesharePres ~ RepNameOrderCentered + RepNameOrderCenteredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   weights=weightPres,
   data = nh2012)
lm(lib_votesharePres ~ OtherNameOrderCentered + OtherNameOrderCenteredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   weights=weightPres,
   data = nh2012)
lm(con_votesharePres ~ OtherNameOrderCentered + OtherNameOrderCenteredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   weights=weightPres,
   data = nh2012)

#RACE=Gubernatorial, METHOD 1 = OLS: first versus later
lm(dem_voteshareGuber ~ DemNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber,
   weights=weightGuber,
   data = nh2012)
lm(rep_voteshareGuber ~ RepNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber,
   weights=weightGuber,
   data = nh2012)
lm(lib_voteshareGuber ~ OtherNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber,
   weights=weightGuber,
   data = nh2012)

#RACE=Gubernatorial, METHOD 1a = OLS: second versus first & last versus first
lm(dem_voteshareGuber ~ DemNameOrder2 + DemNameOrder3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber,
   weights=weightGuber,
   data = nh2012)
lm(rep_voteshareGuber ~ RepNameOrder2 + RepNameOrder3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber,
   weights=weightGuber,
   data = nh2012)
lm(lib_voteshareGuber ~ OtherNameOrder2 + OtherNameOrder3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber,
   weights=weightGuber,
   data = nh2012)

#RACE=Gubernatorial, METHOD 1b = OLS: linear
lm(dem_voteshareGuber ~ DemNameOrderCentered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber,
   weights=weightGuber,
   data = nh2012)
lm(rep_voteshareGuber ~ RepNameOrderCentered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber,
   weights=weightGuber,
   data = nh2012)
lm(lib_voteshareGuber ~ OtherNameOrderCentered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber,
   weights=weightGuber,
   data = nh2012)

#RACE=Gubernatory, METHOD 1c = OLS: nonlinear - quadratic
lm(dem_voteshareGuber ~ DemNameOrderCentered + DemNameOrderCenteredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber,
   weights=weightGuber,
   data = nh2012)
lm(rep_voteshareGuber ~ RepNameOrderCentered + RepNameOrderCenteredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber,
   weights=weightGuber,
   data = nh2012)
lm(lib_voteshareGuber ~ OtherNameOrderCentered + OtherNameOrderCenteredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber,
   weights=weightGuber,
   data = nh2012)

#RACE=CDs, METHOD 1 = OLS: first versus later
lm(dem_voteshareCD1 ~ DemNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   weights=weightCD1,
   data = nh2012)
lm(rep_voteshareCD1 ~ RepNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   weights=weightCD1,
   data = nh2012)
lm(lib_voteshareCD1 ~ OtherNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   weights=weightCD1,
   data = nh2012)

lm(dem_voteshareCD2 ~ DemNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   weights=weightCD2,
   data = nh2012)
lm(rep_voteshareCD2 ~ RepNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   weights=weightCD2,
   data = nh2012)
lm(lib_voteshareCD2 ~ OtherNameOrder1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   weights=weightCD2,
   data = nh2012)

#RACE=CDs, METHOD 1a = OLS: second versus first & last versus first
lm(dem_voteshareCD1 ~ DemNameOrder2 + DemNameOrder3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   weights=weightCD1,
   data = nh2012)
lm(rep_voteshareCD1 ~ RepNameOrder2 + RepNameOrder3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   weights=weightCD1,
   data = nh2012)
lm(lib_voteshareCD1 ~ OtherNameOrder2 + OtherNameOrder3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   weights=weightCD1,
   data = nh2012)

lm(dem_voteshareCD2 ~ DemNameOrder2 + DemNameOrder3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   weights=weightCD2,
   data = nh2012)
lm(rep_voteshareCD2 ~ RepNameOrder2 + RepNameOrder3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   weights=weightCD2,
   data = nh2012)
lm(lib_voteshareCD2 ~ OtherNameOrder2 + OtherNameOrder3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   weights=weightCD2,
   data = nh2012)

#RACE=CDs, METHOD 1b = OLS: linear
lm(dem_voteshareCD1 ~ DemNameOrderCentered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   weights=weightCD1,
   data = nh2012)
lm(rep_voteshareCD1 ~ RepNameOrderCentered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   weights=weightCD1,
   data = nh2012)
lm(lib_voteshareCD1 ~ OtherNameOrderCentered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   weights=weightCD1,
   data = nh2012)

lm(dem_voteshareCD2 ~ DemNameOrderCentered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   weights=weightCD2,
   data = nh2012)
lm(rep_voteshareCD2 ~ RepNameOrderCentered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   weights=weightCD2,
   data = nh2012)
lm(lib_voteshareCD2 ~ OtherNameOrderCentered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   weights=weightCD2,
   data = nh2012)

#RACE=CDs, METHOD 1c = OLS: nonlinear - quadratic
lm(dem_voteshareCD1 ~ DemNameOrderCentered + DemNameOrderCenteredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   weights=weightCD1,
   data = nh2012)
lm(rep_voteshareCD1 ~ RepNameOrderCentered + RepNameOrderCenteredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   weights=weightCD1,
   data = nh2012)
lm(lib_voteshareCD1 ~ OtherNameOrderCentered + OtherNameOrderCenteredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   weights=weightCD1,
   data = nh2012)

lm(dem_voteshareCD2 ~ DemNameOrderCentered + DemNameOrderCenteredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   weights=weightCD2,
   data = nh2012)
lm(rep_voteshareCD2 ~ RepNameOrderCentered + RepNameOrderCenteredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   weights=weightCD2,
   data = nh2012)
lm(lib_voteshareCD2 ~ OtherNameOrderCentered + OtherNameOrderCenteredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   weights=weightCD2,
   data = nh2012)

# Look at the fourth "alternativecoding_FINAL4OSF" file
#RACE=Presidential, METHOD 1 = OLS: first versus later
#RACE=Presidential, METHOD 1a = OLS: second versus first & third versus first
#RACE=Presidential, METHOD 1b = OLS: linear
lm(dem_votesharePres ~ demorderP_centered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   data = nh_2012_2)
lm(rep_votesharePres ~ reporderP_centered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   data = nh_2012_2)
lm(lib_votesharePres ~ johnsonorderP_centered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   data = nh_2012_2)
lm(con_votesharePres ~ goodeorderP_centered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   data = nh_2012_2)
#RACE=Presidential, METHOD 1c = OLS: nonlinear-quadratic
lm(dem_votesharePres ~ demorderP_centered + demorderP_centeredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   data = nh_2012_2)
lm(rep_votesharePres ~ reporderP_centered + reporderP_centeredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   data = nh_2012_2)
lm(lib_votesharePres ~ johnsonorderP_centered + johnsonorderP_centeredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   data = nh_2012_2)
lm(con_votesharePres ~ goodeorderP_centered + goodeorderP_centeredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKPres,
   data = nh_2012_2)