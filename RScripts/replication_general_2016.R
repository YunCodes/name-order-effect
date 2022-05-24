library(tidyverse)
library(janitor)
library(haven)
library(RStata)
library(miceadds)
library(ritest)

# PART 2: 2016 General
nh_2016 <- read_dta("Replication/NH2016General/BallotNH2016_replicate_vars4OSF.dta")
nh_2016_2 <- read_dta("Replication/NH2016General/BallotNH2016_replicate_vars_alternativecoding4OSF.dta")

# There are four .do files in the the folder.
# First, replicate "_FINAL4OSF.do" file


# PART 2 - B1: randomness checks
# (1) By total votes cast within a race within a township

# As the p-value is less than the significance level 0.05, 
# we can conclude that there are significant differences between the groups highlighted with “*"

# Presidential
aov(Totalvotescast ~ Nameorder_Clinton, 
    data = subset(nh_2016, 
                  !is.na(Total_reg))) %>% summary()
aov(Totalvotescast ~ Nameorder_Trump, 
    data = subset(nh_2016, 
                  !is.na(Total_reg))) %>% summary()
aov(Totalvotescast ~ Nameorder_Stein, 
    data = subset(nh_2016, 
                  !is.na(Total_reg))) %>% summary()

# Senate
aov(totalvotescastSenate ~ Nameorder_Clinton, 
    data = subset(nh_2016, 
                  !is.na(Total_reg))) %>% summary()
aov(totalvotescastSenate ~ Nameorder_Trump, 
    data = subset(nh_2016, 
                  !is.na(Total_reg))) %>% summary()
aov(totalvotescastSenate ~ Nameorder_Stein, 
    data = subset(nh_2016, 
                  !is.na(Total_reg))) %>% summary()

# Gubernatorial
aov(totalvotescastGuber ~ Nameorder_Clinton, 
    data = subset(nh_2016, 
                  !is.na(Total_reg))) %>% summary()
aov(totalvotescastGuber ~ Nameorder_Trump, 
    data = subset(nh_2016, 
                  !is.na(Total_reg))) %>% summary()
aov(totalvotescastGuber ~ Nameorder_Stein, 
    data = subset(nh_2016, 
                  !is.na(Total_reg))) %>% summary()


# CD 1 & 2
aov(totalvotescastCD1 ~ Nameorder_Clinton, 
    data = subset(nh_2016, 
                  !is.na(Total_reg))) %>% summary()
aov(totalvotescastCD1 ~ Nameorder_Trump, 
    data = subset(nh_2016, 
                  !is.na(Total_reg))) %>% summary()
aov(totalvotescastCD1 ~ Nameorder_Stein, 
    data = subset(nh_2016, 
                  !is.na(Total_reg))) %>% summary()

aov(totalvotescastCD2 ~ Nameorder_Clinton, 
    data = subset(nh_2016, 
                  !is.na(Total_reg))) %>% summary()
aov(totalvotescastCD2 ~ Nameorder_Trump, 
    data = subset(nh_2016, 
                  !is.na(Total_reg))) %>% summary()
aov(totalvotescastCD2 ~ Nameorder_Stein, 
    data = subset(nh_2016, 
                  !is.na(Total_reg))) %>% summary()

# (2) By registered voters within a township (for all races) 
aov(Total_reg ~ Nameorder_Clinton,
    data = nh_2016) %>% summary()
aov(Total_reg ~ Nameorder_Trump,
    data = nh_2016) %>% summary()
aov(Total_reg ~ Nameorder_Stein,
    data = nh_2016) %>% summary()

# (3) By registered Republican/Democrat/Undeclared - voters within a township (for all races) 
aov(Democrat_reg ~ Nameorder_Clinton, data = nh_2016) %>% summary()
aov(Democrat_reg ~ Nameorder_Trump, data = nh_2016) %>% summary()
aov(Democrat_reg ~ Nameorder_Stein, data = nh_2016) %>% summary()

aov(Republican_reg ~ Nameorder_Clinton, data = nh_2016) %>% summary()
aov(Republican_reg ~ Nameorder_Trump, data = nh_2016) %>% summary()
aov(Republican_reg ~ Nameorder_Stein, data = nh_2016) %>% summary()

aov(Undeclared_reg ~ Nameorder_Clinton, data = nh_2016) %>% summary()
aov(Undeclared_reg ~ Nameorder_Trump, data = nh_2016) %>% summary()
aov(Undeclared_reg ~ Nameorder_Stein, data = nh_2016) %>% summary()

# PART 2 - C: analysis

# RACE=Presidential, METHOD 1 = OLS: first versus later
lm(voteshare_clinton ~ Nameorder_Clinton_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016)
lm(voteshare_trump ~ Nameorder_Trump_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016)
lm(voteshare_dlf ~ Nameorder_Stein_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016)
lm(voteshare_johnson ~ Nameorder_Stein_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016)
lm(voteshare_stein ~ Nameorder_Stein_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016)
lm(voteshare_other ~ Nameorder_Stein_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016)

# why the last two regressions? 

#RACE=Presidential, METHOD 1a = OLS: second versus first & third versus first
lm(voteshare_clinton ~ Nameorder_Clinton_2 + Nameorder_Clinton_3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016)
lm(voteshare_trump ~ Nameorder_Trump_2 + Nameorder_Trump_3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016)
lm(voteshare_dlf ~ Nameorder_Stein_2 + Nameorder_Stein_3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016)
lm(voteshare_johnson ~ Nameorder_Stein_2 + Nameorder_Stein_3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016)
lm(voteshare_stein ~ Nameorder_Stein_2 + Nameorder_Stein_3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016)
lm(voteshare_other ~ Nameorder_Stein_2 + Nameorder_Stein_3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016)

#RACE=Presidential, METHOD 1b = OLS: linear
lm(voteshare_clinton ~ NameorderCentered_Clinton + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016)
lm(voteshare_trump ~ NameorderCentered_Trump + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016)
lm(voteshare_dlf ~ NameorderCentered_Stein + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016)
lm(voteshare_johnson ~ NameorderCentered_Stein + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016)
lm(voteshare_stein ~ NameorderCentered_Stein + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016)
lm(voteshare_other ~ NameorderCentered_Stein + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016)

#RACE=Presidential, METHOD 1c = OLS: nonlinear-quadratic
lm(voteshare_clinton ~ NameorderCentered_Clinton + NameorderCentered_ClintonSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016)
lm(voteshare_trump ~ NameorderCentered_Trump + NameorderCentered_TrumpSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016)
lm(voteshare_dlf ~ NameorderCentered_Stein + NameorderCentered_SteinSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016)
lm(voteshare_johnson ~ NameorderCentered_Stein + NameorderCentered_SteinSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016)
lm(voteshare_stein ~ NameorderCentered_Stein + NameorderCentered_SteinSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016)
lm(voteshare_other ~ NameorderCentered_Stein + NameorderCentered_SteinSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016) 

#RACE=Senatorial, METHOD 1 = OLS: first versus later
lm(dem_voteshareSenate ~ Nameorder_Clinton_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKSenate,
   data = nh_2016)
lm(rep_voteshareSenate ~ Nameorder_Trump_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKSenate,
   data = nh_2016)
lm(lib_voteshareSenate ~ Nameorder_Stein_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKSenate,
   data = nh_2016)
lm(ind_voteshareSenate ~ Nameorder_Stein_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKSenate,
   data = nh_2016)
lm(other_voteshareSenate ~ Nameorder_Stein_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKSenate,
   data = nh_2016)

#RACE=Senatorial, METHOD 1a = OLS: second vs first & last vs first
lm(dem_voteshareSenate ~ Nameorder_Clinton_2 + Nameorder_Clinton_3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKSenate,
   data = nh_2016)
lm(rep_voteshareSenate ~ Nameorder_Trump_2 + Nameorder_Trump_3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKSenate,
   data = nh_2016)
lm(lib_voteshareSenate ~ Nameorder_Stein_2 + Nameorder_Stein_3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKSenate,
   data = nh_2016)
lm(ind_voteshareSenate ~ Nameorder_Stein_2 + Nameorder_Stein_3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKSenate,
   data = nh_2016)
lm(other_voteshareSenate ~ Nameorder_Stein_2 + Nameorder_Stein_3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKSenate,
   data = nh_2016)

#RACE=Senatorial, METHOD 1b = OLS: linear
lm(dem_voteshareSenate ~ NameorderCentered_Clinton + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKSenate,
   data = nh_2016)
lm(rep_voteshareSenate ~ NameorderCentered_Trump + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKSenate,
   data = nh_2016)
lm(lib_voteshareSenate ~ NameorderCentered_Stein + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKSenate,
   data = nh_2016)
lm(ind_voteshareSenate ~ NameorderCentered_Stein + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKSenate,
   data = nh_2016)
lm(other_voteshareSenate ~ NameorderCentered_Stein + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKSenate,
   data = nh_2016)

#RACE=Senatorial, METHOD 1c = OLS: nonlinear - quadratic
lm(dem_voteshareSenate ~ NameorderCentered_Clinton + NameorderCentered_ClintonSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKSenate,
   data = nh_2016)
lm(rep_voteshareSenate ~ NameorderCentered_Trump + NameorderCentered_TrumpSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKSenate,
   data = nh_2016)
lm(lib_voteshareSenate ~ NameorderCentered_Stein + NameorderCentered_SteinSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKSenate,
   data = nh_2016)
lm(ind_voteshareSenate ~ NameorderCentered_Stein + NameorderCentered_SteinSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKSenate,
   data = nh_2016)
lm(other_voteshareSenate ~ NameorderCentered_Stein + NameorderCentered_SteinSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKSenate,
   data = nh_2016)

#RACE=Gubernatory, METHOD 1c = OLS: nonlinear - quadratic
lm(dem_voteshareGuber ~ NameorderCentered_Clinton + NameorderCentered_ClintonSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber,
   data = nh_2016)
lm(rep_voteshareGuber ~ NameorderCentered_Trump + NameorderCentered_TrumpSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber,
   data = nh_2016)
lm(lib_voteshareGuber ~ NameorderCentered_Stein + NameorderCentered_SteinSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber,
   data = nh_2016)

#RACE=CDs, METHOD 1 = OLS: first versus later 
lm(dem_voteshareCD1 ~ Nameorder_Clinton_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016)
lm(rep_voteshareCD1 ~ Nameorder_Trump_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016)
lm(lib_voteshareCD1 ~ Nameorder_Stein_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016)
lm(indK_voteshareCD1 ~ Nameorder_Stein_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016)
lm(indO_voteshareCD1 ~ Nameorder_Stein_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016)
lm(other_voteshareCD1 ~ Nameorder_Stein_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016)

lm(dem_voteshareCD2 ~ Nameorder_Clinton_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   data = nh_2016)
lm(rep_voteshareCD2 ~ Nameorder_Trump_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   data = nh_2016)
lm(ind_voteshareCD2 ~ Nameorder_Stein_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   data = nh_2016)

#RACE=CDs, METHOD 1a = OLS: second versus first & last versus first
lm(dem_voteshareCD1 ~ Nameorder_Clinton_2 + Nameorder_Clinton_3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016)
lm(rep_voteshareCD1 ~ Nameorder_Trump_2 + Nameorder_Trump_3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016)
lm(lib_voteshareCD1 ~ Nameorder_Stein_2 + Nameorder_Stein_3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016)
lm(indK_voteshareCD1 ~ Nameorder_Stein_2 + Nameorder_Stein_3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016)
lm(indO_voteshareCD1 ~ Nameorder_Stein_2 + Nameorder_Stein_3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016)
lm(other_voteshareCD1 ~ Nameorder_Stein_2 + Nameorder_Stein_3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016)

lm(dem_voteshareCD2 ~ Nameorder_Clinton_2 + Nameorder_Clinton_3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   data = nh_2016)
lm(rep_voteshareCD2 ~ Nameorder_Trump_2 + Nameorder_Trump_3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   data = nh_2016)
lm(ind_voteshareCD2 ~ Nameorder_Stein_2 + Nameorder_Stein_3 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   data = nh_2016)

#RACE=CDs, METHOD 1b = OLS: linear
lm(dem_voteshareCD1 ~ NameorderCentered_Clinton + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016)
lm(rep_voteshareCD1 ~ NameorderCentered_Trump+ Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016)
lm(lib_voteshareCD1 ~ NameorderCentered_Stein + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016)
lm(indK_voteshareCD1 ~ NameorderCentered_Stein + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016)
lm(indO_voteshareCD1 ~ NameorderCentered_Stein + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016)
lm(other_voteshareCD1 ~ NameorderCentered_Stein + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016)

lm(dem_voteshareCD2 ~ NameorderCentered_Clinton + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   data = nh_2016)
lm(rep_voteshareCD2 ~ NameorderCentered_Trump+ Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   data = nh_2016)
lm(ind_voteshareCD2 ~ NameorderCentered_Stein + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   data = nh_2016)

#RACE=CDs, METHOD 1c = OLS: nonlinear - quadratic
lm(dem_voteshareCD1 ~ NameorderCentered_Clinton + NameorderCentered_ClintonSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016)
lm(rep_voteshareCD1 ~ NameorderCentered_Trump + NameorderCentered_TrumpSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016)
lm(lib_voteshareCD1 ~ NameorderCentered_Stein + NameorderCentered_SteinSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016)
lm(indK_voteshareCD1 ~ NameorderCentered_Stein + NameorderCentered_SteinSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016)
lm(indO_voteshareCD1 ~ NameorderCentered_Stein + NameorderCentered_SteinSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016)
lm(other_voteshareCD1 ~ NameorderCentered_Stein + NameorderCentered_SteinSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016)

lm(dem_voteshareCD2 ~ NameorderCentered_Clinton + NameorderCentered_ClintonSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   data = nh_2016)
lm(rep_voteshareCD2 ~ NameorderCentered_Trump + NameorderCentered_TrumpSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   data = nh_2016)
lm(ind_voteshareCD2 ~ NameorderCentered_Stein + NameorderCentered_SteinSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2,
   data = nh_2016)

# Now look at the second "randomization_inference" file
#RACE=Presidential, METHOD 1 = OLS: first versus later
ritest(
  lm(voteshare_clinton ~ Nameorder_Clinton_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK, 
     data = nh_2016),
  'Nameorder_Clinton_1', reps=1e4, seed=1234
)
ritest(
  lm(voteshare_trump ~ Nameorder_Trump_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK, 
     data = nh_2016),
  'Nameorder_Trump_1', reps=1e4, seed=1234
)
ritest(
  lm(voteshare_dlf ~ Nameorder_Stein_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK, 
     data = nh_2016),
  'Nameorder_Stein_1', reps=1e4, seed=1234
)
ritest(
  lm(voteshare_johnson ~ Nameorder_Stein_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK, 
     data = nh_2016),
  'Nameorder_Stein_1', reps=1e4, seed=1234
)
ritest(
  lm(voteshare_stein ~ Nameorder_Stein_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK, 
     data = nh_2016),
  'Nameorder_Stein_1', reps=1e4, seed=1234
)
ritest(
  lm(voteshare_other ~ Nameorder_Stein_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK, 
     data = nh_2016),
  'Nameorder_Stein_1', reps=1e4, seed=1234
)

#RACE=Senatorial, METHOD 1 = OLS: first versus later
ritest(
  lm(dem_voteshareSenate ~ Nameorder_Clinton_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber, 
     data = nh_2016),
  'Nameorder_Clinton_1', reps=1e4, seed=1234
)
ritest(
  lm(rep_voteshareSenate ~ Nameorder_Trump_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber, 
     data = nh_2016),
  'Nameorder_Trump_1', reps=1e4, seed=1234
)
ritest(
  lm(lib_voteshareGuber ~ Nameorder_Stein_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber, 
     data = nh_2016),
  'Nameorder_Stein_1', reps=1e4, seed=1234
)
ritest(
  lm(ind_voteshareGuber ~ Nameorder_Stein_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber, 
     data = nh_2016),
  'Nameorder_Stein_1', reps=1e4, seed=1234
)
ritest(
  lm(other_voteshareGuber ~ Nameorder_Stein_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber, 
     data = nh_2016),
  'Nameorder_Stein_1', reps=1e4, seed=1234
)

#RACE=Gubernatorial, METHOD 1 = OLS: first versus later
ritest(
  lm(dem_voteshareGuber ~ Nameorder_Clinton_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber, 
     data = nh_2016),
  'Nameorder_Clinton_1', reps=1e4, seed=1234
)
ritest(
  lm(rep_voteshareGuber ~ Nameorder_Trump_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber, 
     data = nh_2016),
  'Nameorder_Trump_1', reps=1e4, seed=1234
)
ritest(
  lm(lib_voteshareGuber ~ Nameorder_Stein_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKGuber, 
     data = nh_2016),
  'Nameorder_Stein_1', reps=1e4, seed=1234
)

#RACE=CDs, METHOD 1 = OLS: first versus later
ritest(
  lm(dem_voteshareCD1 ~ Nameorder_Clinton_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1, 
     data = nh_2016),
  'Nameorder_Clinton_1', reps=1e4, seed=1234
)
ritest(
  lm(rep_voteshareCD1 ~ Nameorder_Trump_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1, 
     data = nh_2016),
  'Nameorder_Trump_1', reps=1e4, seed=1234
)
ritest(
  lm(lib_voteshareCD1 ~ Nameorder_Stein_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1, 
     data = nh_2016),
  'Nameorder_Stein_1', reps=1e4, seed=1234
)
ritest(
  lm(indK_voteshareCD1 ~ Nameorder_Stein_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1, 
     data = nh_2016),
  'Nameorder_Stein_1', reps=1e4, seed=1234
)
ritest(
  lm(indO_voteshareCD1 ~ Nameorder_Stein_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1, 
     data = nh_2016),
  'Nameorder_Stein_1', reps=1e4, seed=1234
)
ritest(
  lm(other_voteshareCD1 ~ Nameorder_Stein_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1, 
     data = nh_2016),
  'Nameorder_Stein_1', reps=1e4, seed=1234
)

ritest(
  lm(dem_voteshareCD2 ~ Nameorder_Clinton_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2, 
     data = nh_2016),
  'Nameorder_Clinton_1', reps=1e4, seed=1234
)
ritest(
  lm(rep_voteshareCD2 ~ Nameorder_Trump_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2, 
     data = nh_2016),
  'Nameorder_Trump_1', reps=1e4, seed=1234
)
ritest(
  lm(ind_voteshareCD2 ~ Nameorder_Stein_1 + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD2, 
     data = nh_2016),
  'Nameorder_Stein_1', reps=1e4, seed=1234
)

# Look at the third "weights_FINAL4OSF" file

# Look at the fourth "alternativecoding_FINAL4OSF" file
#RACE=Presidential, METHOD 1 = OLS: first versus later
#RACE=Presidential, METHOD 1a = OLS: second versus first & third versus first
#RACE=Presidential, METHOD 1b = OLS: linear
lm(voteshare_clinton ~ demorderP_centered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016_2)
lm(voteshare_trump ~ reporderP_centered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016_2)
lm(voteshare_stein ~ steinorderP_centered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016_2)
lm(voteshare_dlf ~ dlforderP_centered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016_2)
lm(voteshare_johnson ~ johnsonorderP_centered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016_2)

#RACE=Presidential, METHOD 1c = OLS: nonlinear-quadratic
lm(voteshare_clinton ~ demorderP_centered + demorderP_centeredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016_2)
lm(voteshare_trump ~ reporderP_centered + reporderP_centeredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016_2)
lm(voteshare_stein ~ steinorderP_centered + steinorderP_centeredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016_2)
lm(voteshare_dlf ~ dlforderP_centered + dlforderP_centeredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016_2)
lm(voteshare_johnson ~ johnsonorderP_centered + johnsonorderP_centeredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastK,
   data = nh_2016_2)

#RACE=Senatorial, METHOD 1 = OLS: first versus later 
#RACE=Gubernatory, METHOD 1 = OLS: first versus later 
#RACE=Senatorial, METHOD 1a = OLS: second vs first & last vs first
#RACE=Gubernatory, METHOD 1a = OLS: first versus last
#RACE=Senatorial, METHOD 1b = OLS: linear
lm(dem_voteshareSenate ~ demorderS_centered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKSenate,
   data = nh_2016_2)
lm(rep_voteshareSenate ~ reporderS_centered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKSenate,
   data = nh_2016_2)
lm(lib_voteshareSenate ~ liborderS_centered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKSenate,
   data = nh_2016_2)
lm(ind_voteshareSenate ~ indorderS_centered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKSenate,
   data = nh_2016_2)

#RACE=Gubernatory, METHOD 1b = OLS: linear
#RACE=Senatorial, METHOD 1c = OLS: nonlinear - quadratic
lm(dem_voteshareSenate ~ demorderS_centered + demorderS_centeredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKSenate,
   data = nh_2016_2)

lm(rep_voteshareSenate ~ reporderS_centered + reporderS_centeredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKSenate,
   data = nh_2016_2)

lm(lib_voteshareSenate ~ liborderS_centered + liborderS_centeredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKSenate,
   data = nh_2016_2)

lm(ind_voteshareSenate ~ indorderS_centered + indorderS_centeredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKSenate,
   data = nh_2016_2)

#RACE=Gubernatory, METHOD 1c = OLS: nonlinear - quadratic

#RACE=CDs, METHOD 1 = OLS: first versus later ***/ 
#RACE=CDs, METHOD 1a = OLS: second vs first & last vs first ***/
#RACE=CDs, METHOD 1b = OLS: linear

lm(dem_voteshareCD1 ~ demorderCD1_centered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016_2)
lm(rep_voteshareCD1 ~ reporderCD1_centered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016_2)
lm(indO_voteshareCD1 ~ indOorderCD1_centered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016_2)
lm(indK_voteshareCD1 ~ indKorderCD1_centered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016_2)
lm(lib_voteshareCD1 ~ liborderCD1_centered + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016_2)

#RACE=CDs, METHOD 1c = OLS: nonlinear - quadratic
lm(dem_voteshareCD1 ~ demorderCD1_centered + demorderCD1_centeredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016_2)

lm(rep_voteshareCD1 ~ reporderCD1_centered + reporderCD1_centeredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016_2)

lm(indO_voteshareCD1 ~ indOorderCD1_centered + indOorderCD1_centeredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016_2)

lm(indK_voteshareCD1 ~ indKorderCD1_centered + indKorderCD1_centeredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016_2)

lm(lib_voteshareCD1 ~ liborderCD1_centered + liborderCD1_centeredSQ + Democrat_reg_percent + Republican_reg_percent + TotalvotescastKCD1,
   data = nh_2016_2)
