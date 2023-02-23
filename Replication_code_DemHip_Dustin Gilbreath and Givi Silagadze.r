### ------------------------------- data analysis for Democratic hypocricy brief --------------------------------------------------------------------------
rm(list = ls())
setwd("C:/Users/givi/Desktop/Givi/Policy brief - Democratic hypocrisy")
library(haven)
library(descr)
library(tidyverse)
library(ggeffects)
library(survey)
library(MASS)
library(foreign)
library(Matching)
library(rgenoud)
library(psych)
library(standardize)
library(nnet)
library(emmeans)
library(ggplot2)
library(plyr)
demhip<-read_dta("NED_2021_PostElections_29.11.21.dta") # importing dataset
names(demhip)
demhip[demhip==-9]<-NA


demhip$normalized <- (demhip$indwt/((sum(demhip$indwt)/length(demhip$indwt)))) # normalized weights


freq(as_factor(demhip$q14_group))   # intervention

freq(as_factor(demhip$q5)) # which party did u vote for in the 2021 municipal elections?
freq(as_factor(demhip$q21)) # which party is closest to you?
freq(as_factor(demhip$q20)) # Which TV channel do you trust the most?

demhip$q5_r<-demhip$q5
demhip$q5_r[demhip$q5_r<=-1]<--1
demhip$q5_r[demhip$q5_r==1]<-0
demhip$q5_r[demhip$q5_r>=1]<-1
table(demhip$q5_r)
crosstab(demhip$q5_r, demhip$q6)
demhip$q6_r<-demhip$q6
demhip$q6_r[demhip$q6_r==1]<-0
demhip$q6_r[demhip$q6_r==2]<-1
demhip$party<-ifelse(demhip$q6==-7, demhip$q5_r, demhip$q6_r)

table(demhip$party)
table(demhip$party, demhip$q21)
freq(as_factor(demhip$q21))
demhip$q21_r<-demhip$q21
demhip$q21_r[demhip$q21_r<=-1]<--1
demhip$q21_r[demhip$q21_r==1]<-0
demhip$q21_r[demhip$q21_r>=1]<-1
table(demhip$party)
demhip$party_2<-ifelse(demhip$party<=-1, demhip$q21_r, demhip$party)
table(demhip$party_2)
freq(demhip$party_2, demhip$indwt)
table(demhip$party, demhip$q20)
table(demhip$q20)
demhip$q20_r<-demhip$q20               # TV trust
demhip$q20_r[demhip$q20_r<=-1]<--1
demhip$q20_r[demhip$q20_r==14]<--1
demhip$q20_r[demhip$q20_r==15]<--1
demhip$q20_r[demhip$q20_r==9]<--1
demhip$q20_r[demhip$q20_r==11]<--1
demhip$q20_r[demhip$q20_r==1]<-0
demhip$q20_r[demhip$q20_r==13]<-0
demhip$q20_r[demhip$q20_r==5]<-0
demhip$q20_r[demhip$q20_r==6]<-0
demhip$q20_r[demhip$q20_r==7]<-0
demhip$q20_r[demhip$q20_r==8]<-0
demhip$q20_r[demhip$q20_r==2]<-1
demhip$q20_r[demhip$q20_r==3]<-1
demhip$q20_r[demhip$q20_r==12]<-1
demhip$q20_r[demhip$q20_r==4]<-1
demhip$q20_r[demhip$q20_r==10]<-1

table(as_factor(demhip$q20_oth))
table(demhip$q20_r)


demhip$givi_party <- demhip$party_2
table(demhip$givi_party)
demhip$givi_party[demhip$q20_r == 1] <- 1 
demhip$givi_party[demhip$q20_r == 0] <- 0 
table(demhip$givi_party)
table(demhip$party_2)
freq(demhip$party_2, demhip$indwt)
freq(demhip$givi_party, demhip$indwt)
crosstab(demhip$party_2, demhip$q14_group, demhip$indwt, prop.r = T, chisq = T)
demhip$normalized <- (demhip$indwt/((sum(demhip$indwt)/length(demhip$indwt)))) # normalized weights
crosstab(demhip$party_2, demhip$q14_group, demhip$normalized, prop.r = T, chisq = T)

crosstab(demhip$party_2, demhip$q14_1, demhip$indwt, prop.r = T, chisq = T)
crosstab(demhip$party_2, demhip$q14_2, demhip$indwt, prop.r = T, chisq = T)
crosstab(demhip$party_2, demhip$q14_3, demhip$indwt, prop.r = T, chisq = T)
crosstab(demhip$party_2, demhip$q14_4, demhip$indwt, prop.r = T, chisq = T)
crosstab(demhip$party_2, demhip$q14_5, demhip$indwt, prop.r = T, chisq = T)
crosstab(demhip$party_2, demhip$q14_6, demhip$indwt, prop.r = T, chisq = T)
crosstab(demhip$party_2, demhip$q14_7, demhip$indwt, prop.r = T, chisq = T)
crosstab(demhip$party_2, demhip$q14_8, demhip$indwt, prop.r = T, chisq = T)
crosstab(demhip$party_2, demhip$q14_9, demhip$indwt, prop.r = T, chisq = T)
crosstab(demhip$party_2, demhip$q14_10, demhip$indwt, prop.r = T, chisq = T)



#df <- data.frame(matrix(ncol = 5, nrow = 10))
#colnames(df) <- c("RA", "DK", "No support", "Support", "total")
#rownames(df) <- c("Initiate investigations of the financing of opposition media",	
#                  "Initiate constitutional changes without consulting the opposition",	
#                  "All leadership positions in the parliament will be filled by the winning party",	
#                  "Initiate investigations of the sources of the financial backing of the losing party",	
#                  "Restrictions on protesting election results",
#                  "Initiate election reforms without consulting the opposition",	
#                  "Expand surveillance operations on political opponents",	"Investigate NGO funding",
#                  "Remove supporters of the opposition from government jobs",	
#                  "Restrictions on the use of exit polls in forthcoming elections")
#freq(demhip$q14_1, demhip$indwt)
#freq(demhip$q14_2, demhip$indwt)
#freq(demhip$q14_3, demhip$indwt)
#freq(demhip$q14_4, demhip$indwt)
#freq(demhip$q14_5, demhip$indwt)
#freq(demhip$q14_6, demhip$indwt)
#freq(demhip$q14_7, demhip$indwt)
#freq(demhip$q14_8, demhip$indwt)
#freq(demhip$q14_9, demhip$indwt)
#freq(demhip$q14_10, demhip$indwt)
#
#df[1,] <- freq(demhip$q14_1, demhip$indwt)[,2]
#df[2,] <- freq(demhip$q14_2, demhip$indwt)[,2]
#df[3,] <- freq(demhip$q14_3, demhip$indwt)[,2]
#df[4,] <- freq(demhip$q14_4, demhip$indwt)[,2]
#df[5,] <- freq(demhip$q14_5, demhip$indwt)[,2]
#df[6,] <- freq(demhip$q14_6, demhip$indwt)[,2]
#df[7,] <- freq(demhip$q14_7, demhip$indwt)[,2]
#df[8,] <- freq(demhip$q14_8, demhip$indwt)[,2]
#df[9,] <- freq(demhip$q14_9, demhip$indwt)[,2]
#df[10,] <- freq(demhip$q14_10, demhip$indwt)[,2]
#writexl::write_xlsx(df, "crosstab_support_for_nd_actions.xlsx")

crosstab(as_factor(demhip$q14_group), demhip$party_2,  prop.c = TRUE)

demhip$party_2_r<-(demhip$party_2+2)
demhip$party_2_r<-(demhip$party_2_r*10)
freq(demhip$party_2_r)
demhip$treat_group_party<-(demhip$q14_group+demhip$party_2_r)
freq(demhip$treat_group_party)
demhip$q14_1_r<-demhip$q14_1
demhip$q14_1_r[demhip$q14_1_r<=-1]<-NA
freq(demhip$q14_1)
table(demhip$indwt)
options(max.print = 1500)


## 
demhip$givi_party_r<-(demhip$givi_party+2)
demhip$givi_party_r<-(demhip$givi_party_r*10)
freq(demhip$givi_party_r)
demhip$treat_group_party_givi<-(demhip$q14_group+demhip$givi_party_r)
freq(demhip$treat_group_party_givi)
##


demhip<-demhip[complete.cases(demhip$indwt),]
freq(as_factor(demhip$stratum))

#### -------------socio-demographic variables
#age 
table(demhip$age)
demhip$age_group<-demhip$age
demhip$age_group[demhip$age_group<=34]<-1
demhip$age_group[demhip$age_group>=55]<-3
demhip$age_group[demhip$age_group>=35]<-2

# left bank vs. right bank
table(as_factor(demhip$stratum))
demhip$district <- demhip$stratum
table(demhip$district)
demhip$district[demhip$district<=-1]<-NA
demhip$district[demhip$district <= 4 | demhip$district == 7 | demhip$district == 9] <- 21  # left bank 
demhip$district[demhip$district != 21] <- 20  # right bank 
demhip$district <- demhip$district - 20 
table(demhip$district)
freq(demhip$district, demhip$indwt)



# ethnicity
table(as_factor(demhip$q19))
demhip$ethnic<-demhip$q19
demhip$ethnic[demhip$ethnic<=-1]<-NA
demhip$ethnic[demhip$ethnic!=3]<-1
demhip$ethnic[demhip$ethnic==3]<-0
table(demhip$ethnic)

# idp status
freq(demhip$q18)
demhip$idp<-demhip$q18
demhip$idp[demhip$idp<=-1]<-NA
table(demhip$idp)


# wealth
demhip$q17_1_r<-demhip$q17_1
demhip$q17_1_r[demhip$q17_1_r<=-1]<-NA
demhip$q17_2_r<-demhip$q17_2
demhip$q17_2_r[demhip$q17_2_r<=-1]<-NA
demhip$q17_3_r<-demhip$q17_3
demhip$q17_3_r[demhip$q17_3_r<=-1]<-NA
demhip$q17_4_r<-demhip$q17_4
demhip$q17_4_r[demhip$q17_4_r<=-1]<-NA
demhip$q17_5_r<-demhip$q17_5
demhip$q17_5_r[demhip$q17_5_r<=-1]<-NA
demhip$q17_6_r<-demhip$q17_6
demhip$q17_6_r[demhip$q17_6_r<=-1]<-NA
demhip$q17_7_r<-demhip$q17_7
demhip$q17_7_r[demhip$q17_7_r<=-1]<-NA
demhip$q17_8_r<-demhip$q17_8
demhip$q17_8_r[demhip$q17_8_r<=-1]<-NA
demhip$q17_9_r<-demhip$q17_9
demhip$q17_9_r[demhip$q17_9_r<=-1]<-NA
demhip$q17_10_r<-demhip$q17_10
demhip$q17_10_r[demhip$q17_10_r<=-1]<-NA
demhip$wealth<-(demhip$q17_1_r+
                  demhip$q17_2_r+
                  demhip$q17_3_r+
                  demhip$q17_4_r+
                  demhip$q17_5_r+
                  demhip$q17_6_r+
                  demhip$q17_7_r+
                  demhip$q17_8_r+
                  demhip$q17_9_r+
                  demhip$q17_10_r)
hist(demhip$wealth)

# education
freq(as_factor(demhip$q16))
demhip$edu<-demhip$q16
demhip$edu[demhip$edu<=-1]<-NA
demhip$edu[demhip$edu<=5]<-0
demhip$edu[demhip$edu>=6]<-1
table(demhip$edu)

crosstab(as_factor(demhip$edu), as_factor(demhip$district), demhip$indwt, prop.c = TRUE, chisq = T)

# employment
freq(as_factor(demhip$q15))
demhip$emp<-demhip$q15
demhip$emp[demhip$emp<=-1]<-NA
demhip$emp[demhip$emp<=4]<-0
demhip$emp[demhip$emp>=7]<-0
demhip$emp[demhip$emp>=5]<-1
table(demhip$emp)

table(demhip$treat_group_party)
#1 is non partisans
#2 is party they want takes power
#3 is party they don't want takes power
demhip$treatment<-demhip$treat_group_party
demhip$treatment[demhip$treatment<=12]<-1
demhip$treatment[demhip$treatment==21]<-2
demhip$treatment[demhip$treatment==32]<-2
demhip$treatment[demhip$treatment==32]<-2
demhip$treatment[demhip$treatment>=22]<-3


demhip$treatment[demhip$treatment==3]<-0
table(demhip$treatment, demhip$party)
demhip$treatment<-as_factor(demhip$treatment)
freq(demhip$treatment)




demhip$tr_givi<-demhip$treat_group_party_givi
demhip$q14_group
demhip$tr_givi[demhip$tr_givi<=12]<-1  # 1 - non affiliated
demhip$tr_givi[demhip$tr_givi==21]<-2  # 2 - preferred party wins
demhip$tr_givi[demhip$tr_givi==32]<-2  
demhip$tr_givi[demhip$tr_givi==32]<-2
demhip$tr_givi[demhip$tr_givi>=22]<-3
demhip$tr_givi[demhip$tr_givi == 3] <- 0 # 0 - preferred party loses
table(demhip$tr_givi, demhip$party)
demhip$tr_givi<-as_factor(demhip$tr_givi)
freq(demhip$tr_givi)
freq(demhip$tr_givi, demhip$indwt)  ##### 43% preferred party lost, 34% preferred party won, 23% not affiliated












# 
table(demhip$q14_1_r)
demhip$treatment <- as.factor(demhip$treatment)
demhip$tr_givi <- as.factor(demhip$tr_givi)
demhip_svy<-svydesign(~psu, strata = demhip$stratum, weights = demhip$indwt, data = demhip)
table(demhip$party_2_r)
model1<-svyglm(q14_1_r~treatment + psu_group, design = demhip_svy,
               family = "binomial")
summary(model1)
ggemmeans(model1, c("treatment"))
ggeffect(model1, c("treatment"))
ggpredict(model1, c("treatment"))
names(model1)
length(model1$fitted.values)
length(model1$residuals)
length(model1$coefficients)
length(demhip$q14_1_r)
length(demhip$treatment)
length(demhip$psu_group)
## 
model1_givi<-svyglm(q14_1_r~tr_givi + psu_group, design = demhip_svy,
                    family = "binomial")
summary(model1_givi)
ggemmeans(model1_givi, c("tr_givi"))
table(demhip$treatment)
table(demhip$tr_givi)
freq(demhip$tr_givi, demhip$indwt)
library("margins")
margins(model1_givi, design = demhip_svy) -> margins_model1_givi
summary(margins_model1_givi)
## 



freq(as_factor(demhip$party_2_r))
model1_party<-svyglm(q14_1_r~as_factor(treatment)*as_factor(party_2_r) + psu_group, design = demhip_svy,
                     family = "binomial")
summary(model1_party)
ggemmeans(model1_party, c( "party_2_r","treatment"))
plot(ggemmeans(model1_party, c( "party_2_r","treatment")))

#install.packages("marginaleffects")
library(marginaleffects)
ame_m1<-marginaleffects(model1)
summary(ame_m1)

plot_cme(model1, effect = "treatment", condition = c("party_2"))

model1<-svyglm(q14_1_r~as_factor(treat_group_party) + psu_group, design = demhip_svy,
               family = "binomial")
summary(model1)

plot(ggemmeans(model1, "treat_group_party"))

model1_emp<-svyglm(q14_1_r~as_factor(treat_group_party)*as_factor(emp), design = demhip_svy,
                   family = "binomial")

summary(model1_emp)

model1_edu<-svyglm(q14_1_r~as_factor(treat_group_party)*as_factor(edu), design = demhip_svy,
                   family = "binomial")

summary(model1_edu)

plot(ggemmeans(model1_edu, c("treat_group_party", "edu")))


model1_wealth<-svyglm(q14_1_r~as_factor(treat_group_party)*wealth, design = demhip_svy,
                      family = "binomial")

summary(model1_wealth)
plot(ggemmeans(model1_wealth, c("treat_group_party", "wealth")))
model1_idp<-svyglm(q14_1_r~as_factor(treat_group_party)*as_factor(idp), design = demhip_svy,
                   family = "binomial")

summary(model1_idp)

plot(ggemmeans(model1_idp, c("treat_group_party", "idp")))

model1_ethnic<-svyglm(q14_1_r~as_factor(treat_group_party)*as_factor(ethnic), design = demhip_svy,
                      family = "binomial")

summary(model1_ethnic)

plot(ggemmeans(model1_ethnic, c("treat_group_party", "ethnic")))
table(demhip$ethnic, demhip$treat_group_party)
table(demhip$idp, demhip$treat_group_party)

model1_sex<-svyglm(q14_1_r~as_factor(treat_group_party)*as_factor(sex), design = demhip_svy,
                   family = "binomial")

summary(model1_sex)

plot(ggemmeans(model1_sex, c("treat_group_party", "sex")))


model1_district<-svyglm(q14_1_r~as_factor(treat_group_party)*as_factor(district), design = demhip_svy,
                        family = "binomial")

summary(model1_district)

plot(ggemmeans(model1_district, c("treat_group_party", "district")))
table(demhip$district, demhip$treat_group_party)


model1_age_group<-svyglm(q14_1_r~as_factor(treat_group_party)*as_factor(age_group), design = demhip_svy,
                         family = "binomial")

summary(model1_age_group)

plot(ggemmeans(model1_age_group, c("treat_group_party", "age_group")))

demhip$q14_2_r<-demhip$q14_2
demhip$q14_2_r[demhip$q14_2_r<=-1]<-NA
demhip_svy<-svydesign(~psu, strata = demhip$stratum, weights = demhip$indwt, data = demhip)
freq(demhip$q14_2)
model2<-svyglm(q14_2_r~treatment+ psu_group, design = demhip_svy,
               family = "binomial")
summary(model2)
plot(ggemmeans(model2, "treatment"))
ggemmeans(model2, "treatment")

# 
model2_givi<-svyglm(q14_2_r~tr_givi+psu_group, design = demhip_svy,
                    family = "binomial")
summary(model2_givi)
demhip$q14_2
plot(ggemmeans(model2_givi, "tr_givi"))
ggemmeans(model2_givi, "tr_givi")
margins(model2_givi, design = demhip_svy) -> margins_model2_givi
summary(margins_model2_givi)
## 

freq(demhip$q14_2)
model2_emp<-svyglm(q14_2_r~as_factor(treat_group_party)*as_factor(emp), design = demhip_svy,
                   family = "binomial")

summary(model2_emp)

model2_edu<-svyglm(q14_2_r~as_factor(treat_group_party)*as_factor(edu), design = demhip_svy,
                   family = "binomial")

summary(model2_edu)

plot(ggemmeans(model2_edu, c("treat_group_party", "edu")))


model2_wealth<-svyglm(q14_2_r~as_factor(treat_group_party)*wealth, design = demhip_svy,
                      family = "binomial")

summary(model2_wealth)
plot(ggemmeans(model2_wealth, c("treat_group_party", "wealth")))
model2_idp<-svyglm(q14_2_r~as_factor(treat_group_party)*as_factor(idp), design = demhip_svy,
                   family = "binomial")

summary(model2_idp)

plot(ggemmeans(model2_idp, c("treat_group_party", "idp")))

model2_ethnic<-svyglm(q14_2_r~as_factor(treat_group_party)*as_factor(ethnic), design = demhip_svy,
                      family = "binomial")

summary(model2_ethnic)

plot(ggemmeans(model2_ethnic, c("treat_group_party", "ethnic")))
table(demhip$ethnic, demhip$treat_group_party)
table(demhip$idp, demhip$treat_group_party)

model2_sex<-svyglm(q14_2_r~as_factor(treat_group_party)*as_factor(sex), design = demhip_svy,
                   family = "binomial")

summary(model2_sex)

plot(ggemmeans(model2_sex, c("treat_group_party", "sex")))


model2_district<-svyglm(q14_2_r~as_factor(treat_group_party)*as_factor(district), design = demhip_svy,
                        family = "binomial")

summary(model2_district)

plot(ggemmeans(model2_district, c("treat_group_party", "district")))
table(demhip$district, demhip$treat_group_party)


model2_age_group<-svyglm(q14_2_r~as_factor(treat_group_party)*as_factor(age_group), design = demhip_svy,
                         family = "binomial")

summary(model2_age_group)

plot(ggemmeans(model2_age_group, c("treat_group_party", "age_group")))


demhip$q14_3_r<-demhip$q14_3
demhip$q14_3_r[demhip$q14_3_r<=-1]<-NA
freq(demhip$q14_3_r)
demhip_svy<-svydesign(~psu, strata = demhip$stratum, weights = demhip$indwt, data = demhip)

model_3<-svyglm(q14_3_r~treatment + psu_group, design = demhip_svy,
                family = "binomial")
summary(model_3)
plot(ggemmeans(model_3, "treatment"))
ggemmeans(model_3, "treatment")


## 
model_3_givi<-svyglm(q14_3_r~tr_givi + psu_group, design = demhip_svy,
                     family = "binomial")
summary(model_3_givi)
plot(ggemmeans(model_3_givi, "tr_givi"))
ggemmeans(model_3_givi, "tr_givi")
margins(model_3_givi, design = demhip_svy) -> margins_model_3_givi
summary(margins_model_3_givi)
## 

plot(ggemmeans(model_3, "psu_group"))
model3_emp<-svyglm(q14_3_r~as_factor(treat_group_party)*as_factor(emp), design = demhip_svy,
                   family = "binomial")

summary(model3_emp)

model3_edu<-svyglm(q14_3_r~as_factor(treat_group_party)*as_factor(edu), design = demhip_svy,
                   family = "binomial")

summary(model3_edu)

plot(ggemmeans(model3_edu, c("treat_group_party", "edu")))


model3_wealth<-svyglm(q14_3_r~as_factor(treat_group_party)*wealth, design = demhip_svy,
                      family = "binomial")

summary(model3_wealth)
plot(ggemmeans(model3_wealth, c("treat_group_party", "wealth")))
model3_idp<-svyglm(q14_3_r~as_factor(treat_group_party)*as_factor(idp), design = demhip_svy,
                   family = "binomial")

summary(model3_idp)

plot(ggemmeans(model3_idp, c("treat_group_party", "idp")))

model3_ethnic<-svyglm(q14_3_r~as_factor(treat_group_party)*as_factor(ethnic), design = demhip_svy,
                      family = "binomial")

summary(model3_ethnic)

plot(ggemmeans(model3_ethnic, c("treat_group_party", "ethnic")))
table(demhip$ethnic, demhip$treat_group_party)
table(demhip$idp, demhip$treat_group_party)

model3_sex<-svyglm(q14_3_r~as_factor(treat_group_party)*as_factor(sex), design = demhip_svy,
                   family = "binomial")

summary(model3_sex)

plot(ggemmeans(model3_sex, c("treat_group_party", "sex")))


model3_district<-svyglm(q14_3_r~as_factor(treat_group_party)*as_factor(district), design = demhip_svy,
                        family = "binomial")

summary(model3_district)

plot(ggemmeans(model3_district, c("treat_group_party", "district")))
table(demhip$district, demhip$treat_group_party)


model3_age_group<-svyglm(q14_3_r~as_factor(treat_group_party)*as_factor(age_group), design = demhip_svy,
                         family = "binomial")

summary(model3_age_group)

plot(ggemmeans(model3_age_group, c("treat_group_party", "age_group")))

demhip$q14_4_r<-demhip$q14_4
demhip$q14_4_r[demhip$q14_4_r<=-1]<-NA
demhip_svy<-svydesign(~psu, strata = demhip$stratum, weights = demhip$indwt, data = demhip)
model_4<-svyglm(q14_4_r~treatment+ psu_group, design = demhip_svy, family = "binomial")
summary(model_4)
plot(ggemmeans(model_4, "treatment"))
ggemmeans(model_4,"treatment")
freq(demhip$q14_4)

## 
model_4_givi<-svyglm(q14_4_r~tr_givi+ psu_group, design = demhip_svy, family = "binomial")
summary(model_4_givi)
ggemmeans(model_4_givi, "tr_givi")
margins(model_4_givi, design = demhip_svy) -> margins_model_4_givi
summary(margins_model_4_givi)
# 

demhip$q14_5_r<-demhip$q14_5
demhip$q14_5_r[demhip$q14_5_r<=-1]<-NA
demhip$q14_6_r<-demhip$q14_6
demhip$q14_6_r[demhip$q14_6_r<=-1]<-NA
demhip$q14_7_r<-demhip$q14_7
demhip$q14_7_r[demhip$q14_7_r<=-1]<-NA
demhip$q14_8_r<-demhip$q14_8
demhip$q14_8_r[demhip$q14_8_r<=-1]<-NA
demhip$q14_9_r<-demhip$q14_9
demhip$q14_9_r[demhip$q14_9_r<=-1]<-NA
demhip$q14_10_r<-demhip$q14_10
demhip$q14_10_r[demhip$q14_10_r<=-1]<-NA
demhip_svy<-svydesign(~psu, strata = demhip$stratum, weights = demhip$indwt, data = demhip)
model_5<-svyglm(q14_5_r~treatment+ psu_group, design = demhip_svy, family = "binomial")
summary(model_5)
plot(ggemmeans(model_5, "treatment"))
ggemmeans(model_5, "treatment")

## 
model_5_givi<-svyglm(q14_5_r~tr_givi+ psu_group, design = demhip_svy, family = "binomial")
summary(model_5_givi)
plot(ggemmeans(model_5_givi, "tr_givi"))
ggemmeans(model_5_givi, "tr_givi")
margins(model_5_givi, design = demhip_svy) -> margins_model_5_givi
summary(margins_model_5_givi)
## 

model_6<-svyglm(q14_6_r~treatment+ psu_group, design = demhip_svy, family = "binomial")
summary(model_6)
plot(ggemmeans(model_6, "treatment"))
ggemmeans(model_6, "treatment")
ggemmeans(model_6, "psu_group")
table(demhip$psu_group)
# interaction 
model_6_int<-svyglm(q14_6_r~treatment*psu_group, design = demhip_svy, family = "binomial")
summary(model_6_int) # not significant

## 
model_6_givi<-svyglm(q14_6_r~tr_givi+ psu_group, design = demhip_svy, family = "binomial")
summary(model_6_givi)
plot(ggemmeans(model_6_givi, "tr_givi"))
ggemmeans(model_6_givi, "tr_givi")
ggpredict(model_6_givi, "tr_givi")
ggemmeans(model_6_givi, "psu_group")

margins(model_6_givi, design = demhip_svy) -> margins_model_6_givi
summary(margins_model_6_givi)
## 


model_7<-svyglm(q14_7_r~treatment+ psu_group, design = demhip_svy, family = "binomial")
summary(model_7)
plot(ggemmeans(model_7, "treatment"))
ggemmeans(model_7, "treatment")
## 
model_7_givi<-svyglm(q14_7_r~tr_givi+ psu_group, design = demhip_svy, family = "binomial")
summary(model_7_givi)
ggemmeans(model_7_givi, "tr_givi")
margins(model_7_givi, design = demhip_svy) -> margins_model_7_givi
summary(margins_model_7_givi)
## 

model_8<-svyglm(q14_8_r~treatment+ psu_group, design = demhip_svy, family = "binomial")
summary(model_8)
plot(ggemmeans(model_8, "treatment"))
ggemmeans(model_8, "treatment")

## 
model_8_givi<-svyglm(q14_8_r~tr_givi+ psu_group, design = demhip_svy, family = "binomial")
summary(model_8_givi)
ggemmeans(model_8_givi, "tr_givi")
margins(model_8_givi, design = demhip_svy) -> margins_model_8_givi
summary(margins_model_8_givi)
##

model_9<-svyglm(q14_9_r~treatment+ psu_group, design = demhip_svy, family = "binomial")
summary(model_9)
plot(ggemmeans(model_9, "treatment"))
ggemmeans(model_9, "treatment")
## 
model_9_givi<-svyglm(q14_9_r~tr_givi+ psu_group, design = demhip_svy, family = "binomial")
summary(model_9_givi)
ggemmeans(model_9_givi, "tr_givi") 
margins(model_9_givi, design = demhip_svy) -> margins_model_9_givi
summary(margins_model_9_givi)
##  

model_10<-svyglm(q14_10_r~treatment+ psu_group, design = demhip_svy, family = "binomial")
summary(model_10)
plot(ggemmeans(model_10, "treatment"))
ggemmeans(model_10, "treatment")
## 
model_10_givi<-svyglm(q14_10_r~tr_givi+ psu_group, design = demhip_svy, family = "binomial")
summary(model_10_givi)
ggemmeans(model_10_givi, "tr_givi")
#install.packages("margins")
library("margins")
margins(model_10_givi, design = demhip_svy) -> margins_model_10_givi
summary(margins_model_10_givi)
## 


demhip$index<-(demhip$q14_1_r+
                 demhip$q14_2_r+
                 demhip$q14_3_r+
                 demhip$q14_4_r+
                 demhip$q14_5_r+
                 demhip$q14_6_r+
                 demhip$q14_7_r+
                 demhip$q14_8_r+
                 demhip$q14_9_r+
                 demhip$q14_10_r)
summary(demhip$index)
class(demhip$index)
hist(demhip$index)

demhip_svy<-svydesign(~psu, strata = demhip$stratum, weights = demhip$indwt, data = demhip)
demhip$treatment <- as.factor(demhip$treatment)

model_index<-svyglm(index~treatment+ psu_group, design = demhip_svy, family = "poisson")
summary(model_index)
plot(ggemmeans(model_index, "treatment"))
ggemmeans(model_index, "treatment")
ggpredict(model_index, "treatment")


demhip$tr_givi <- as.factor(demhip$tr_givi)
model_index_givi<-svyglm(index~tr_givi+ psu_group, design = demhip_svy, family = "poisson")
summary(model_index_givi)
plot(ggemmeans(model_index_givi, "tr_givi"))
ggemmeans(model_index_givi, "tr_givi")                    ##### Figure 3. Predicted scores on the index
freq(demhip$tr_givi, demhip$indwt)
svymean(demhip$index, demhip_svy, na.rm=T,deff=FALSE)


## ------ Additonal analysis based on reviwers' comments ---------------------- START ----------------------------
demhip$index_no_NGOs<-(demhip$q14_2_r+           # without NGOs
                         demhip$q14_3_r+
                         demhip$q14_4_r+
                         demhip$q14_5_r+
                         demhip$q14_6_r+
                         demhip$q14_7_r+
                         demhip$q14_8_r+
                         demhip$q14_9_r+
                         demhip$q14_10_r)
summary(demhip$index_no_NGOs)
demhip_svy<-svydesign(~psu, strata = demhip$stratum, weights = demhip$indwt, data = demhip)
model_index_no_NGOs_givi<-svyglm(index_no_NGOs~tr_givi+ psu_group, design = demhip_svy, family = "poisson")
summary(model_index_no_NGOs_givi)                       # still statistically significant
plot(ggemmeans(model_index_no_NGOs_givi, "tr_givi"))
ggemmeans(model_index_no_NGOs_givi, "tr_givi")             # co-partisants in power 1.6; co-partisans out of power 1

demhip$index_no_media<-(demhip$q14_1_r+           # without Media
                          demhip$q14_3_r+
                          demhip$q14_4_r+
                          demhip$q14_5_r+
                          demhip$q14_6_r+
                          demhip$q14_7_r+
                          demhip$q14_8_r+
                          demhip$q14_9_r+
                          demhip$q14_10_r)
summary(demhip$index_no_media)
demhip_svy<-svydesign(~psu, strata = demhip$stratum, weights = demhip$indwt, data = demhip)
model_index_no_media_givi<-svyglm(index_no_media~tr_givi+ psu_group, design = demhip_svy, family = "poisson")
summary(model_index_no_media_givi)                       # still statistically significant
plot(ggemmeans(model_index_no_media_givi, "tr_givi"))
ggemmeans(model_index_no_media_givi, "tr_givi") 

## ------ Additonal analysis based on reviewers' comments ---------------------- END ----------------------------
model_index<-svyglm(index~as_factor(treatment)+ psu_group, design = demhip_svy, family = "poisson")
summary(model_index)


model_index_emp<-svyglm(index~as_factor(treatment)*as_factor(emp)+ psu_group, design = demhip_svy, family = "poisson")
summary(model_index_emp)
ggemmeans(model_index_emp, terms = c("treatment", "emp"))
## 
demhip$emp_new <- demhip$emp
demhip$emp_new <- as.factor(demhip$emp_new)
table(demhip$emp_new)
demhip_svy_new<-svydesign(~psu, strata = demhip$stratum, weights = demhip$indwt, data = demhip)
model_index_emp_givi<-svyglm(index~tr_givi*emp_new+ psu_group, 
                             design = demhip_svy_new, 
                             family = "poisson")
summary(model_index_emp_givi)
ggemmeans(model_index_emp_givi, terms = c("tr_givi", "emp_new"))
## 



model_index_edu<-svyglm(index~as_factor(treatment)*as_factor(edu)+ psu_group, design = demhip_svy, family = "poisson")
summary(model_index_edu)
ggemmeans(model_index_edu, terms = c("treatment", "edu"))

model_index_wealth<-svyglm(index~as_factor(treatment)*wealth+ psu_group, design = demhip_svy, family = "poisson")
summary(model_index_wealth)
plot(ggemmeans(model_index_wealth, terms = c("treatment", "wealth")))

model_index_idp<-svyglm(index~as_factor(treatment)*as_factor(idp)+ psu_group, design = demhip_svy, family = "poisson")
summary(model_index_idp)
ggemmeans(model_index_idp, terms = c("treatment", "idp"))

model_index_ethnic<-svyglm(index~as_factor(treatment)*as_factor(ethnic)+ psu_group, design = demhip_svy, family = "poisson")
summary(model_index_ethnic)
ggemmeans(model_index_ethnic, terms = c("treatment", "ethnic"))


model_index_sex<-svyglm(index~as_factor(treatment)*as_factor(sex)+ psu_group, design = demhip_svy, family = "poisson")
summary(model_index_sex)
ggemmeans(model_index_sex, terms = c("treatment", "sex"))
model_index_district<-svyglm(index~as_factor(treatment)*as_factor(district)+ psu_group, design = demhip_svy, family = "poisson")
summary(model_index_district)
ggemmeans(model_index_district, terms = c("treatment", "district"))

model_index_age_group<-svyglm(index~as_factor(treatment)*as_factor(age_group)+ psu_group, design = demhip_svy, family = "poisson")
summary(model_index_age_group)
ggemmeans(model_index_age_group, terms = c("treatment", "age_group"))

model_index_party_2_r<-svyglm(index~as_factor(treatment)*as_factor(party_2_r)+ psu_group, design = demhip_svy, family = "poisson")
summary(model_index_party_2_r)
ggemmeans(model_index_party_2_r, terms = c("treatment", "party_2_r"))
## 
table(demhip$party_2_r)
table(demhip$party_2)
table(demhip$party_reversed)
table(demhip$givi_party)
table(demhip$givi_party_r)
demhip$party_to_test <- demhip$party_reversed
demhip$emp_new <- demhip$emp
demhip$party_to_test <- as.factor(demhip$party_to_test)
demhip_svy_new<-svydesign(~psu, strata = demhip$stratum, weights = demhip$indwt, data = demhip)
model_index_party_givi<-svyglm(index~tr_givi+party_to_test+psu_group, 
                               design = demhip_svy_new, 
                               family = "poisson")
summary(model_index_party_givi)
ggemmeans(model_index_party_givi, "party_to_test")
ggemmeans(model_index_emp_givi, terms = c("tr_givi", "emp_new"))
## 

svymean(~index, design = demhip_svy, na.rm = TRUE)
hist(demhip$index)

class(demhip$edu)
table(demhip$emp)

demhip$party_2_r <- as.factor(demhip$party_2_r)
demhip$emp <- as.factor(demhip$emp)
demhip$edu <- as.factor(demhip$edu)
demhip$idp <- as.factor(demhip$idp)
demhip$ethnic <- as.factor(demhip$ethnic)
demhip$sex <- as.factor(demhip$sex)
demhip$district <- as.factor(demhip$district)
demhip$age_group <- as.factor(demhip$age_group)
demhip_svy<-svydesign(~psu, strata = demhip$stratum, weights = demhip$indwt, data = demhip)
model_index_predict<-svyglm(index~
                              party_2_r+emp+
                              edu+
                              wealth+
                              ethnic+
                              idp+
                              sex+
                              district+
                              age_group+
                              psu_group, design = demhip_svy)

summary(model_index_predict)
table(demhip$party_2_r)
table(demhip$givi_party_r)
ggemmeans(model_index_predict, "district")
ggpredict(model_index_predict, "district")
ggemmeans(model_index_predict, "party_2_r")
ggemmeans(model_index_predict, "party_2_r")
as_factor(district)
as_factor(age_group)
as_factor(party_2_r)

### ---------------start-------- GIVI --------------------------------------------------------------------------------------------------------------------------
demhip$givi_party_r <- as.factor(demhip$givi_party_r)
table(demhip$givi_party_r)
demhip_svy<-svydesign(~psu, strata = demhip$stratum, weights = demhip$indwt, data = demhip)
model_index_predict_givi<-svyglm(index~
                                   givi_party_r+emp+
                                   edu+
                                   wealth+
                                   idp+
                                   sex+
                                   district+
                                   age_group+
                                   psu_group, design = demhip_svy)

summary(model_index_predict_givi)
ggemmeans(model_index_predict_givi, "givi_party_r")
ggemmeans(model_index_predict_givi, "district")

table(demhip$q14_group)
demhip$survey_treatment <- demhip$q14_group
demhip$survey_treatment <- as.factor(demhip$survey_treatment)
table(demhip$survey_treatment)
demhip_svy<-svydesign(~psu, strata = demhip$stratum, weights = demhip$indwt, data = demhip)
xxxx<-svyglm(index~givi_party_r+emp+
               edu+
               wealth+
               idp+
               sex+
               district+
               age_group+
               psu_group+
               survey_treatment, design = demhip_svy)

summary(xxxx)
ggemmeans(xxxx, "givi_party_r")
ggemmeans(xxxx, "tr_givi")
car::vif(xxxx)

svymean(~index, design = demhip_svy, na.rm = T)
car::vif(model_index_predict_givi)


demhip$party_reversed <- NA
demhip$party_reversed[demhip$givi_party_r == "20" ]<-1# GD
demhip$party_reversed[demhip$givi_party_r == "30" ]<-0# Opposition
demhip$party_reversed[demhip$givi_party_r == "10" ]<-2#Unaffiliated
table(demhip$party_reversed)
demhip$party_reversed <- as.factor(demhip$party_reversed)
demhip_svy<-svydesign(~psu, strata = demhip$stratum, weights = demhip$indwt, data = demhip)
model_index_predict_givi_rev<-svyglm(index~
                                       party_reversed+emp+
                                       edu+
                                       wealth+
                                       idp+
                                       sex+
                                       district+
                                       age_group+
                                       psu_group, design = demhip_svy)

summary(model_index_predict_givi_rev)
ggemmeans(model_index_predict_givi_rev, "party_reversed")
### ---------------end-------- GIVI ----------------------------------------------------------------------------------------------------------------------------

