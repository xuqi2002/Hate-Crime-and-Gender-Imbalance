# Replication  File for numbers we mention in the main text of the paper. 
# Please see "ContextAnalysis_Main.R", "SurveyAnalysis_Main.R", "ContextAnalysis_Appendix.R", and "SurveyAnalysis_Appendix.R"
# to reproduce Tables and Figures in the paper and in the appendix. 

# R version 4.0.2 (2020-06-22)

rm(list=ls())
# install.packages("readstata13")  #  readstata13_0.9.2
# install.packages("MASS") # MASS_7.3-51.6  
# install.packages("sandwich")  # sandwich_2.5-1 
# install.packages("lmtest") # lmtest_0.9-37 

require(readstata13)  #  readstata13_0.9.2
require(MASS) # MASS_7.3-51.6  
require(sandwich)  # sandwich_2.5-1 
require(lmtest) # lmtest_0.9-37  
source("Help.R")

# ###############
# Section: Existing Explanations and Mate Competition
# ###############
you_data <- read.dta13(file  = "YouGov.dta")

# The number of people who think marriages between a German woman and a non-German man is common and very common
prop.table(table(you_data$int_marriage))[3:4]

# ###############
# Section: Refugees and Mate Competition: A Topic of Debate
# ###############
# The number of hate crimes in 2015 and 2016 
dat <- read.dta13("context.dta")
tapply(dat$Hate_all_muni, dat$year, sum)[1:2]


# ###############
# Section: Empirical Analyses
# ###############
# ###############
# Sub Section: Mate Competition and the Incidence of Anti-Refugee Hate Crime
# ###############
# The proportion of municipalities that witnessed at least one hate crime in each year
round(tapply(dat$Hate_all_muni_bin, dat$year, mean)*100, 1)

# The proportion of municipalities that witnessed at least one hate crime in three years
dat_2015 <- dat[dat$year == 2015, ]
dat_2016 <- dat[dat$year == 2016, ]
dat_2017 <- dat[dat$year == 2017, ]
dat_2015$Hate_all_muni_1517 <- dat_2015$Hate_all_muni + dat_2016$Hate_all_muni + dat_2017$Hate_all_muni
dat_2015$Hate_all_muni_1517_bin <- ifelse(dat_2015$Hate_all_muni_1517 > 0, 1, 0)
round(mean(dat_2015$Hate_all_muni_1517_bin)*100)

# Point Estimates that we mention when discussing Figure 1
# Remove Extreme Value of Excess Males 
range_x <- quantile(dat_2015$pop_15_44_muni_gendergap_2015, c(0.025, 0.975), na.rm = TRUE)
dat_2015_s <- dat_2015[dat_2015$pop_15_44_muni_gendergap_2015 >= range_x[1] & 
                         dat_2015$pop_15_44_muni_gendergap_2015 <= range_x[2], ]
dat_s <- dat[dat$pop_15_44_muni_gendergap_2015 >= range_x[1] & 
               dat$pop_15_44_muni_gendergap_2015 <= range_x[2], ]

# The size of population for exxluded municipalities
dat_exc <- dat[dat$pop_15_44_muni_gendergap_2015 < range_x[1] | 
                 dat$pop_15_44_muni_gendergap_2015 > range_x[2], ]

ceiling(median(dat_exc$population_muni_2015, na.rm = TRUE)) # 247 

# sum
bin_1_sum <- bin.summary(Hate_all_muni_1517_bin ~ 
                           pop_15_44_muni_gendergap_2015 + 
                           log_population_muni_2015 + log_popdens_muni_2015 + 
                           log_unemp_all_muni_2015 + d_pop1511_muni + vote_afd_2013_muni + 
                           log_ref_inflow_1514  + log_pop_ref_2014 + log_violence_percap_2015 + ## county level
                           pc_hidegree_all2011 + d_manuf1115 + pc_manufacturing_2015 + ## county level
                           unemp_gendergap_2015 + as.factor(ags_state),
                         id  = "ags_county", data = dat_2015_s)

# annual
bin_1_p <- bin.summary(Hate_all_muni_bin ~ 
                         pop_15_44_muni_gendergap_2015 + 
                         log_population_muni_2015 + log_popdens_muni_2015 + 
                         log_unemp_all_muni_2015 + d_pop1511_muni + vote_afd_2013_muni + 
                         log_ref_inflow_1514  + log_pop_ref_2014 + log_violence_percap_2015 + ## county level
                         pc_hidegree_all2011 + d_manuf1115 + pc_manufacturing_2015 + ## county level
                         unemp_gendergap_2015 + as.factor(ags_state) + as.factor(year),
                       id  = "ags_county", data = dat_s)

# Excess Males
# Effect Estimation 
bin_1_sum_effect <- marginal_effect(bin_1_sum,
                                    newdata = dat_2015_s, family = "logit",
                                    main_var = "pop_15_44_muni_gendergap_2015",
                                    difference = TRUE, 
                                    treat_range = c(1, 1.2))

bin_1_p_effect <- marginal_effect(bin_1_p,
                                  newdata = dat_s, family = "logit",
                                  main_var = "pop_15_44_muni_gendergap_2015",
                                  difference = TRUE, 
                                  treat_range = c(1, 1.2))

# Point estimate
round(bin_1_sum_effect$out_main[1:3]*100, 2)
# 2.5%       97.5% 
# 0.88  2.60  4.19 
round(bin_1_p_effect$out_main[1:3]*100, 2)
# 2.5%       97.5% 
# 0.76  1.71  2.55 


## Comparing to round(bin_1_sum_effect$out_main[1:3]*100, 2)
## The Effect of Unemployment 
effect_unemp <- marginal_effect(bin_1_sum,
                                newdata = dat_2015_s, family = "logit",
                                main_var = "log_unemp_all_muni_2015",
                                difference = TRUE, 
                                treat_range = quantile(dat_2015_s$log_unemp_all_muni_2015, prob = c(0.2, 0.8), 
                                                       na.rm = TRUE))
round(effect_unemp$out_main[1:3]*100, 2) # 2.60 is more than half of 4.24 

## The Effect of Education
effect_educ <- marginal_effect(bin_1_sum,
                               newdata = dat_2015_s, family = "logit",
                               main_var = "pc_hidegree_all2011",
                               difference = TRUE, 
                               treat_range = quantile(dat_2015_s$pc_hidegree_all2011, prob = c(0.8, 0.2), 
                                                      na.rm = TRUE))
round(effect_educ$out_main[1:3]*100, 2) 
# 2.60 is more than twice of 1.20


# Correlation between Excess Males and Male Disadvantage
round(cor(dat_2015_s$pop_15_44_muni_gendergap_2015, dat_2015_s$unemp_gendergap_2015, use = "complete.obs"), 3)


# ###############
# Sub Section: Mate Competition and Support for Anti-Refugee Hate Crime
# ###############
rm(list=ls())
# install.packages("readstata13")  #  readstata13_0.9.2
# install.packages("MASS") # MASS_7.3-51.6  
# install.packages("sandwich")  # sandwich_2.5-1 
# install.packages("lmtest") # lmtest_0.9-37 
# install.packages("pBrackets") # pBrackets_1.0  
# install.packages("stargazer") # stargazer_5.2.2

require(readstata13)  #  readstata13_0.9.2
require(MASS) # MASS_7.3-51.6  
require(sandwich)  # sandwich_2.5-1 
require(lmtest) # lmtest_0.9-37  
require(pBrackets) # pBrackets_1.0  
require(stargazer) # stargazer_5.2.2
source("Help.R")

dat <- read.dta13(file =  "survey.dta")

# Subset to people in the wave 4
dat_use <- dat[dat$wave == 4, ]

# Prepare Two data sets
dat_male <- dat_use[dat_use$gender == "Male" & dat_use$age <= 44 & dat_use$age >= 18, ]
dat_male_y <- dat_use[dat_use$gender == "Male" & dat_use$age <= 40 & dat_use$age >= 30, ]

# Overall Samples 
dat_use$MateComp.cont_bin <- ifelse(dat_use$MateComp.cont >= 3, 1, 0)
dat_use$excess_c <- ifelse(dat_use$pop_15_44_muni_gendergap_2015 < 1.04, "1", 
                           ifelse(dat_use$pop_15_44_muni_gendergap_2015 < 1.12, "2", "3"))
mean_all <- tapply(dat_use$MateComp.cont_bin, dat_use$excess_c, mean)
se_all <- tapply(dat_use$MateComp.cont_bin, dat_use$excess_c, sd)/sqrt(table(dat_use$excess_c))

# Male (18 - 44)
dat_male$MateComp.cont_bin <- ifelse(dat_male$MateComp.cont >= 3, 1, 0)
dat_male$excess_c <- ifelse(dat_male$pop_15_44_muni_gendergap_2015 < 1.04, "1", 
                            ifelse(dat_male$pop_15_44_muni_gendergap_2015 < 1.12, "2", "3"))
mean_all_m <- tapply(dat_male$MateComp.cont_bin, dat_male$excess_c, mean)
se_all_m <- tapply(dat_male$MateComp.cont_bin, dat_male$excess_c, sd)/sqrt(table(dat_male$excess_c))

# Male (30 - 40)
dat_male_y$MateComp.cont_bin <- ifelse(dat_male_y$MateComp.cont >= 3, 1, 0)
dat_male_y$excess_c <- ifelse(dat_male_y$pop_15_44_muni_gendergap_2015 < 1.04, "1", 
                              ifelse(dat_male_y$pop_15_44_muni_gendergap_2015 < 1.12, "2", "3"))
mean_all_y <- tapply(dat_male_y$MateComp.cont_bin, dat_male_y$excess_c, mean)
se_all_y <- tapply(dat_male_y$MateComp.cont_bin, dat_male_y$excess_c, sd)/sqrt(table(dat_male_y$excess_c))


round(mean_all,2)[c(1,3)] ## 0.18 0.22 
round(mean_all_m,2)[c(1,3)] ## 0.23 0.38 
round(mean_all_y,2)[c(1,3)] ## 0.17 0.47 

# ################
# List Experiment
# ###############
rm(list=ls())
# install.packages("readstata13")  #  readstata13_0.9.2
# install.packages("MASS") # MASS_7.3-51.6  
# install.packages("sandwich")  # sandwich_2.5-1 
# install.packages("lmtest") # lmtest_0.9-37 
# install.packages("list") # list_9.2 

require(readstata13)  #  readstata13_0.9.2
require(MASS) # MASS_7.3-51.6  
require(sandwich)  # sandwich_2.5-1 
require(lmtest) # lmtest_0.9-37  
require(list) # list_9.2 

dat <- read.dta13(file =  "survey.dta")
data.u2 <- dat[dat$wave == 2, ]

# Means: When it comes to the refugee problem, violence is sometimes the only means that citizens have to get the attention of German politicians
data.list.u2   <- data.u2[data.u2$list == "1",]
data.direct.u2 <- data.u2[data.u2$list == "2",]
data.list.u2 <- data.list.u2[is.na(data.list.u2$treatment_list)==FALSE,]
data.list.u2$List.treat <- ifelse(data.list.u2$treatment_list == "Scenario 2", 1, 0)

# The mean for Control Group
round(mean(data.list.u2$outcome_list[data.list.u2$List.treat == 0]), 2)
# The mean for Treatment Group
round(mean(data.list.u2$outcome_list[data.list.u2$List.treat == 1]), 2)
# Please see "SurveyAnalysis_Main.R" for complete analysis, which was used to create Figure 3. 
# Here, we only reproduce the code for numbers we mention in the paper. 

## Compare to All People who answered Direct Question (n = 2170)
data.u2.all.direct <- data.u2[is.na(data.u2$hate_violence_means) == FALSE, ]
data.u2.all.direct$hate.direct.bin <- ifelse(data.u2.all.direct$hate_violence_means >=3, 1, 0)
round(mean(data.u2.all.direct$hate.direct.bin)*100) ## 18