# Replication File for Appendix Survey Analyses
# Table C1 in Appendix C1: The Effect of Excess Males on the Probability of Observing at least One Hate Crime
# Table C2 in Appendix C2: The Effect of Excess Males on the Probability of Observing at least One Hate Crime (Different Definition of “Excess Males”)
# Table C3 in Appendix C3: The Effect of Excess Males on the Probability of Observing at least One Hate Crime (linear probability model)
# Table C4 in Appendix C4: The Effect of Excess Males on the Probability of Observing at least One Physical Attack
# Table C5 in Appendix C5: Negative Binomial Regression
# Table C6 in Appendix C6: Interaction  between  Excess  Males  and  East/West  Germany
# Table C7 in Appendix C7: Interaction between Excess Males and Refugee Inflow
# Table C9 in Appendix C9: Placebo Analysis
# Appendix C10: Descriptive Statistics

# R version 4.0.2 (2020-06-22)

rm(list=ls())
# install.packages("readstata13")  #  readstata13_0.9.2
# install.packages("MASS") # MASS_7.3-51.6  
# install.packages("sandwich")  # sandwich_2.5-1 
# install.packages("lmtest") # lmtest_0.9-37 
# install.packages("stargazer") # stargazer_5.2.2

require(readstata13)  #  readstata13_0.9.2
require(MASS) # MASS_7.3-51.6  
require(sandwich)  # sandwich_2.5-1 
require(lmtest) # lmtest_0.9-37  
require(stargazer) # stargazer_5.2.2
source("Help.R")

dat <- read.dta13("context.dta")

dat_2015 <- dat[dat$year == 2015, ]
dat_2016 <- dat[dat$year == 2016, ]
dat_2017 <- dat[dat$year == 2017, ]
dat_2015$Hate_all_muni_1517 <- dat_2015$Hate_all_muni + dat_2016$Hate_all_muni + dat_2017$Hate_all_muni
dat_2015$Hate_all_muni_1517_bin <- ifelse(dat_2015$Hate_all_muni_1517 > 0, 1, 0)

# Remove Extreme Value of Excess Males 
range_x <- quantile(dat_2015$pop_15_44_muni_gendergap_2015, c(0.025, 0.975), na.rm = TRUE)
dat_2015_s <- dat_2015[dat_2015$pop_15_44_muni_gendergap_2015 >= range_x[1] & 
                         dat_2015$pop_15_44_muni_gendergap_2015 <= range_x[2], ]
dat_s <- dat[dat$pop_15_44_muni_gendergap_2015 >= range_x[1] & 
               dat$pop_15_44_muni_gendergap_2015 <= range_x[2], ]

# ##########################################
# Main Table (Table C1 in Appendix C1) 
# ##########################################
bin_1_sum <- bin.summary(Hate_all_muni_1517_bin ~ 
                           pop_15_44_muni_gendergap_2015 + 
                           log_population_muni_2015 + log_popdens_muni_2015 + 
                           as.factor(ags_county), # county fixed effects
                         id  = "ags_county", data = dat_2015_s)

bin_1_p <- bin.summary(Hate_all_muni_bin ~ 
                         pop_15_44_muni_gendergap_2015 + 
                         log_population_muni_2015 + log_popdens_muni_2015 + 
                         as.factor(ags_county) + as.factor(year), # county + year fixed effects
                       id  = "ags_county", data = dat_s)

bin_2_sum <- bin.summary(Hate_all_muni_1517_bin ~ 
                           pop_15_44_muni_gendergap_2015 + 
                           log_population_muni_2015 + log_popdens_muni_2015 + 
                           log_unemp_all_muni_2015 + d_pop1511_muni + vote_afd_2013_muni + 
                           as.factor(ags_county), # county fixed effects
                         id  = "ags_county", data = dat_2015_s)

bin_2_p <- bin.summary(Hate_all_muni_bin ~ 
                         pop_15_44_muni_gendergap_2015 + 
                         log_population_muni_2015 + log_popdens_muni_2015 + 
                         log_unemp_all_muni_2015 + d_pop1511_muni + vote_afd_2013_muni + 
                         as.factor(ags_county) + as.factor(year), # county + year fixed effects
                       id  = "ags_county", data = dat_s)

bin_3_sum <- bin.summary(Hate_all_muni_1517_bin ~ 
                           pop_15_44_muni_gendergap_2015 + 
                           log_population_muni_2015 + log_popdens_muni_2015 + 
                           log_unemp_all_muni_2015 + d_pop1511_muni + vote_afd_2013_muni + 
                           log_ref_inflow_1514  + log_pop_ref_2014 + log_violence_percap_2015 + ## county level
                           pc_hidegree_all2011 + d_manuf1115 + pc_manufacturing_2015 + ## county level
                           unemp_gendergap_2015 + 
                           as.factor(ags_state), # state fixed effects
                         id  = "ags_county", data = dat_2015_s)

bin_3_p <- bin.summary(Hate_all_muni_bin ~ 
                         pop_15_44_muni_gendergap_2015 + 
                         log_population_muni_2015 + log_popdens_muni_2015 + 
                         log_unemp_all_muni_2015 + d_pop1511_muni + vote_afd_2013_muni + 
                         log_ref_inflow_1514  + log_pop_ref_2014 + log_violence_percap_2015 + ## county level
                         pc_hidegree_all2011 + d_manuf1115 + pc_manufacturing_2015 + ## county level
                         unemp_gendergap_2015 + 
                         as.factor(ags_state) + as.factor(year), # state + year fixed effects
                       id  = "ags_county", data = dat_s)

fit_list <- list(bin_1_sum$fit, bin_1_p$fit,  
                 bin_2_sum$fit, bin_2_p$fit,  
                 bin_3_sum$fit, bin_3_p$fit)
se_list  <- list(sqrt(diag(bin_1_sum$vcov)), sqrt(diag(bin_1_p$vcov)),
                 sqrt(diag(bin_2_sum$vcov)), sqrt(diag(bin_2_p$vcov)),
                 sqrt(diag(bin_3_sum$vcov)), sqrt(diag(bin_3_p$vcov)))

star_out(stargazer(fit_list, se = se_list,
                   covariate.labels = c("Excess Males (Age 15 - 44)", 
                                        "Log (Population)","Log (Population Density)", 
                                        "Log (Unemployment Rate)",
                                        "% of population change (2011 vs 2015)", 
                                        "Vote share for AfD (2013)", 
                                        "Log (Refugee Inflow) (2014 vs 2015)",  "Log (Refugee Size) (2014)",  
                                        "Log (General Violence per capita)",
                                        "% of High Education", 
                                        "Change in Manufacturing Share (2011 vs 2015)",
                                        "Share of Manufacturing", "Male Disadvantage"),
                   keep=c("pop_15_44_muni_gendergap_2015", 
                          "log_population_muni_2015", 
                          "log_popdens_muni_2015", "log_unemp_all_muni_2015",
                          "d_pop1511_muni", "vote_afd_2013_muni", 
                          "log_ref_inflow_1514", "log_pop_ref_2014",
                          "log_violence_percap_2015", 
                          "pc_hidegree_all2011", "d_manuf1115", "pc_manufacturing_2015", "unemp_gendergap_2015")),
         name = "table_C1.tex")

# ###############################################
# Replicate Table with 25-44 (Table C2 in C2)
# ###############################################
range_x2 <- quantile(dat_2015$pop_25_44_muni_gendergap_2015, c(0.025, 0.975), na.rm = TRUE)
dat_2015_s2 <- dat_2015[dat_2015$pop_25_44_muni_gendergap_2015 >= range_x2[1] & 
                          dat_2015$pop_25_44_muni_gendergap_2015 <= range_x2[2], ]
dat_s2 <- dat[dat$pop_25_44_muni_gendergap_2015 >= range_x2[1] & 
                dat$pop_25_44_muni_gendergap_2015 <= range_x2[2], ]


bin_r_1_sum <- bin.summary(Hate_all_muni_1517_bin ~ 
                             pop_25_44_muni_gendergap_2015 + 
                             log_population_muni_2015 + log_popdens_muni_2015 + 
                             as.factor(ags_county),
                           id  = "ags_county", data = dat_2015_s2)

bin_r_1_p <- bin.summary(Hate_all_muni_bin ~ 
                           pop_25_44_muni_gendergap_2015 + 
                           log_population_muni_2015 + log_popdens_muni_2015 + 
                           as.factor(ags_county) + as.factor(year),
                         id  = "ags_county", data = dat_s2)

bin_r_2_sum <- bin.summary(Hate_all_muni_1517_bin ~ 
                             pop_25_44_muni_gendergap_2015 + 
                             log_population_muni_2015 + log_popdens_muni_2015 + 
                             log_unemp_all_muni_2015 + d_pop1511_muni + vote_afd_2013_muni + 
                             as.factor(ags_county),
                           id  = "ags_county", data = dat_2015_s2)

bin_r_2_p <- bin.summary(Hate_all_muni_bin ~ 
                           pop_25_44_muni_gendergap_2015 + 
                           log_population_muni_2015 + log_popdens_muni_2015 + 
                           log_unemp_all_muni_2015 + d_pop1511_muni + vote_afd_2013_muni + 
                           as.factor(ags_county) + as.factor(year),
                         id  = "ags_county", data = dat_s2)

bin_r_3_sum <- bin.summary(Hate_all_muni_1517_bin ~ 
                             pop_25_44_muni_gendergap_2015 + 
                             log_population_muni_2015 + log_popdens_muni_2015 + 
                             log_unemp_all_muni_2015 + d_pop1511_muni + vote_afd_2013_muni + 
                             log_ref_inflow_1514  + log_pop_ref_2014 + log_violence_percap_2015 + ## county level
                             pc_hidegree_all2011 + d_manuf1115 + pc_manufacturing_2015 + ## county level
                             unemp_gendergap_2015 + 
                             as.factor(ags_state),
                           id  = "ags_county", data = dat_2015_s2)

bin_r_3_p <- bin.summary(Hate_all_muni_bin ~ 
                           pop_25_44_muni_gendergap_2015 + 
                           log_population_muni_2015 + log_popdens_muni_2015 + 
                           log_unemp_all_muni_2015 + d_pop1511_muni + vote_afd_2013_muni + 
                           log_ref_inflow_1514  + log_pop_ref_2014 + log_violence_percap_2015 + ## county level
                           pc_hidegree_all2011 + d_manuf1115 + pc_manufacturing_2015 + ## county level
                           unemp_gendergap_2015 + 
                           as.factor(ags_state) + as.factor(year),
                         id  = "ags_county", data = dat_s2)


## Table C2 in Appendix C2
fit_list2 <- list(bin_r_1_sum$fit, bin_r_1_p$fit,  
                  bin_r_2_sum$fit, bin_r_2_p$fit,  
                  bin_r_3_sum$fit, bin_r_3_p$fit)
se_list2  <- list(sqrt(diag(bin_r_1_sum$vcov)), sqrt(diag(bin_r_1_p$vcov)),
                  sqrt(diag(bin_r_2_sum$vcov)), sqrt(diag(bin_r_2_p$vcov)),
                  sqrt(diag(bin_r_3_sum$vcov)), sqrt(diag(bin_r_3_p$vcov)))

star_out(stargazer(fit_list2, se = se_list2,
                   covariate.labels = c("Excess Males (Age 25 - 44)", 
                                        "Log (Population)","Log (Population Density)", 
                                        "Log (Unemployment Rate)",
                                        "% of population change (2011 vs 2015)", 
                                        "Vote share for AfD (2013)", 
                                        "Log (Refugee Inflow) (2014 vs 2015)",  "Log (Refugee Size) (2014)",  
                                        "Log (General Violence per capita)",
                                        "% of High Education", 
                                        "Change in Manufacturing Share (2011 vs 2015)",
                                        "Share of Manufacturing", "Male Disadvantage"),
                   keep=c("pop_25_44_muni_gendergap_2015", 
                          "log_population_muni_2015", 
                          "log_popdens_muni_2015", "log_unemp_all_muni_2015",
                          "d_pop1511_muni", "vote_afd_2013_muni", 
                          "log_ref_inflow_1514", "log_pop_ref_2014",
                          "log_violence_percap_2015", 
                          "pc_hidegree_all2011", "d_manuf1115", "pc_manufacturing_2015", "unemp_gendergap_2015")),
         name = "table_C2.tex")

# #####################################
# Linear Probability Model (Table C3 in Appendix C3)
# #####################################
lm_1_sum <- lm.summary(Hate_all_muni_1517_bin ~ 
                         pop_15_44_muni_gendergap_2015 + 
                         log_population_muni_2015 + log_popdens_muni_2015 + 
                         as.factor(ags_county),
                       id  = "ags_county", data = dat_2015_s)

lm_1_p <- lm.summary(Hate_all_muni_bin ~ 
                       pop_15_44_muni_gendergap_2015 + 
                       log_population_muni_2015 + log_popdens_muni_2015 + 
                       as.factor(ags_county) + as.factor(year),
                     id  = "ags_county", data = dat_s)

lm_2_sum <- lm.summary(Hate_all_muni_1517_bin ~ 
                         pop_15_44_muni_gendergap_2015 + 
                         log_population_muni_2015 + log_popdens_muni_2015 + 
                         log_unemp_all_muni_2015 + d_pop1511_muni + vote_afd_2013_muni + 
                         as.factor(ags_county),
                       id  = "ags_county", data = dat_2015_s)

lm_2_p <- lm.summary(Hate_all_muni_bin ~ 
                       pop_15_44_muni_gendergap_2015 + 
                       log_population_muni_2015 + log_popdens_muni_2015 + 
                       log_unemp_all_muni_2015 + d_pop1511_muni + vote_afd_2013_muni + 
                       as.factor(ags_county) + as.factor(year),
                     id  = "ags_county", data = dat_s)

lm_3_sum <- lm.summary(Hate_all_muni_1517_bin ~ 
                         pop_15_44_muni_gendergap_2015 + 
                         log_population_muni_2015 + log_popdens_muni_2015 + 
                         log_unemp_all_muni_2015 + d_pop1511_muni + vote_afd_2013_muni + 
                         log_ref_inflow_1514  + log_pop_ref_2014 + log_violence_percap_2015 + ## county level
                         pc_hidegree_all2011 + d_manuf1115 + pc_manufacturing_2015 + ## county level
                         unemp_gendergap_2015 + 
                         as.factor(ags_state),
                       id  = "ags_county", data = dat_2015_s)

lm_3_p <- lm.summary(Hate_all_muni_bin ~ 
                       pop_15_44_muni_gendergap_2015 + 
                       log_population_muni_2015 + log_popdens_muni_2015 + 
                       log_unemp_all_muni_2015 + d_pop1511_muni + vote_afd_2013_muni + 
                       log_ref_inflow_1514  + log_pop_ref_2014 + log_violence_percap_2015 + ## county level
                       pc_hidegree_all2011 + d_manuf1115 + pc_manufacturing_2015 + ## county level
                       unemp_gendergap_2015 + 
                       as.factor(ags_state) + as.factor(year),
                     id  = "ags_county", data = dat_s)


fit_list_lm <- list(lm_1_sum$fit, lm_1_p$fit,  
                    lm_2_sum$fit, lm_2_p$fit,  
                    lm_3_sum$fit, lm_3_p$fit)
se_list_lm  <- list(sqrt(diag(lm_1_sum$vcov)), sqrt(diag(lm_1_p$vcov)),
                    sqrt(diag(lm_2_sum$vcov)), sqrt(diag(lm_2_p$vcov)),
                    sqrt(diag(lm_3_sum$vcov)), sqrt(diag(lm_3_p$vcov)))

star_out(stargazer(fit_list_lm, se = se_list_lm,
                   covariate.labels = c("Excess Males (Age 15 - 44)", 
                                        "Log (Population)","Log (Population Density)", 
                                        "Log (Unemployment Rate)",
                                        "% of population change (2011 vs 2015)", 
                                        "Vote share for AfD (2013)", 
                                        "Log (Refugee Inflow) (2014 vs 2015)",  "Log (Refugee Size) (2014)",  
                                        "Log (General Violence per capita)",
                                        "% of High Education", 
                                        "Change in Manufacturing Share (2011 vs 2015)",
                                        "Share of Manufacturing", "Male Disadvantage"),
                   keep=c("pop_15_44_muni_gendergap_2015", 
                          "log_population_muni_2015", 
                          "log_popdens_muni_2015", "log_unemp_all_muni_2015",
                          "d_pop1511_muni", "vote_afd_2013_muni", 
                          "log_ref_inflow_1514", "log_pop_ref_2014",
                          "log_violence_percap_2015", 
                          "pc_hidegree_all2011", "d_manuf1115", "pc_manufacturing_2015", "unemp_gendergap_2015")),
         name = "table_C3.tex")


## ############################################
## Physical Attacks (Table C4 in Appendix C4)
## ############################################
dat_2015$Physical_muni_1517 <- dat_2015$Physical_muni + dat_2016$Physical_muni + dat_2017$Physical_muni
dat_2015$Physical_muni_1517_bin <- ifelse(dat_2015$Physical_muni_1517 > 0, 1, 0)
range_x <- quantile(dat_2015$pop_15_44_muni_gendergap_2015, c(0.025, 0.975), na.rm = TRUE)
dat_2015_s <- dat_2015[dat_2015$pop_15_44_muni_gendergap_2015 >= range_x[1] & 
                         dat_2015$pop_15_44_muni_gendergap_2015 <= range_x[2], ]
dat_s <- dat[dat$pop_15_44_muni_gendergap_2015 >= range_x[1] & 
               dat$pop_15_44_muni_gendergap_2015 <= range_x[2], ]

bin_phys_1_sum <- bin.summary(Physical_muni_1517_bin ~ 
                                pop_15_44_muni_gendergap_2015 + 
                                log_population_muni_2015 + log_popdens_muni_2015 + 
                                as.factor(ags_county),
                              id  = "ags_county", data = dat_2015_s)

bin_phys_1_p <- bin.summary(Physical_muni_bin ~ 
                              pop_15_44_muni_gendergap_2015 + 
                              log_population_muni_2015 + log_popdens_muni_2015 + 
                              as.factor(ags_county) + as.factor(year),
                            id  = "ags_county", data = dat_s)

bin_phys_2_sum <- bin.summary(Physical_muni_1517_bin ~ 
                                pop_15_44_muni_gendergap_2015 + 
                                log_population_muni_2015 + log_popdens_muni_2015 + 
                                log_unemp_all_muni_2015 + d_pop1511_muni + vote_afd_2013_muni + 
                                as.factor(ags_county),
                              id  = "ags_county", data = dat_2015_s)

bin_phys_2_p <- bin.summary(Physical_muni_bin ~ 
                              pop_15_44_muni_gendergap_2015 + 
                              log_population_muni_2015 + log_popdens_muni_2015 + 
                              log_unemp_all_muni_2015 + d_pop1511_muni + vote_afd_2013_muni + 
                              as.factor(ags_county) + as.factor(year),
                            id  = "ags_county", data = dat_s)

bin_phys_3_sum <- bin.summary(Physical_muni_1517_bin ~ 
                                pop_15_44_muni_gendergap_2015 + 
                                log_population_muni_2015 + log_popdens_muni_2015 + 
                                log_unemp_all_muni_2015 + d_pop1511_muni + vote_afd_2013_muni + 
                                log_ref_inflow_1514  + log_pop_ref_2014 + log_violence_percap_2015 + ## county level
                                pc_hidegree_all2011 + d_manuf1115 + pc_manufacturing_2015 + ## county level
                                unemp_gendergap_2015 + 
                                as.factor(ags_state),
                              id  = "ags_county", data = dat_2015_s)

bin_phys_3_p <- bin.summary(Physical_muni_bin ~ 
                              pop_15_44_muni_gendergap_2015 + 
                              log_population_muni_2015 + log_popdens_muni_2015 + 
                              log_unemp_all_muni_2015 + d_pop1511_muni + vote_afd_2013_muni + 
                              log_ref_inflow_1514  + log_pop_ref_2014 + log_violence_percap_2015 + ## county level
                              pc_hidegree_all2011 + d_manuf1115 + pc_manufacturing_2015 + ## county level
                              unemp_gendergap_2015 + 
                              as.factor(ags_state) + as.factor(year),
                            id  = "ags_county", data = dat_s)

## Table C4 in Appendix C4
fit_list_bin_phys <- list(bin_phys_1_sum$fit, bin_phys_1_p$fit,  
                          bin_phys_2_sum$fit, bin_phys_2_p$fit,  
                          bin_phys_3_sum$fit, bin_phys_3_p$fit)
se_list_bin_phys  <- list(sqrt(diag(bin_phys_1_sum$vcov)), sqrt(diag(bin_phys_1_p$vcov)),
                          sqrt(diag(bin_phys_2_sum$vcov)), sqrt(diag(bin_phys_2_p$vcov)),
                          sqrt(diag(bin_phys_3_sum$vcov)), sqrt(diag(bin_phys_3_p$vcov)))

star_out(stargazer(fit_list_bin_phys, se = se_list_bin_phys,
                   covariate.labels = c("Excess Males (Age 15 - 44)", 
                                        "Log (Population)","Log (Population Density)", 
                                        "Log (Unemployment Rate)",
                                        "% of population change (2011 vs 2015)", 
                                        "Vote share for AfD (2013)", 
                                        "Log (Refugee Inflow) (2014 vs 2015)",  "Log (Refugee Size) (2014)",  
                                        "Log (General Violence per capita)",
                                        "% of High Education", 
                                        "Change in Manufacturing Share (2011 vs 2015)",
                                        "Share of Manufacturing", "Male Disadvantage"),
                   keep=c("pop_15_44_muni_gendergap_2015", 
                          "log_population_muni_2015", 
                          "log_popdens_muni_2015", "log_unemp_all_muni_2015",
                          "d_pop1511_muni", "vote_afd_2013_muni", 
                          "log_ref_inflow_1514", "log_pop_ref_2014",
                          "log_violence_percap_2015", 
                          "pc_hidegree_all2011", "d_manuf1115", "pc_manufacturing_2015", "unemp_gendergap_2015")),
         name = "table_C4.tex")

## ########################################
## Appendix C5: Count Model 
## ########################################
rm(list=ls())

dat <- read.dta13("context.dta")
source("Help.R")

dat_2015 <- dat[dat$year == 2015, ]
dat_2016 <- dat[dat$year == 2016, ]
dat_2017 <- dat[dat$year == 2017, ]
dat_2015$Hate_all_muni_1517 <- dat_2015$Hate_all_muni + dat_2016$Hate_all_muni + dat_2017$Hate_all_muni

# Remove Extreme Value of Excess Males 
range_x <- quantile(dat_2015$pop_15_44_muni_gendergap_2015, c(0.025, 0.975), na.rm = TRUE)
dat_2015_s <- dat_2015[dat_2015$pop_15_44_muni_gendergap_2015 >= range_x[1] & 
                         dat_2015$pop_15_44_muni_gendergap_2015 <= range_x[2], ]
dat_s <- dat[dat$pop_15_44_muni_gendergap_2015 >= range_x[1] & 
               dat$pop_15_44_muni_gendergap_2015 <= range_x[2], ]

for_s <- as.formula(Hate_all_muni_1517 ~ 
                      pop_15_44_muni_gendergap_2015 + 
                      log_population_muni_2015 + log_popdens_muni_2015 + 
                      log_unemp_all_muni_2015 + d_pop1511_muni + vote_afd_2013_muni + 
                      log_ref_inflow_1514  + log_pop_ref_2014 + log_violence_percap_2015 + ## county level
                      pc_hidegree_all2011 + d_manuf1115 + pc_manufacturing_2015 + ## county level
                      unemp_gendergap_2015 + 
                      as.factor(ags_state)) # state fixed effects

for_p <- as.formula(Hate_all_muni ~ 
                      pop_15_44_muni_gendergap_2015 + 
                      log_population_muni_2015 + log_popdens_muni_2015 + 
                      log_unemp_all_muni_2015 + d_pop1511_muni + vote_afd_2013_muni + 
                      log_ref_inflow_1514  + log_pop_ref_2014 + log_violence_percap_2015 + ## county level
                      pc_hidegree_all2011 + d_manuf1115 + pc_manufacturing_2015 + ## county level
                      unemp_gendergap_2015 +  as.factor(ags_state) + as.factor(year)) # state + year fixed effects

## Use boostrap to compute standard errors (Use the "out_count.rdata" to get the exact same estimates)
## Note: the following codes take a while to run
full_run <- FALSE
if(full_run == TRUE){
  # Sum 
  nb_1_sum_b <- glm.boot(for_s, family = "negative-binomial", 
                         data = dat_2015_s, 
                         cluster_id = dat_2015_s$ags_county)
  
  # Panel
  nb_1_p_b <- glm.boot(for_p, family = "negative-binomial", 
                       data = dat_s, 
                       cluster_id = dat_s$ags_county)
  
  fit_list_nb <- list(nb_1_sum_b$fit, nb_1_p_b$fit)
  se_list_nb  <- list(nb_1_sum_b$se, nb_1_p_b$se)
  
  out_count_table <- list(fit_list_nb, se_list_nb)
  save(out_count_table, file =  "out_count_table.rdata")
}

load(file =  "out_count_table.rdata")
fit_list_nb <- out_count_table[[1]]
se_list_nb <- out_count_table[[2]]

star_out(stargazer(fit_list_nb, se = se_list_nb,
                   covariate.labels = c("Excess Males (Age 15 - 44)", 
                                        "Log (Population)","Log (Population Density)", 
                                        "Log (Unemployment Rate)",
                                        "% of population change (2011 vs 2015)", 
                                        "Vote share for AfD (2013)", 
                                        "Log (Refugee Inflow) (2014 vs 2015)",  "Log (Refugee Size) (2014)",  
                                        "Log (General Violence per capita)",
                                        "% of High Education", 
                                        "Change in Manufacturing Share (2011 vs 2015)",
                                        "Share of Manufacturing", "Male Disadvantage"),
                   keep=c("pop_15_44_muni_gendergap_2015", 
                          "log_population_muni_2015", 
                          "log_popdens_muni_2015", "log_unemp_all_muni_2015",
                          "d_pop1511_muni", "vote_afd_2013_muni", 
                          "log_ref_inflow_1514", "log_pop_ref_2014",
                          "log_violence_percap_2015", 
                          "pc_hidegree_all2011", "d_manuf1115", "pc_manufacturing_2015", "unemp_gendergap_2015")),
         name = "table_C5.tex")



## ########################################################################
## Replicate Tables with East/West Interaction (Table C6 in Appendix C6)
## ########################################################################
rm(list=ls())

dat <- read.dta13("context.dta")
source("Help.R")

dat_2015 <- dat[dat$year == 2015, ]
dat_2016 <- dat[dat$year == 2016, ]
dat_2017 <- dat[dat$year == 2017, ]
dat_2015$Hate_all_muni_1517 <- dat_2015$Hate_all_muni + dat_2016$Hate_all_muni + dat_2017$Hate_all_muni
dat_2015$Hate_all_muni_1517_bin <- as.numeric(dat_2015$Hate_all_muni_1517 > 0)

# Remove Extreme Value of Excess Males 
range_x <- quantile(dat_2015$pop_15_44_muni_gendergap_2015, c(0.025, 0.975), na.rm = TRUE)
dat_2015_s <- dat_2015[dat_2015$pop_15_44_muni_gendergap_2015 >= range_x[1] & 
                         dat_2015$pop_15_44_muni_gendergap_2015 <= range_x[2], ]
dat_s <- dat[dat$pop_15_44_muni_gendergap_2015 >= range_x[1] & 
               dat$pop_15_44_muni_gendergap_2015 <= range_x[2], ]

dat_2015_s$west <- 1 - dat_2015_s$east
dat_s$west <- 1 - dat_s$east

bin_2_sum_ew <- bin.summary(Hate_all_muni_1517_bin ~ 
                              pop_15_44_muni_gendergap_2015 + west + 
                              log_population_muni_2015 + log_popdens_muni_2015 + 
                              log_unemp_all_muni_2015 + d_pop1511_muni + vote_afd_2013_muni + 
                              log_ref_inflow_1514  + log_pop_ref_2014 + log_violence_percap_2015 + ## county level
                              pc_hidegree_all2011 + d_manuf1115 + pc_manufacturing_2015 + ## county level
                              unemp_gendergap_2015,
                            id  = "ags_county", data = dat_2015_s)

bin_2_p_ew <- bin.summary(Hate_all_muni_bin ~ 
                            pop_15_44_muni_gendergap_2015 + west + 
                            log_population_muni_2015 + log_popdens_muni_2015 + 
                            log_unemp_all_muni_2015 + d_pop1511_muni + vote_afd_2013_muni + 
                            log_ref_inflow_1514  + log_pop_ref_2014 + log_violence_percap_2015 + ## county level
                            pc_hidegree_all2011 + d_manuf1115 + pc_manufacturing_2015 + ## county level
                            unemp_gendergap_2015 + as.factor(year),
                          id  = "ags_county", data = dat_s)

bin_3_sum_ew <- bin.summary(Hate_all_muni_1517_bin ~ 
                              pop_15_44_muni_gendergap_2015*west + 
                              log_population_muni_2015 + log_popdens_muni_2015 + 
                              log_unemp_all_muni_2015 + d_pop1511_muni + vote_afd_2013_muni + 
                              log_ref_inflow_1514  + log_pop_ref_2014 + log_violence_percap_2015 + ## county level
                              pc_hidegree_all2011 + d_manuf1115 + pc_manufacturing_2015 + ## county level
                              unemp_gendergap_2015,
                            id  = "ags_county", data = dat_2015_s)

bin_3_p_ew <- bin.summary(Hate_all_muni_bin ~ 
                            pop_15_44_muni_gendergap_2015*west + 
                            log_population_muni_2015 + log_popdens_muni_2015 + 
                            log_unemp_all_muni_2015 + d_pop1511_muni + vote_afd_2013_muni + 
                            log_ref_inflow_1514  + log_pop_ref_2014 + log_violence_percap_2015 + ## county level
                            pc_hidegree_all2011 + d_manuf1115 + pc_manufacturing_2015 + ## county level
                            unemp_gendergap_2015 + as.factor(year),
                          id  = "ags_county", data = dat_s)

fit_list_ew <- list(bin_2_sum_ew$fit, bin_2_p_ew$fit, 
                    bin_3_sum_ew$fit, bin_3_p_ew$fit)
se_list_ew  <- list(sqrt(diag(bin_2_sum_ew$vcov)), sqrt(diag(bin_2_p_ew$vcov)),
                    sqrt(diag(bin_3_sum_ew$vcov)), sqrt(diag(bin_3_p_ew$vcov)))

star_out(stargazer(fit_list_ew, se = se_list_ew,
                   covariate.labels = c("Excess Males (Age 15 - 44)", "West",
                                        "Log (Population)","Log (Population Density)", 
                                        "Log (Unemployment Rate)",
                                        "% of population change (2011 vs 2015)", 
                                        "Vote share for AfD (2013)", 
                                        "Log (Refugee Inflow) (2014 vs 2015)",  
                                        "Log (Refugee Size) (2014)",  
                                        "Log (General Violence per capita)",
                                        "% of High Education", 
                                        "Change in Manufacturing Share (2011 vs 2015)",
                                        "Share of Manufacturing", "Male Disadvantage",
                                        "Excess Males x West"),
                   keep=c("pop_15_44_muni_gendergap_2015", "west",
                          "log_population_muni_2015", 
                          "log_popdens_muni_2015", "log_unemp_all_muni_2015",
                          "d_pop1511_muni", "vote_afd_2013_muni", 
                          "log_ref_inflow_1514", 
                          "log_pop_ref_2014",
                          "log_violence_percap_2015", 
                          "pc_hidegree_all2011", "d_manuf1115", "pc_manufacturing_2015", "unemp_gendergap_2015",
                          "pop_15_44_muni_gendergap_2015:west")),
         name = "table_C6.tex")


## ##############################################################
## Interaction with Refugee Inflow (Table C7 in Appendix C7)
## ##############################################################
bin_sum_int <- bin.summary(Hate_all_muni_1517_bin ~ 
                             pop_15_44_muni_gendergap_2015*log_ref_inflow_1514 + 
                             log_population_muni_2015 + log_popdens_muni_2015 + 
                             log_unemp_all_muni_2015 + d_pop1511_muni + vote_afd_2013_muni + 
                             log_ref_inflow_1514  + log_pop_ref_2014 + log_violence_percap_2015 + ## county level
                             pc_hidegree_all2011 + d_manuf1115 + pc_manufacturing_2015 + ## county level
                             unemp_gendergap_2015 + 
                             as.factor(ags_state),
                           id  = "ags_county", data = dat_2015_s)

bin_p_int <- bin.summary(Hate_all_muni_bin ~ 
                           pop_15_44_muni_gendergap_2015*log_ref_inflow_1514 + 
                           log_population_muni_2015 + log_popdens_muni_2015 + 
                           log_unemp_all_muni_2015 + d_pop1511_muni + vote_afd_2013_muni + 
                           log_ref_inflow_1514  + log_pop_ref_2014 + log_violence_percap_2015 + ## county level
                           pc_hidegree_all2011 + d_manuf1115 + pc_manufacturing_2015 + ## county level
                           unemp_gendergap_2015 + 
                           as.factor(ags_state) + as.factor(year),
                         id  = "ags_county", data = dat_s)

## Table C7 in Appendix C7
fit_list_int <- list(bin_sum_int$fit, bin_p_int$fit)
se_list_int  <- list(sqrt(diag(bin_sum_int$vcov)), sqrt(diag(bin_p_int$vcov)))

star_out(stargazer(fit_list_int, se = se_list_int,
                   covariate.labels = c("Excess Males (Age 15 - 44)", 
                                        "Log (Refugee Inflow) (2014 vs 2015)",  
                                        "Log (Population)","Log (Population Density)", 
                                        "Log (Unemployment Rate)",
                                        "% of population change (2011 vs 2015)", 
                                        "Vote share for AfD (2013)", 
                                        "Log (Refugee Size) (2014)",  
                                        "Log (General Violence per capita)",
                                        "% of High Education", 
                                        "Change in Manufacturing Share (2011 vs 2015)",
                                        "Share of Manufacturing", "Male Disadvantage",
                                        "Excess Males × Log (Refugee Inflow)"),
                   keep=c("pop_15_44_muni_gendergap_2015", "log_ref_inflow_1514", 
                          "log_population_muni_2015", 
                          "log_popdens_muni_2015", "log_unemp_all_muni_2015",
                          "d_pop1511_muni", "vote_afd_2013_muni", 
                          "log_pop_ref_2014",
                          "log_violence_percap_2015", 
                          "pc_hidegree_all2011", "d_manuf1115", "pc_manufacturing_2015", "unemp_gendergap_2015",
                          "pop_15_44_muni_gendergap_2015:log_ref_inflow_1514")),
         name = "table_C7.tex")

# ################################################################
# Appendix C9. Placebo Analysis
# ###############################################################
rm(list=ls())

dat_pl <- read.dta13("context_placebo.dta") # data for placebo analysis
source("Help.R")

dat_2015_s <- dat_pl[dat_pl$year == 2015, ]
dat_2016_s <- dat_pl[dat_pl$year == 2016, ]
dat_2017_s <- dat_pl[dat_pl$year == 2017, ]

# ##########################################
# 2015
# ##########################################
# main model + Placebo Treatment 
bin_15_sum_pl <- bin.summary(Hate_all_muni_bin ~ 
                               pop_15_44_muni_gendergap_future + 
                               pop_15_44_muni_gendergap_anu + 
                               log(population_muni_anu) + log(popdens_muni_anu) + 
                               log_unemp_all_muni_anu + d_pop_muni_anu + vote_afd_2013_muni + 
                               log_ref_inflow_anu  + log(pop_ref_anu) + log(violence_percap_anu) + ## county level
                               pc_hidegree_all2011 + d_manuf1115 + pc_manufacturing_2015 + ## county level
                               unemp_gendergap_anu + 
                               as.factor(ags_state), # state fixed effects
                             id  = "ags_county", data = dat_2015_s)

# ##########################################
# 2016
# ##########################################
# Main model + Placebo 
bin_16_sum_pl <- bin.summary(Hate_all_muni_bin ~ pop_15_44_muni_gendergap_future + 
                               pop_15_44_muni_gendergap_anu + 
                               log(population_muni_anu) + log(popdens_muni_anu) + 
                               log_unemp_all_muni_anu + d_pop_muni_anu + vote_afd_2013_muni + 
                               log_ref_inflow_anu  + log(pop_ref_anu) + log(violence_percap_anu) + ## county level
                               pc_hidegree_all2011 + d_manuf1115 + pc_manufacturing_2015 + ## county level
                               unemp_gendergap_anu + 
                               as.factor(ags_state), # state fixed effects
                             id  = "ags_county", data = dat_2016_s)

# ##########################
# 2017
# #########################
# Main model + Placebo 
bin_17_sum_pl <- bin.summary(Hate_all_muni_bin ~ pop_15_44_muni_gendergap_future + 
                               pop_15_44_muni_gendergap_anu + 
                               log(population_muni_anu) + log(popdens_muni_anu) + 
                               log_unemp_all_muni_anu + d_pop_muni_anu + vote_afd_2013_muni + 
                               log_ref_inflow_anu  + log(pop_ref_anu) + log(violence_percap_anu) + ## county level
                               pc_hidegree_all2011 + d_manuf1115 + pc_manufacturing_2015 + ## county level
                               unemp_gendergap_anu + 
                               as.factor(ags_state), # state fixed effects
                             id  = "ags_county", data = dat_2017_s)

## ####################
## Pooled Analysis
## ####################
# Final model + Placebo 
bin_pool_sum_pl <- bin.summary(Hate_all_muni_bin ~ pop_15_44_muni_gendergap_future + 
                                 pop_15_44_muni_gendergap_anu + 
                                 log(population_muni_anu) + log(popdens_muni_anu) + 
                                 log_unemp_all_muni_anu + d_pop_muni_anu + vote_afd_2013_muni + 
                                 log_ref_inflow_anu  + log(pop_ref_anu) + log(violence_percap_anu) + ## county level
                                 pc_hidegree_all2011 + d_manuf1115 + pc_manufacturing_2015 + ## county level
                                 unemp_gendergap_anu + 
                                 as.factor(ags_state) + as.factor(year), # state + year fixed effects
                               id  = "ags_county", data = dat_pl)

# Repeat the analysis for Large Counties
dat_pool_s_l <- dat_pl[dat_pl$population_muni_anu > 
                         quantile(dat_pl$population_muni_anu, prob  = 0.5), ] 

# Main model + Placebo 
bin_pool_sum_pl_l <- bin.summary(Hate_all_muni_bin ~ pop_15_44_muni_gendergap_future + 
                                   pop_15_44_muni_gendergap_anu + 
                                   log(population_muni_anu) + log(popdens_muni_anu) + 
                                   log_unemp_all_muni_anu + d_pop_muni_anu + vote_afd_2013_muni + 
                                   log_ref_inflow_anu  + log(pop_ref_anu) + log(violence_percap_anu) + ## county level
                                   pc_hidegree_all2011 + d_manuf1115 + pc_manufacturing_2015 + ## county level
                                   unemp_gendergap_anu + 
                                   as.factor(ags_state) + as.factor(year), # state + year fixed effects
                                 id  = "ags_county", data = dat_pool_s_l)

# Table
pl_fit_list_m <- list(bin_15_sum_pl$fit,
                      bin_16_sum_pl$fit,
                      bin_17_sum_pl$fit,
                      bin_pool_sum_pl$fit,
                      bin_pool_sum_pl_l$fit)
pl_se_list_m  <- list(sqrt(diag(bin_15_sum_pl$vcov)),
                      sqrt(diag(bin_16_sum_pl$vcov)),
                      sqrt(diag(bin_17_sum_pl$vcov)),
                      sqrt(diag(bin_pool_sum_pl$vcov)),
                      sqrt(diag(bin_pool_sum_pl_l$vcov)))

star_out(stargazer(pl_fit_list_m, se = pl_se_list_m,
                   covariate.labels = c("Future-Treatment"),
                   keep=c("pop_15_44_muni_gendergap_future")),
         name = "table_C9.tex")


# ##############################
# Appendix C10. Descriptive Statistics
# ##############################
rm(list=ls())

dat <- read.dta13("context.dta")

min15 <- round(min(dat$pc_ref_male[dat$year ==  2015], na.rm = TRUE),2)
min16 <- round(min(dat$pc_ref_male[dat$year ==  2016], na.rm = TRUE),2)

pdf("figure_C10.pdf", height = 5, width = 10)
par(mfrow = c(1, 2))
plot(density(dat$pc_ref_male[dat$year ==  2015], na.rm  = TRUE),
     main = "Proportion of Male Refugees (2015)", 
     xlim = c(50, 100), xlab = "Percent of Male Refugees Among Refugees (county)")
text(x = 90, y = 0.08, paste0("min = ", min15), font = 2)
polygon(density(dat$pc_ref_male[dat$year ==  2015], na.rm  = TRUE)$x,
        density(dat$pc_ref_male[dat$year ==  2015], na.rm  = TRUE)$y,col='grey80')

plot(density(dat$pc_ref_male[dat$year ==  2016], na.rm  = TRUE),
     main = "Proportion of Male Refugees (2016)", xlim = c(50, 100),
     xlab = "Percent of Male Refugees Among Refugees (county)")
text(x = 90, y = 0.12, paste0("min = ", min16), font = 2)
polygon(density(dat$pc_ref_male[dat$year ==  2016], na.rm  = TRUE)$x,
        density(dat$pc_ref_male[dat$year ==  2016], na.rm  = TRUE)$y,col='grey80')
dev.off()

