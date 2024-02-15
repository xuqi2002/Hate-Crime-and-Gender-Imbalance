# Replication File for Survey Analysis  
# Figure 2: Individuals Living in Municipalities with a Higher Degree ofExcess MalesPerceiveMore Mate Competition
# Figure 3: List Experiment
# Table 1: Mate Competition Predicts Support for Hate Crime
# Figure 4: Estimated Effects of Mate Competition on Support for Hate Crimes

# R version 4.0.2 (2020-06-22)

# ##################
# Data Preparation  
# ##################
rm(list=ls())
# Set the directly appropriately

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

# #######################
# Figure 2
# #######################
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


mean_all ## 0.1835004 0.1970803 0.2244489 
mean_all_m ## 0.2282609 0.2745902 0.3750000 
mean_all_y ## 0.1743119 0.2818182 0.4705882 

{
  diff <- c(mean_all[2] - mean_all[1], 
            mean_all[3] - mean_all[2],  
            mean_all[3] - mean_all[1])
  sd_d <- c(sqrt(se_all[2]^2 + se_all[1]^2), 
            sqrt(se_all[3]^2 + se_all[2]^2),  
            sqrt(se_all[3]^2 + se_all[1]^2))
  diff_m <- c(mean_all_m[2] - mean_all_m[1], 
              mean_all_m[3] - mean_all_m[2],  
              mean_all_m[3] - mean_all_m[1])
  sd_d_m <- c(sqrt(se_all_m[2]^2 + se_all_m[1]^2), 
              sqrt(se_all_m[3]^2 + se_all_m[2]^2),  
              sqrt(se_all_m[3]^2 + se_all_m[1]^2))
  diff_y <- c(mean_all_y[2] - mean_all_y[1], 
              mean_all_y[3] - mean_all_y[2],  
              mean_all_y[3] - mean_all_y[1])
  sd_d_y <- c(sqrt(se_all_y[2]^2 + se_all_y[1]^2), 
              sqrt(se_all_y[3]^2 + se_all_y[2]^2),  
              sqrt(se_all_y[3]^2 + se_all_y[1]^2))
  
  
  diff_l <- c(diff, diff_m, diff_y)
  se_l <- c(sd_d, sd_d_m, sd_d_y)
  p_value <- 2*(1 - pnorm(abs(diff_l/se_l)))
  diff_table <- cbind(diff_l, se_l, p_value)
}

pdf("figure_2.pdf", height= 15.5, width = 6.5)
par(mfrow = c(3, 1), mar = c(6,5,5,2), oma = c(0,4,0,0))
plot(seq(1:3), mean_all, pch = 19, ylim = c(0.1,0.4), 
     xlim = c(0.5, 3.5),
     main = "All", xaxt = "n", xlab = "", ylab = "", 
     cex.axis = 2.25, cex.main = 2.5, yaxt  = "n",
     cex = 2.25, cex.lab = 2.5)
segments(seq(1:3), mean_all - 1.96*se_all, 
         seq(1:3), mean_all + 1.96*se_all, pch = 19, lwd = 3)
Axis(side = 1, at = c(1,2,3), labels = c("1st tercile", "2nd tercile", "3rd tercile"), cex.axis = 2.25)
Axis(side = 2, at = c(0.1,0.2,0.3, 0.4), labels = c("0.1", "0.2", "0.3", "0.4"), cex.axis = 2.25)
brackets(x1 = 1.1, y1  = 0.3, x2 = 1.9, y2 =  0.3, h = 0.01, type = 4)
brackets(x1 = 2.1, y1  = 0.3, x2 = 2.9, y2 =  0.3, h = 0.01, type =  4)
brackets(x1 = 1, y1  = 0.37, x2 = 3, y2 =  0.37, h = 0.01, type =  4)
# text(x = 1.5, y = 0.33, paste0("pv = ", round(p_value[1],digits=3)), cex = 1.95)
text(x = 1.5, y = 0.33, paste0("pv = 0.40"), cex = 1.95)
text(x = 2.5, y = 0.33, paste0("pv = ", round(p_value[2],2)), cex = 1.95)
text(x = 2, y = 0.40, paste0("pv = ", round(p_value[3],2)), cex = 1.95)
mtext("Excess Males", side = 1, cex = 1.75, line = 3.75)
mtext("Proportion Perceiving\nMate Competition", side = 2, cex = 1.75, line = 3.75)

plot(seq(1:3), mean_all_m, pch = 19, ylim = c(0.1,0.6),
     xlim = c(0.5, 3.5),
     main = "Male (18-44)", xaxt = "n", xlab = "", ylab = "", cex.axis = 2.25, cex.main = 2.5,
     cex = 2.25, cex.lab = 2.5)
segments(seq(1:3), mean_all_m - 1.96*se_all_m, 
         seq(1:3), mean_all_m + 1.96*se_all_m, pch = 19, lwd = 3)
Axis(side = 1, at = c(1,2,3), labels = c("1st tercile", "2nd tercile", "3rd tercile"), cex.axis = 2.25)
brackets(x1 = 1.1, y1  = 0.48, x2 = 1.9, y2 =  0.48, h = 0.01, type = 4)
brackets(x1 = 2.1, y1  = 0.48, x2 = 2.9, y2 =  0.48, h = 0.01, type =  4)
brackets(x1 = 1, y1  = 0.53, x2 = 3, y2 =  0.53, h = 0.03, type =  4)
text(x = 1.5, y = 0.51, paste0("pv = ", round(p_value[4],2)), cex = 1.95)
text(x = 2.5, y = 0.51, paste0("pv = ", round(p_value[5],2)), cex = 1.95)
# text(x = 2, y = 0.58, paste0("pv = ", round(p_value[6],2)), cex = 1.95)
text(x = 2, y = 0.58, paste0("pv = 0.00"), cex = 1.95)
mtext("Excess Males", side = 1, cex = 1.75, line = 3.75)
mtext("Proportion Perceiving\nMate Competition", side = 2, cex = 1.75, line = 3.75)


plot(seq(1:3), mean_all_y, pch = 19, ylim = c(0.1,0.75),
     xlim = c(0.5, 3.5),
     main = "Male (30 - 40)", xaxt = "n", xlab = "", ylab = "", cex.axis = 2.25, cex.main = 2.5,
     cex = 2.25, cex.lab = 2.5)
segments(seq(1:3), mean_all_y - 1.96*se_all_y, 
         seq(1:3), mean_all_y + 1.96*se_all_y, pch = 19, lwd = 3)
Axis(side = 1, at = c(1,2,3), labels = c("1st tercile", "2nd tercile", "3rd tercile"), cex.axis = 2.25)
brackets(x1 = 1.1, y1  = 0.62, x2 = 1.9, y2 =  0.62, h = 0.03, type = 4)
brackets(x1 = 2.1, y1  = 0.62, x2 = 2.9, y2 =  0.62, h = 0.03, type =  4)
brackets(x1 = 1, y1  = 0.7, x2 = 3, y2 =  0.7, h = 0.03, type =  4)
text(x = 1.5, y = 0.67, paste0("pv = ", round(p_value[7],2)),cex = 1.95)
text(x = 2.5, y = 0.67, paste0("pv = ", round(p_value[8],2)), cex = 1.95)
text(x = 2, y = 0.75, paste0("pv = 0.00"), cex = 1.95)
# text(x = 2, y = 0.75, paste0("pv = ", round(p_value[9],3)))
mtext("Excess Males", side = 1, cex = 1.75, line = 3.75)
mtext("Proportion Perceiving\nMate Competition", side = 2, cex = 1.75, line = 3.75)

dev.off()

# ############################
# Main Models (Table 1)
# ############################
lm1 <- lm(hate_violence_means ~ MateComp.cont, data=dat_use)
summary(lm1)

lm2 <- lm(hate_violence_means ~ MateComp.cont + JobComp.cont + LifeSatis.cont, data=dat_use)
summary(lm2)

lm3 <- lm(hate_violence_means ~ MateComp.cont + JobComp.cont + LifeSatis.cont + 
            factor(age_group) +     # age group
            factor(gender) +     # gender 
            factor(state) +     # state  
            factor(citizenship) +    # german citizen
            factor(marital) +    # marital status
            factor(religion) +    # religious affiliation
            eduyrs +    # education
            factor(occupation) +    # main activity
            factor(income) +   # income
            factor(household_size) +   # household size
            factor(self_econ),    # subjective social status
          data=dat_use)
summary(lm3)

lm4 <- lm(hate_violence_means ~ MateComp.cont + JobComp.cont + LifeSatis.cont + 
            factor(age_group) +   # age group
            factor(gender) +   # gender 
            factor(state) +   # state  
            factor(citizenship) +  # german citizen
            factor(marital) +  # marital status
            factor(religion) +  # religious affiliation
            eduyrs +  # education
            factor(occupation) +  # main activity
            factor(income) + # income
            factor(household_size) + # household size
            factor(self_econ) + # subjective social status
            factor(ref_integrating) + # Refugee Index (National-level; Q73) 8 in total
            factor(ref_citizenship) + factor(ref_reduce) + factor(ref_moredone) + factor(ref_cultgiveup) + 
            factor(ref_economy) + factor(ref_crime) + factor(ref_terror),
          data=dat_use)
summary(lm4)

lm5 <- lm(hate_violence_means ~ MateComp.cont + JobComp.cont + LifeSatis.cont + 
            factor(age_group) +      # age group
            factor(gender) +      # gender 
            factor(state) +      # state  
            factor(citizenship) +     # german citizen
            factor(marital) +     # marital status
            factor(religion) +     # religious affiliation
            eduyrs + # education
            factor(occupation) +     # main activity
            factor(income) +    # income
            factor(household_size) +    # household size
            factor(self_econ) +    # subjective social status
            factor(ref_integrating) + # Refugee Index (National-level; Q73) 8 in total
            factor(ref_citizenship) + factor(ref_reduce) + factor(ref_moredone) + factor(ref_cultgiveup) + 
            factor(ref_economy) + factor(ref_crime) + factor(ref_terror) + 
            factor(ref_loc_services) +    # Refugee Index (Local, Q75)
            factor(ref_loc_economy) + factor(ref_loc_crime) + factor(ref_loc_culture) + factor(ref_loc_islam) + 
            factor(ref_loc_schools) + factor(ref_loc_housing) + factor(ref_loc_wayoflife), ## end
          data=dat_use)
summary(lm5)


# Add More Variables 
# lrscale  Q21  Left-Right Scale
# afd, Q23  Closeness to AfD
# muslim_ind, afd_ind, contact_ind
# distance_ref Q71. Distance to refugee reception centers
# settle_ref Q72. Settlement of refugees living in area

formula.5 <- 
  as.character("hate_violence_means ~ MateComp.cont + JobComp.cont + 
               LifeSatis.cont +  factor(age_group) + factor(gender) + 
               factor(state) + factor(citizenship) + factor(marital) + 
               factor(religion) + eduyrs + factor(occupation) + 
               factor(income) + factor(household_size) + factor(self_econ) + 
               factor(ref_integrating) + factor(ref_citizenship) + factor(ref_reduce) + 
               factor(ref_moredone) + factor(ref_cultgiveup) + 
               factor(ref_economy) + factor(ref_crime) + factor(ref_terror)  + 
               factor(ref_loc_services) +  factor(ref_loc_economy) + factor(ref_loc_crime) + 
               factor(ref_loc_culture) + factor(ref_loc_islam) + 
               factor(ref_loc_schools) + factor(ref_loc_housing) + factor(ref_loc_wayoflife)")

formula.6 <- paste(formula.5, "factor(distance_ref) + factor(settle_ref)", 
                   "lrscale + afd + muslim_ind + afd_ind + contact_ind", 
                   sep="+", collapse="+") 

lm6 <- lm(as.formula(formula.6), data=dat_use)
summary(lm6)

lm.list.table1 <- list(lm1, lm2, lm3, lm4, lm5, lm6)

# Table 1
star_out(stargazer(lm.list.table1,
                   covariate.labels = c("Mate Competition","Job Competition","Life Satisfaction"),
                   keep=c("MateComp.cont","JobComp.cont","LifeSatis.cont"),
                   star.char = c("\\dagger", "*", "**"),
                   notes = c("$^{\\dagger}$ p$<$0.1; $^{*}$ p$<$0.05; $^{**}$ p$<$0.01"), notes.append = FALSE), 
         name = "table1.tex")

## #################
## Figure 4
## #################
# with Difference Outcomes 
# hate_pol_message (v_320): "82. Support for Hate Crime_Attacks against refugee homes are somet" 
# hate_prevent_settlement (v_319): "82. Support for Hate Crime_Racist violence is defensible if it lea"
# hate_polcondemn (v_316): "82. Support for Hate Crime_Politicians should condemn attacks agai" 
# hate_justified (v_315): "82. Support for Hate Crime_Hostility against foreigners is sometimes justified" 

formula.7.means   <- paste("hate_violence_means ~ ", as.character(as.formula(formula.6))[3], sep = "")
formula.7.message <- paste("hate_pol_message ~", as.character(as.formula(formula.6))[3], sep = "")
formula.7.prevent <- paste("hate_prevent_settlement ~", as.character(as.formula(formula.6))[3], sep = "")
formula.7.condemn <- paste("hate_polcondemn ~ ", as.character(as.formula(formula.6))[3], sep = "")
formula.7.justified <- paste("hate_justified ~ ", as.character(as.formula(formula.6))[3], sep = "")

# output
lm7.means <- lm(as.formula(formula.7.means), data=dat_use)
lm7.justified <- lm(as.formula(formula.7.justified), data=dat_use)
lm7.message <- lm(as.formula(formula.7.message), data=dat_use)
lm7.prevent <- lm(as.formula(formula.7.prevent), data=dat_use)
lm7.condemn <- lm(as.formula(formula.7.condemn), data=dat_use)

# Figure 5
point <- c(coef(lm7.means)["MateComp.cont"],
           coef(lm7.justified)["MateComp.cont"], coef(lm7.message)["MateComp.cont"], 
           coef(lm7.prevent)["MateComp.cont"], coef(lm7.condemn)["MateComp.cont"])

se <- c(summary(lm7.means)$coef["MateComp.cont", 2],
        summary(lm7.justified)$coef["MateComp.cont", 2], summary(lm7.message)$coef["MateComp.cont", 2],
        summary(lm7.prevent)$coef["MateComp.cont", 2], summary(lm7.condemn)$coef["MateComp.cont", 2])


pdf("figure_4.pdf", height = 4, width = 8)
par(mar = c(2,4,4,1))
plot(seq(1:5), point, pch = 19, ylim = c(-0.05, 0.25), xlim = c(0.5, 5.5), 
     xlab = "", xaxt = "n", ylab = "Estimated Effects", 
     main = "Estimated Effects of Mate Competition", cex.lab = 1.25, cex.axis = 1.25, cex.main = 1.5)
segments(seq(1:5), point - 1.96*se, 
         seq(1:5), point + 1.96*se, lwd = 2)
Axis(side=1, at = seq(1:5), labels = c("Only Means", "Justified", "Message", 
                                       "Prevent", "Condemn"), cex.axis = 1.25)
abline(h =0, lty = 2)
dev.off()

## Table C.5 (in Appendix C.5)
# lm.list_d <- list(lm7.means, lm7.justified, lm7.message, lm7.prevent, lm7.condemn)
# stargazer(lm.list_d,
#           covariate.labels = c("Mate Competition","Job Competition","Life Satisfaction"),
#           keep=c("MateComp.cont","JobComp.cont","LifeSatis.cont"))


## #############################
## Figure 3: List Experiment
## #############################
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

## Difference-in-Means
## with Mean = 0.15401 sd = 0.03358
diff.in.means.results2 <- ictreg(outcome_list ~ 1, data = data.list.u2,
                                 treat = "List.treat", J=3, method = "lm")
summary(diff.in.means.results2)

## Compare to All People who answered Direct Question (n = 2170)
data.u2.all.direct <- data.u2[is.na(data.u2$hate_violence_means) == FALSE, ]
data.u2.all.direct$hate.direct.bin <- ifelse(data.u2.all.direct$hate_violence_means >=3, 1, 0)
point_dir2 <- mean(data.u2.all.direct$hate.direct.bin) ## 0.181 
se_dir2 <- sd(data.u2.all.direct$hate.direct.bin)/sqrt(length(data.u2.all.direct$hate.direct.bin)) # 0.0083

# Compare Questions within Wave 2 
# Direct Questions
data.u2$message.bin <- ifelse(data.u2$hate_pol_message >= 3, 1, 0)
data.u2$condemn.bin <- ifelse(data.u2$hate_polcondemn >= 3, 1, 0)
data.u2$justified.bin <- ifelse(data.u2$hate_justified >= 3, 1, 0)

message.mean2 <- mean(data.u2$message.bin)
condemn.mean2 <- mean(data.u2$condemn.bin)
justified.mean2 <- mean(data.u2$justified.bin)
message.se2 <- sd(data.u2$message.bin)/sqrt(length(data.u2$message.bin)) # 0.0070
condemn.se2 <- sd(data.u2$condemn.bin)/sqrt(length(data.u2$condemn.bin)) # 0.0079
justified.se2 <- sd(data.u2$justified.bin)/sqrt(length(data.u2$justified.bin)) # 0.0074

# plot
point <- c(summary(diff.in.means.results2)$par.treat, point_dir2, justified.mean2, message.mean2, condemn.mean2)
se_p  <- c(summary(diff.in.means.results2)$se.treat,  se_dir2, justified.se2, message.se2, condemn.se2)
base <- barplot(point, ylim = c(0, 0.20))
bar_name_u <- c("Only Means\n(List)","Only Means\n(Direct)", "Justified", "Message", "Condemn")
bar_name <- rep("",5)

pdf("figure_3.pdf", height = 4.5, width = 8)
par(mar = c(4, 5, 2, 1))
barplot(point, ylim = c(0, 0.3), names.arg = bar_name, 
        col = c(adjustcolor("red", 0.4), "gray", "gray", "gray", "gray"), cex.axis = 1.3,
        ylab = "Proportion of respondents", cex.lab = 1.45)
arrows(base[,1], point - 1.96*se_p, base[,1], point + 1.96*se_p, 
       lwd = 3, angle = 90, length = 0.05, code = 3,
       col = c("red", "black", "black", "black", "black"))
mtext(bar_name_u[1], outer = FALSE, side = 1, at = base[1], cex = 1.2, line = 2.4)
mtext(bar_name_u[2], outer = FALSE, side = 1, at = base[2], cex = 1.2, line = 2.4)
mtext(bar_name_u[3], outer = FALSE, side = 1, at = base[3], cex = 1.2, line = 2.4)
mtext(bar_name_u[4], outer = FALSE, side = 1, at = base[4], cex = 1.2, line = 2.4)
mtext(bar_name_u[5], outer = FALSE, side = 1, at = base[5], cex = 1.2, line = 2.4)
text(x = base[1], y = 0.275, "Estimate from \nList Experiment", col = "red", font = 2)
text(x = (base[3] + base[4])/2, y = 0.275, "Direct Questions", font = 2)
dev.off()