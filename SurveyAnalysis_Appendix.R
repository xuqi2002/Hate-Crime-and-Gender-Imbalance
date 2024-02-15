# Replication File for Appendix: Survey Analysis
# Appendix D2 Figure D2: Replicate Figure 3 with only among anti-refugees
# Appendix D3 Figure D3: Variables Predicting Mate Competition vs. Other Views About Refugees
# Appendix D4 Figures D.4.1 and D.4.2: Replicate Figure 4 with wave 1
# Appendix D5 Table D.5: Table representation of Figure 5
# Appendix D6 Table D.6.1, Figure.6.2, Table.D.6.3, Table.D.6.4
# Appendix D8 Table D.8.1: Robustness Check with YouGov Survey Data

# R version 4.0.2 (2020-06-22)

# ##################
# Data Preparation
# ##################
rm(list=ls())
# install.packages("readstata13")  #  readstata13_0.9.2
# install.packages("MASS") # MASS_7.3-51.6  
# install.packages("sandwich")  # sandwich_2.5-1 
# install.packages("lmtest") # lmtest_0.9-37 
# install.packages("stargazer") # stargazer_5.2.2
# install.packages("foreign") # foreign_0.8-80
# install.packages("list") # list_9.2


require(readstata13)  #  readstata13_0.9.2
require(MASS) # MASS_7.3-51.6
require(sandwich)  # sandwich_2.5-1
require(lmtest) # lmtest_0.9-37
require(stargazer) # stargazer_5.2.2
require(foreign) # foreign_0.8-80
require(list) # list_9.2
source("Help.R")

dat <- read.dta13(file =  "survey.dta")

# Subset to people who pass the check
dat_use <- dat[dat$wave == 4, ]

## ###############################
## 1: Appendix D2 Figure D2
## ###############################
# Replicate only among anti-refugee
quantile(dat_use$refugee_ind, probs = 0.75)

dat_use_r <- dat_use[dat_use$refugee_ind > 0.875, ]
dat_use_r$MateComp.cont_bin <- ifelse(dat_use_r$MateComp.cont >= 3, 1, 0)
dat_use_r$excess_c <- ifelse(dat_use_r$pop_15_44_muni_gendergap_2015 < 1.04, "1",
                             ifelse(dat_use_r$pop_15_44_muni_gendergap_2015 < 1.12, "2", "3"))
dat_male_r <- dat_use_r[dat_use_r$gender == "Male" & dat_use_r$age <= 44 & dat_use_r$age >= 18, ]
dat_male_y_r <- dat_use_r[dat_use_r$gender == "Male" & dat_use_r$age <= 40 & dat_use_r$age >= 30, ]

mean_all_r <- tapply(dat_use_r$MateComp.cont_bin, dat_use_r$excess_c, mean)
se_all_r   <- tapply(dat_use_r$MateComp.cont_bin, dat_use_r$excess_c, sd)/sqrt(table(dat_use_r$excess_c))

mean_all_m_r <- tapply(dat_male_r$MateComp.cont_bin, dat_male_r$excess_c, mean)
se_all_m_r <- tapply(dat_male_r$MateComp.cont_bin, dat_male_r$excess_c, sd)/sqrt(table(dat_male_r$excess_c))

mean_all_y_r <- tapply(dat_male_y_r$MateComp.cont_bin, dat_male_y_r$excess_c, mean)
se_all_y_r <- tapply(dat_male_y_r$MateComp.cont_bin, dat_male_y_r$excess_c, sd)/sqrt(table(dat_male_y_r$excess_c))

pdf("figure_D2.pdf", height= 6, width = 17.5)
par(mfrow = c(1, 3), mar = c(2,2,3,2), oma = c(4,4,0,0))
plot(seq(1:3), mean_all_r, pch = 19, ylim = c(0, 1),
     xlim = c(0.5, 3.5),
     main = "All", xaxt = "n", xlab = "", ylab = "", cex.axis = 2.25, cex.main = 2.5,
     cex = 2.25, cex.lab = 2.5)
segments(seq(1:3), mean_all_r - 1.96*se_all_r,
         seq(1:3), mean_all_r + 1.96*se_all_r, pch = 19, lwd = 3)
Axis(side = 1, at = c(1,2,3), labels = c("1st tercile", "2nd tercile", "3rd tercile"), cex.axis = 2.25)

plot(seq(1:3), mean_all_m_r, pch = 19, ylim = c(0, 1),
     xlim = c(0.5, 3.5),
     main = "Male (18-44)", xaxt = "n", xlab = "", ylab = "", cex.axis = 2.25, cex.main = 2.5,
     cex = 2.25, cex.lab = 2.5)
segments(seq(1:3), mean_all_m_r - 1.96*se_all_m_r,
         seq(1:3), mean_all_m_r + 1.96*se_all_m_r, pch = 19, lwd = 3)
Axis(side = 1, at = c(1,2,3), labels = c("1st tercile", "2nd tercile", "3rd tercile"), cex.axis = 2.25)

plot(seq(1:3), mean_all_y_r, pch = 19, ylim = c(0, 1),
     xlim = c(0.5, 3.5),
     main = "Male (30 - 40)", xaxt = "n", xlab = "", ylab = "", cex.axis = 2.25, cex.main = 2.5,
     cex = 2.25, cex.lab = 2.5)
segments(seq(1:3), mean_all_y_r - 1.96*se_all_y_r,
         seq(1:3), mean_all_y_r + 1.96*se_all_y_r, pch = 19, lwd = 3)
Axis(side = 1, at = c(1,2,3), labels = c("1st tercile", "2nd tercile", "3rd tercile"), cex.axis = 2.25)
mtext("Proportion Perceiving Mate Competition", side = 2, outer = TRUE, at = 0.5,
      cex = 1.5, line = 1.75)
mtext("Excess Males", side = 1, outer = TRUE, at = 0.175,
      cex = 1.5, line = 1.75)
mtext("Excess Males", side = 1, outer = TRUE, at = 0.5,
      cex = 1.5, line = 1.75)
mtext("Excess Males", side = 1, outer = TRUE, at = 0.825,
      cex = 1.5, line = 1.75)
dev.off()


## ###############################
## 2: Appendix D3 Figure D3
## ###############################
# Coefficients of Male x Single on Refugee Variables

rm(list=ls())
dat <- read.dta13(file =  "survey.dta")
dat_use <- dat[dat$wave == 4, ]
source("Help.R")
dat_use$male <- as.numeric(dat_use$gender == "Male")

# outcomes we want to analyze
outcome_ref <- c("MateComp.cont", "JobComp.cont", "ref_integrating",
                 "ref_citizenship","ref_reduce","ref_moredone", "ref_cultgiveup",
                 "ref_economy", "ref_crime", "ref_terror", "ref_loc_services",
                 "ref_loc_economy", "ref_loc_crime", "ref_loc_culture",
                 "ref_loc_islam", "ref_loc_schools", "ref_loc_housing", "ref_loc_wayoflife")

outcome_ref_name <- c("Mate competition", "Job competition", "Integration",
                      "Citizenship for refugees","Number of refugees","More for refugees",
                      "Culture",
                      "Economy", "Crime", "Terrorism", "Local social services",
                      "Local economy", "Local crime", "Local culture",
                      "Islam", "Local school", "Housing", "Living")

# Fit Ordered Logit
lm_l <- list()
lm_out <- list()
male_mat <- sing_mat <- int_mat <- matrix(NA, nrow = 18, ncol = 2)
for(i in 1:18){
  control <- paste(outcome_ref[-i], collapse = "+")
  for_i <- paste("as.factor(", outcome_ref[i],")", "~ male*singdivsep + ", control, sep = "")
  lm_l[[i]]  <- polr(for_i, data = dat_use, Hess = TRUE)
  lm_out[[i]] <- summary(lm_l[[i]])$coef
  male_mat[i, 1:2] <- summary(lm_l[[i]])$coef["male", 1:2]
  sing_mat[i, 1:2] <- summary(lm_l[[i]])$coef["singdivsep", 1:2]
  int_mat[i, 1:2] <- summary(lm_l[[i]])$coef["male:singdivsep", 1:2]
}
rownames(int_mat) <- outcome_ref

# Fit linear regression
lm2_l <- list()
lm2_out <- list()
male_mat2 <- sing_mat2 <- int_mat2 <- matrix(NA, nrow = 18, ncol = 2)
for(i in 1:18){
  control <- paste(outcome_ref[-i], collapse = "+")
  for_i <- paste(outcome_ref[i], "~ male*singdivsep + ", control, sep = "")
  lm2_l[[i]]  <- lm(for_i, data = dat_use)
  lm2_out[[i]] <- summary(lm2_l[[i]])$coef
  male_mat2[i, 1:2] <- summary(lm2_l[[i]])$coef["male", 1:2]
  sing_mat2[i, 1:2] <- summary(lm2_l[[i]])$coef["singdivsep", 1:2]
  int_mat2[i, 1:2] <- summary(lm2_l[[i]])$coef["male:singdivsep", 1:2]
}
rownames(int_mat2) <- outcome_ref


# Show Coefficients for Male x Single Interaction (after controlling for other refugee variables)
# Both Ordered Logit and Linear regression
col_p <-  rev(c("red", rep("black", 17)))

pdf("figure_D3_1.pdf", height = 6, width = 8)
par(mfrow = c(1, 2), mar = c(4, 2, 4, 1), oma = c(1, 10, 2, 2))
plot(rev(int_mat[,1]), seq(1:18), pch = 19, xlim = c(-0.6, 1.0), ylim = c(1, 18),
     xlab = "Coefficients", ylab = "", yaxt = "n",
     main = "Ordered logit", col = col_p)
segments(rev(int_mat[,1]) - 1.96*rev(int_mat[,2]), seq(1:18),
         rev(int_mat[,1]) + 1.96*rev(int_mat[,2]), seq(1:18), col = col_p)
abline(v = 0, lty = 2)

plot(rev(int_mat2[,1]), seq(1:18), pch = 19, xlim = c(-0.3, 0.3), ylim = c(1, 18),
     xlab = "Coefficients", ylab = "", yaxt = "n",
     main = "Linear regression", col = col_p)
segments(rev(int_mat2[,1]) - 1.96*rev(int_mat2[,2]), seq(1:18),
         rev(int_mat2[,1]) + 1.96*rev(int_mat2[,2]), seq(1:18), col = col_p)
abline(v = 0, lty = 2)

Axis(side = 2, at = seq(1:18), labels = rev(outcome_ref_name), las = 1, tick = 0,
     outer  = TRUE, hadj = 0, line = 7.5)
mtext(side = 3, at = 0.5, text = "Coefficients of Male x Single", cex = 1.5, font = 2, outer = TRUE)
dev.off()

# ######################################
# Coefficients of Women's Role on Mate Competition
# ######################################
# Ordered Logit
lm_l <- list()
lm_out <- list()
role_mat <- matrix(NA, nrow = 18, ncol = 2)
for(i in 1:18){
  control <- paste(outcome_ref[-i], collapse = "+")
  for_i <- paste("as.factor(", outcome_ref[i], ")", "~ women_role + ", control, sep = "")
  lm_l[[i]]  <- polr(for_i, data = dat_use, Hess = TRUE)
  lm_out[[i]] <- summary(lm_l[[i]])$coef
  role_mat[i, 1:2] <- summary(lm_l[[i]])$coef["women_role", 1:2]
}
rownames(role_mat) <- outcome_ref

# OLS
lm_l2 <- list()
lm_out2 <- list()
role_mat2 <- matrix(NA, nrow = 18, ncol = 2)
for(i in 1:18){
  control <- paste(outcome_ref[-i], collapse = "+")
  for_i <- paste(outcome_ref[i], "~ women_role + ", control, sep = "")
  lm_l2[[i]]  <- lm(for_i, data = dat_use)
  lm_out2[[i]] <- summary(lm_l2[[i]])$coef
  role_mat2[i, 1:2] <- summary(lm_l2[[i]])$coef["women_role", 1:2]
}
rownames(role_mat2) <- outcome_ref

pdf("figure_D3_2.pdf", height = 6, width = 8)
par(mfrow = c(1, 2), mar = c(4, 2, 4, 1), oma = c(1, 10, 2, 2))

plot(rev(role_mat[,1]), seq(1:18), pch = 19, xlim = c(-0.3, 0.6), ylim = c(1, 18),
     xlab = "Coefficients", ylab = "", yaxt = "n",
     main = "Ordered logit", col = col_p)
segments(rev(role_mat[,1]) - 1.96*rev(role_mat[,2]), seq(1:18),
         rev(role_mat[,1]) + 1.96*rev(role_mat[,2]), seq(1:18), col = col_p)
abline(v = 0, lty = 2)

plot(rev(role_mat2[,1]), seq(1:18), pch = 19, xlim = c(-0.1, 0.15), ylim = c(1, 18),
     xlab = "Coefficients", ylab = "", yaxt = "n",
     main = "Linear regression", col = col_p)
segments(rev(role_mat2[,1]) - 1.96*rev(role_mat2[,2]), seq(1:18),
         rev(role_mat2[,1]) + 1.96*rev(role_mat2[,2]), seq(1:18), col = col_p)
abline(v = 0, lty = 2)

Axis(side = 2, at = seq(1:18), labels = rev(outcome_ref_name), las = 1, tick = 0,
     outer  = TRUE, hadj = 0, line = 7.5)

mtext(side = 3, at = 0.5, text = "Coefficients of Women's Role",
      cex = 1.5, font = 2, outer = TRUE)
dev.off()


## ###################################
## Appendix D4: Figure D.4.1 & D.4.2
## ###################################
# Replicate Figure 3 with Wave 1
data.u1 <- dat[dat$wave == 1, ]

data.u1$List.treat <- ifelse(data.u1$treatment_list == "Scenario 2", 1, 0)

# Difference-in-Means (0.12618)
# Message (hate_pol_message):
# Attacks against refugee homes are sometimes necessary to make it clear to politicians that we have a refugee problem
diff.in.means.results <- ictreg(outcome_list ~ 1, data = data.u1,
                                treat = "List.treat", J = 3, method = "lm")
summary(diff.in.means.results)

data.u1$means_bin <- ifelse(data.u1$hate_violence_means >= 3, 1, 0)
data.u1$condemn_bin <- ifelse(data.u1$hate_polcondemn >= 3, 1, 0)
data.u1$justified_bin <- ifelse(data.u1$hate_justified >= 3, 1, 0)

only.mean <- mean(data.u1$means_bin)
condemn.mean <- mean(data.u1$condemn_bin)
justified.mean <- mean(data.u1$justified_bin)

only.se <- sd(data.u1$means_bin)/sqrt(length(data.u1$means_bin))
condemn.se <- sd(data.u1$condemn_bin)/sqrt(length(data.u1$condemn_bin))
justified.se <- sd(data.u1$justified_bin)/sqrt(length(data.u1$justified_bin))

# plot different questions within the same wave
point <- c(summary(diff.in.means.results)$par.treat, only.mean, condemn.mean, justified.mean)
se_p  <- c(summary(diff.in.means.results)$se.treat,  only.se, condemn.se, justified.se)
base <- barplot(point, ylim = c(0, 0.20))
bar_name_u <- c("Message (List)", "Only Means", "Condemn", "Justified")
bar_name <- rep("",4)

# Figure D.4.1
pdf("figure_D4_1.pdf", height = 4.5, width = 8)
par(mar = c(4, 5, 2, 1))
barplot(point, ylim = c(0, 0.3), names.arg = bar_name,
        col = c(adjustcolor("red", 0.4), "gray", "gray", "gray"), cex.axis = 1.3)
arrows(base[,1], point - 1.96*se_p, base[,1], point + 1.96*se_p,
       lwd = 3, angle = 90, length = 0.05, code = 3,
       col = c("red", "black", "black", "black"))
mtext(bar_name_u[1], outer = FALSE, side = 1, at = base[1], cex = 1.2, line = 2.4)
mtext(bar_name_u[2], outer = FALSE, side = 1, at = base[2], cex = 1.2, line = 2.4)
mtext(bar_name_u[3], outer = FALSE, side = 1, at = base[3], cex = 1.2, line = 2.4)
mtext(bar_name_u[4], outer = FALSE, side = 1, at = base[4], cex = 1.2, line = 2.4)
text(x = base[1], y = 0.28, "Estimate from \nList Experiment", col = "red", font = 2)
text(x = base[3], y = 0.28, "Direct Questions", font = 2)
dev.off()

## "Message" across Waves
data.u1 <- dat[dat$wave == 1, ]
data.u2 <- dat[dat$wave == 2, ]
data.u3 <- dat[dat$wave == 3, ]
data.u4 <- dat[dat$wave == 4, ]
dat_all <- rbind(data.u1, data.u2, data.u3, data.u4)

dat_all$hate_pol_message_bin <- ifelse(dat_all$hate_pol_message >=3, 1, 0)
message_direct <- tapply(dat_all$hate_pol_message_bin, dat_all$wave, mean, na.rm = TRUE)[c(2,3,4)]
message_direct_num <- tapply(dat_all$hate_pol_message_bin, dat_all$wave, function(x) sum(is.na(x)==FALSE))[c(2,3,4)]
message_direct_se <- tapply(dat_all$hate_pol_message_bin, dat_all$wave, sd, na.rm = TRUE)[c(2,3,4)]/sqrt(message_direct_num)

# plot The same question over time
point <- c(summary(diff.in.means.results)$par.treat, message_direct)
se_p  <- c(summary(diff.in.means.results)$se.treat, message_direct_se)
base <- barplot(point, ylim = c(0, 0.20))
bar_name_u <- c("Message \n(List)", "Message \n(Direct, Wave 2)",
                "Message \n(Direct, Wave 3)", "Message \n(Direct, Wave 4)")
bar_name <- rep("",4)

# Figure D.4.2
pdf("figure_D4_2.pdf", height = 4.5, width = 8)
par(mar = c(4, 5, 2, 1))
barplot(point, ylim = c(0, 0.25), names.arg = bar_name,
        col = c(adjustcolor("red", 0.4), "gray", "gray", "gray"), cex.axis = 1.3,
        ylab = "Proportion of respondents", cex.lab = 1.45)
arrows(base[,1], point - 1.96*se_p, base[,1], point + 1.96*se_p,
       lwd = 3, angle = 90, length = 0.05, code = 3,
       col = c("red", "black", "black", "black"))
mtext(bar_name_u[1], outer = FALSE, side = 1, at = base[1], cex = 1.2, line = 2.4)
mtext(bar_name_u[2], outer = FALSE, side = 1, at = base[2], cex = 1.2, line = 2.4)
mtext(bar_name_u[3], outer = FALSE, side = 1, at = base[3], cex = 1.2, line = 2.4)
mtext(bar_name_u[4], outer = FALSE, side = 1, at = base[4], cex = 1.2, line = 2.4)
text(x = base[1], y = 0.225, "Estimate from \nList Experiment", col = "red", font = 2)
text(x = base[3], y = 0.225, "Direct Questions", font = 2)
dev.off()


# #############################
# Appendix D5 Table D5
# #############################
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

# with Difference Outcomes
# hate_pol_message : "82. Support for Hate Crime_Attacks against refugee homes are somet"
# hate_prevent_settlement : "82. Support for Hate Crime_Racist violence is defensible if it lea"
# hate_polcondemn : "82. Support for Hate Crime_Politicians should condemn attacks agai"
# hate_justified: "82. Support for Hate Crime_Hostility against foreigners is sometimes justified"

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

## Table D.5 (in Appendix D.5)
lm.list_d <- list(lm7.means, lm7.justified, lm7.message, lm7.prevent, lm7.condemn)
star_out(stargazer(lm.list_d,
                   covariate.labels = c("Mate Competition","Job Competition","Life Satisfaction"),
                   keep=c("MateComp.cont","JobComp.cont","LifeSatis.cont")),
         name = "table_D5_1.tex")

## ##################################
## Table D.5.2 (appendix) with East/West
## ##################################
rm(list=ls())
# Set the directly appropriately

dat <- read.dta13(file =  "survey.dta")
source("Help.R")

# Subset to wave 4
dat_use <- dat[dat$wave == 4, ]
{
  dat_use$west <- 1 - dat_use$east

  #  remove state
  formula.5_int <-
    as.character("hate_violence_means ~ MateComp.cont*west + JobComp.cont +
               LifeSatis.cont +  factor(age_group) + factor(gender) +
               factor(citizenship) + factor(marital) +
               factor(religion) + eduyrs + factor(occupation) +
               factor(income) + factor(household_size) + factor(self_econ) +
               factor(ref_integrating) + factor(ref_citizenship) + factor(ref_reduce) +
               factor(ref_moredone) + factor(ref_cultgiveup) +
               factor(ref_economy) + factor(ref_crime) + factor(ref_terror)  +
               factor(ref_loc_services) +  factor(ref_loc_economy) + factor(ref_loc_crime) +
               factor(ref_loc_culture) + factor(ref_loc_islam) +
               factor(ref_loc_schools) + factor(ref_loc_housing) + factor(ref_loc_wayoflife)")

  formula.6_int <- paste(formula.5_int, "factor(distance_ref) + factor(settle_ref)",
                         "lrscale + afd + muslim_ind + afd_ind + contact_ind",
                         sep="+", collapse="+")

  ## Interaction with East/West
  # with Difference Outcomes
  # hate_pol_message: "82. Support for Hate Crime_Attacks against refugee homes are somet"
  # hate_prevent_settlement: "82. Support for Hate Crime_Racist violence is defensible if it lea"
  # hate_polcondemn: "82. Support for Hate Crime_Politicians should condemn attacks agai"
  # hate_justified: "82. Support for Hate Crime_Hostility against foreigners is sometimes justified"

  formula.7_int.means   <- paste("hate_violence_means ~ ",
                                 as.character(as.formula(formula.6_int))[3], sep = "")
  formula.7_int.message <- paste("hate_pol_message ~",
                                 as.character(as.formula(formula.6_int))[3], sep = "")
  formula.7_int.prevent <- paste("hate_prevent_settlement ~",
                                 as.character(as.formula(formula.6_int))[3], sep = "")
  formula.7_int.condemn <- paste("hate_polcondemn ~ ",
                                 as.character(as.formula(formula.6_int))[3], sep = "")
  formula.7_int.justified <- paste("hate_justified ~ ",
                                   as.character(as.formula(formula.6_int))[3], sep = "")

  # output
  lm7_int.means <- lm(as.formula(formula.7_int.means), data = dat_use)
  lm7_int.justified <- lm(as.formula(formula.7_int.justified), data=dat_use)
  lm7_int.message <- lm(as.formula(formula.7_int.message), data=dat_use)
  lm7_int.prevent <- lm(as.formula(formula.7_int.prevent), data=dat_use)
  lm7_int.condemn <- lm(as.formula(formula.7_int.condemn), data=dat_use)

  ## Table D.5.2 (in Appendix D.5)
  lm.list_int <- list(lm7_int.means, lm7_int.justified, lm7_int.message, lm7_int.prevent, lm7_int.condemn)
  star_out(stargazer(lm.list_int,
                     covariate.labels = c("Mate Competition",
                                          "West",
                                          "Job Competition","Life Satisfaction",
                                          "Mate Competition x West"),
                     keep=c("MateComp.cont",  "west",
                            "JobComp.cont","LifeSatis.cont",
                            "MateComp.cont:west")),
           name  = "table_D5_2.tex")
}

# ##########################################
# Appendix D6: Replcate Results with Men
# ##########################################
rm(list=ls())
# Set the directly appropriately

dat <- read.dta13(file =  "survey.dta")
source("Help.R")

# Subset to wave 4
dat_use <- dat[dat$wave == 4, ]
dat_male <- dat_use[dat_use$gender == "Male",]
dat_female <- dat_use[dat_use$gender == "Female",]

# ##########################################
# Table D.6.1: Replicate Main Models
# ##########################################
{
  
  lm1 <- lm(hate_violence_means ~ MateComp.cont, data=dat_male)
  
  lm2 <- lm(hate_violence_means ~ MateComp.cont + JobComp.cont + LifeSatis.cont, data=dat_male)
  
  lm3 <- lm(hate_violence_means ~ MateComp.cont + JobComp.cont + LifeSatis.cont + 
              factor(age_group) +     # age group
              factor(state) +     # state  
              factor(citizenship) +    # german citizen
              factor(marital) +    # marital status
              factor(religion) +    # religious affiliation
              eduyrs +    # education
              factor(occupation) +    # main activity
              factor(income) +   # income
              factor(household_size) +   # household size
              factor(self_econ),    # subjective social status
            data=dat_male)
  
  lm4 <- lm(hate_violence_means ~ MateComp.cont + JobComp.cont + LifeSatis.cont + 
              factor(age_group) +   # age group
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
            data=dat_male)
  
  lm5 <- lm(hate_violence_means ~ MateComp.cont + JobComp.cont + LifeSatis.cont + 
              factor(age_group) +      # age group
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
            data=dat_male)
  
  
  # Add More Variables 
  # lrscale  Q21  Left-Right Scale
  # afd, Q23  Closeness to AfD
  # muslim_ind, afd_ind, contact_ind
  # distance_ref Q71. Distance to refugee reception centers
  # settle_ref Q72. Settlement of refugees living in area
  
  formula.5 <- 
    as.character("hate_violence_means ~ MateComp.cont + JobComp.cont + 
               LifeSatis.cont +  factor(age_group) + 
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
  
  lm6 <- lm(as.formula(formula.6), data=dat_male)
}
lm.list.table1 <- list(lm1, lm2, lm3, lm4, lm5, lm6)

# Table D.6.1
star_out(stargazer(lm.list.table1,
                   covariate.labels = c("Mate Competition","Job Competition","Life Satisfaction"),
                   keep=c("MateComp.cont","JobComp.cont","LifeSatis.cont")), 
         name = "table_D6_1.tex")

## ###############################################
## Figure D.6.2: Replicating Figure 4 (with Male)
## ###############################################
# with Difference Outcomes
# hate_pol_message: "82. Support for Hate Crime_Attacks against refugee homes are somet"
# hate_prevent_settlement: "82. Support for Hate Crime_Racist violence is defensible if it lea"
# hate_polcondemn: "82. Support for Hate Crime_Politicians should condemn attacks agai"
# hate_justified: "82. Support for Hate Crime_Hostility against foreigners is sometimes justified"

# without gender
formula.5 <- 
  as.character("hate_violence_means ~ MateComp.cont + JobComp.cont + 
               LifeSatis.cont +  factor(age_group) + 
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

formula.7.means   <- paste("hate_violence_means ~ ", as.character(as.formula(formula.6))[3], sep = "")
formula.7.message <- paste("hate_pol_message ~", as.character(as.formula(formula.6))[3], sep = "")
formula.7.prevent <- paste("hate_prevent_settlement ~", as.character(as.formula(formula.6))[3], sep = "")
formula.7.condemn <- paste("hate_polcondemn ~ ", as.character(as.formula(formula.6))[3], sep = "")
formula.7.justified <- paste("hate_justified ~ ", as.character(as.formula(formula.6))[3], sep = "")

# output
lm7.means <- lm(as.formula(formula.7.means), data=dat_male)
lm7.justified <- lm(as.formula(formula.7.justified), data=dat_male)
lm7.message <- lm(as.formula(formula.7.message), data=dat_male)
lm7.prevent <- lm(as.formula(formula.7.prevent), data=dat_male)
lm7.condemn <- lm(as.formula(formula.7.condemn), data=dat_male)

point <- c(coef(lm7.means)["MateComp.cont"],
           coef(lm7.justified)["MateComp.cont"], coef(lm7.message)["MateComp.cont"],
           coef(lm7.prevent)["MateComp.cont"], coef(lm7.condemn)["MateComp.cont"])

se <- c(summary(lm7.means)$coef["MateComp.cont", 2],
        summary(lm7.justified)$coef["MateComp.cont", 2], summary(lm7.message)$coef["MateComp.cont", 2],
        summary(lm7.prevent)$coef["MateComp.cont", 2], summary(lm7.condemn)$coef["MateComp.cont", 2])


pdf("figure_D6_2.pdf", height = 4, width = 8)
par(mar = c(2,4,4,1))
plot(seq(1:5), point, pch = 19, ylim = c(-0.05, 0.25), xlim = c(0.5, 5.5),
     xlab = "", xaxt = "n", ylab = "Estimated Effects",
     main = "Estimated Effects of Mate Competition (among male)", cex.lab = 1.25, cex.axis = 1.25, cex.main = 1.5)
segments(seq(1:5), point - 1.96*se,
         seq(1:5), point + 1.96*se, lwd = 2)
Axis(side=1, at = seq(1:5), labels = c("Only Means", "Justified", "Message",
                                       "Prevent", "Condemn"), cex.axis = 1.25)
abline(h =0, lty = 2)
dev.off()

## Table D.6.3
lm.list_d_m <- list(lm7.means, lm7.justified, lm7.message, lm7.prevent, lm7.condemn)
star_out(stargazer(lm.list_d_m,
                   covariate.labels = c("Mate Competition","Job Competition","Life Satisfaction"),
                   keep=c("MateComp.cont","JobComp.cont","LifeSatis.cont")),
         name  = "table_D6_3.tex")

# ##########################################
# Appendix D8, Table D8: YouGov analysis
# ##########################################
rm(list=ls())
you_data <- read.dta13(file  = "YouGov.dta")
source("Help.R")

## (1) Main Regression
lm1 <- lm(hate_cont ~ mate_compete +
            age + # age
            gender +  # gender
            factor(sta) +  #state
            factor(mstat) +  # Marital Status
            reli + # religion
            educ_aggr_rec  + # education
            hinc +  # income
            housz + # household size
            pol_leftright, # leftright scale
          data = you_data)
summary(lm1)

## (2) + Aggression Score
lm2 <- lm(hate_cont ~
            mate_compete +
            age + # age
            gender +  # gender
            factor(sta) +  #state
            factor(mstat) +  # Marital Status
            reli + # religion
            educ_aggr_rec  + # education
            hinc +  # income
            housz + # household size
            pol_leftright + # leftright scale
            angry_mean, # aggression score
          data = you_data)
summary(lm2)

## (3) + Refugee Index
lm3 <- lm(hate_cont ~
            mate_compete +
            age + # age
            gender +  # gender
            factor(sta) +  #state
            factor(mstat) +  # Marital Status
            reli + # religion
            educ_aggr_rec  + # education
            hinc +  # income
            housz + # household size
            pol_leftright + # leftright scale
            angry_mean + # aggression score
            ref_loc_services + ref_loc_economy + ref_loc_crime + ## Refugee Questions (9 Questions)
            ref_loc_culture + ref_loc_islam + ref_local_job +
            ref_loc_schools + ref_loc_housing + ref_loc_wayoflife,
          data = you_data)
summary(lm3)

## (4) + Refugee Contact
lm4 <- lm(hate_cont ~
            mate_compete +
            age + # age
            gender +  # gender
            factor(sta) +  #state
            factor(mstat) +  # Marital Status
            reli + # religion
            educ_aggr_rec  + # education
            hinc +  # income
            housz + # household size
            pol_leftright + # leftright scale
            angry_mean + # aggression score
            ref_loc_services + ref_loc_economy + ref_loc_crime + ## Refugee Questions (9 Questions)
            ref_loc_culture + ref_loc_islam + ref_local_job +
            ref_loc_schools + ref_loc_housing + ref_loc_wayoflife +
            see_ref_road + see_ref_store + see_ref_center + ## Refugee Contact (5 Questions)
            see_ref_school + see_ref_work,
          data = you_data)
summary(lm4)

## (5) + AfD Score
lm5 <- lm(hate_cont ~
            mate_compete +
            age + # age
            gender +  # gender
            factor(sta) +  #state
            factor(mstat) +  # Marital Status
            reli + # religion
            educ_aggr_rec  + # education
            hinc +  # income
            housz + # household size
            pol_leftright + # leftright scale
            angry_mean + # aggression score
            ref_loc_services + ref_loc_economy + ref_loc_crime + ## Refugee Questions (9 Questions)
            ref_loc_culture + ref_loc_islam + ref_local_job +
            ref_loc_schools + ref_loc_housing + ref_loc_wayoflife +
            see_ref_road + see_ref_store + see_ref_center + ## Refugee Contact (5 Questions)
            see_ref_school + see_ref_work +
            afd.score, # Closeness to AfD
          data = you_data)
summary(lm5)

star_out(stargazer(list(lm1, lm2, lm3, lm4, lm5),
          covariate.labels = c("Mate Competition", "Aggressiveness"), keep=c("mate_compete", "angry_mean")),
         name = "table_D8_1.tex")


rm(list=ls())
you_data <- read.dta13(file  = "YouGov.dta")
you_male <- you_data[you_data$gender == levels(you_data$gender)[1], ]
source("Help.R")

{
  ## (1) Main Regression 
  lm1 <- lm(hate_cont ~ mate_compete + 
              age + # age 
              factor(sta) +  #state 
              factor(mstat) +  # Marital Status
              reli + # religion 
              educ_aggr_rec  + # education 
              hinc +  # income 
              housz + # household size 
              pol_leftright, # leftright scale
            data = you_male)
  
  ## (2) + Aggression Score 
  lm2 <- lm(hate_cont ~ 
              mate_compete + 
              age + # age 
              factor(sta) +  #state 
              factor(mstat) +  # Marital Status
              reli + # religion 
              educ_aggr_rec  + # education 
              hinc +  # income 
              housz + # household size 
              pol_leftright + # leftright scale
              angry_mean, # aggression score 
            data = you_male)
  
  ## (3) + Refugee Index
  lm3 <- lm(hate_cont ~ 
              mate_compete + 
              age + # age 
              factor(sta) +  #state 
              factor(mstat) +  # Marital Status
              reli + # religion 
              educ_aggr_rec  + # education 
              hinc +  # income 
              housz + # household size 
              pol_leftright + # leftright scale
              angry_mean + # aggression score 
              ref_loc_services + ref_loc_economy + ref_loc_crime + ## Refugee Questions (9 Questions)
              ref_loc_culture + ref_loc_islam + ref_local_job +
              ref_loc_schools + ref_loc_housing + ref_loc_wayoflife, 
            data = you_male)
  summary(lm3)
  
  ## (4) + Refugee Contact 
  lm4 <- lm(hate_cont ~ 
              mate_compete + 
              age + # age 
              # gender +  # gender 
              factor(sta) +  #state 
              factor(mstat) +  # Marital Status
              reli + # religion 
              educ_aggr_rec  + # education 
              hinc +  # income 
              housz + # household size 
              pol_leftright + # leftright scale
              angry_mean + # aggression score 
              ref_loc_services + ref_loc_economy + ref_loc_crime + ## Refugee Questions (9 Questions)
              ref_loc_culture + ref_loc_islam + ref_local_job +
              ref_loc_schools + ref_loc_housing + ref_loc_wayoflife +
              see_ref_road + see_ref_store + see_ref_center + ## Refugee Contact (5 Questions)
              see_ref_school + see_ref_work, 
            data = you_male)
  summary(lm4)
  
  ## (5) + AfD Score 
  lm5 <- lm(hate_cont ~ 
              mate_compete + 
              age + # age 
              # gender +  # gender 
              factor(sta) +  #state 
              factor(mstat) +  # Marital Status
              reli + # religion 
              educ_aggr_rec  + # education 
              hinc +  # income 
              housz + # household size 
              pol_leftright + # leftright scale
              angry_mean + # aggression score 
              ref_loc_services + ref_loc_economy + ref_loc_crime + ## Refugee Questions (9 Questions)
              ref_loc_culture + ref_loc_islam + ref_local_job +
              ref_loc_schools + ref_loc_housing + ref_loc_wayoflife +
              see_ref_road + see_ref_store + see_ref_center + ## Refugee Contact (5 Questions)
              see_ref_school + see_ref_work + 
              afd.score, # Closeness to AfD
            data = you_male)
  summary(lm5)
}

star_out(stargazer(list(lm1, lm2, lm3, lm4, lm5),
                   covariate.labels = c("Mate Competition", "Aggressiveness"), 
                   keep=c("mate_compete", "angry_mean")),
         name = "table_D8_2.tex")
