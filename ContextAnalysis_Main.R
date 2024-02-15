# Replication  File for 
# Figure 1 (Effects of Excess Males on Prob of Hate Crime)
# Appendix: Figure C8 (Effects of Male Diadvantage on Prob of Hate Crime)

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

dat <- read.dta13("context.dta")

dat_2015 <- dat[dat$year == 2015, ]
dat_2016 <- dat[dat$year == 2016, ]
dat_2017 <- dat[dat$year == 2017, ]
dat_2015$Hate_all_muni_1517 <- dat_2015$Hate_all_muni + dat_2016$Hate_all_muni + dat_2017$Hate_all_muni
dat_2015$Hate_all_muni_1517_bin <- ifelse(dat_2015$Hate_all_muni_1517 > 0, 1, 0)

## #########################
## Main Figure (Figure 1)
## #########################
# Remove Extreme Value of Excess Males 
range_x <- quantile(dat_2015$pop_15_44_muni_gendergap_2015, c(0.025, 0.975), na.rm = TRUE)
dat_2015_s <- dat_2015[dat_2015$pop_15_44_muni_gendergap_2015 >= range_x[1] & 
                         dat_2015$pop_15_44_muni_gendergap_2015 <= range_x[2], ]
dat_s <- dat[dat$pop_15_44_muni_gendergap_2015 >= range_x[1] & 
               dat$pop_15_44_muni_gendergap_2015 <= range_x[2], ]

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

# Dose function
bin_1_sum_dose <- marginal_effect(bin_1_sum,
                                  newdata = dat_2015_s, family = "logit",
                                  main_var = "pop_15_44_muni_gendergap_2015")

bin_1_p_dose <- marginal_effect(bin_1_p,
                                newdata = dat_s, family = "logit",
                                main_var = "pop_15_44_muni_gendergap_2015")


# Male Diadvantage
# Effect Estimation 
bin_1_sum_gap <- marginal_effect(bin_1_sum,
                                 newdata = dat_2015_s, family = "logit",
                                 main_var = "unemp_gendergap_2015",
                                 difference = TRUE, 
                                 treat_range = c(1, 1.15))

bin_1_p_gap <- marginal_effect(bin_1_p,
                               newdata = dat_s, family = "logit",
                               main_var = "unemp_gendergap_2015",
                               difference = TRUE, 
                               treat_range = c(1, 1.15))

# Dose Function
bin_1_sum_gap_dose <- marginal_effect(bin_1_sum,
                                      newdata = dat_2015_s, family = "logit",
                                      main_var = "unemp_gendergap_2015")

bin_1_p_gap_dose <- marginal_effect(bin_1_p,
                                    newdata = dat_s, family = "logit",
                                    main_var = "unemp_gendergap_2015")

# #####################
# Plot Effects (Figure 1)
# #####################
point <- c(bin_1_sum_effect$out_main[2], bin_1_p_effect$out_main[2])
high  <- c(bin_1_sum_effect$out_main[3], bin_1_p_effect$out_main[3])
low   <- c(bin_1_sum_effect$out_main[1], bin_1_p_effect$out_main[1])

## Short Panel 
marginal_list <- list()
marginal_list[[1]] <- bin_1_sum_dose
marginal_list[[2]] <- bin_1_p_dose

title_c <- c("Predicted Probability: Sum", "Predicted Probability: Annual")

# Plot Dose function

pdf("figure_1.pdf", height = 4, width = 11)
par(mfrow = c(1,3), mar = c(4.5, 2, 4, 1), oma = c(0, 2, 0, 0))
for(i in 1:2){
  plot_coef_all  <- do.call("rbind", marginal_list[[i]]$out_main)
  plot_x <-  marginal_list[[i]]$treat_range
  if(i == 1){
    ylim_u <- c(0.14, 0.23)
    ylab_u <- ""
  }
  if(i == 2){
    ylim_u <- c(0.06, 0.11)
    ylab_u <- ""
  }
  
  plot(plot_x, plot_coef_all[ ,2], pch = 19, 
       main = paste("", title_c[i], sep = ""),
       ylim = ylim_u, 
       xlab = "Excess Males", 
       ylab = ylab_u,
       col = "black", cex = 2, type = "l", lwd = 3, cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
  lines(plot_x, plot_coef_all[ ,1], col = "black", lty = 2)
  lines(plot_x, plot_coef_all[ ,3], col = "black", lty = 2)
  abline(h = marginal_list[[i]]$sample, lty = 2, col = "red", lwd = 2)
  polygon(c(plot_x, rev(plot_x)), c(plot_coef_all[ ,1], rev(plot_coef_all[ ,3])),
          col = adjustcolor("black", 0.2), border = NA)
  par(new=TRUE)
  hist(dat_2015_s$pop_15_44_muni_gendergap_2015, freq = FALSE, 
       breaks = seq(from = 0, to = 6, by = 0.01), xlim = c(min(plot_x), max(plot_x)), 
       xaxt = "n", yaxt = "n", xlab = "", ylab = "", ylim = c(0, 40), main ="")
}
par(mar = c(4.5, 5, 4, 1))
plot(seq(1:2), point, ylim = c(-0.005, 0.045), 
     ylab = "Effects on Prob (hate crime)", 
     main = "Effects of Excess Males",
     xlim = c(0.5, 2.5), xlab = "Outcome Types", xaxt = "n", pch = c(19, 15), 
     cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
segments(seq(1:6), low, seq(1:6), high, lwd = 2, c(rep("black",2), rep("black",2), rep("black",2)))
Axis(side = 1, at = c(1, 2), labels = c("Sum", "Annual"), cex.axis = 1.5)
abline(h = 0, lty = 2)
mtext(side = 2, at = 0.5, "Prob (hate crime)", outer = TRUE, line = 0.5)
dev.off()

# ########################
# Figure C.8 in Appendix
# ########################

# Plot Effects 
point_g <- c(bin_1_sum_gap$out_main[2], bin_1_p_gap$out_main[2])
high_g  <- c(bin_1_sum_gap$out_main[3], bin_1_p_gap$out_main[3])
low_g   <- c(bin_1_sum_gap$out_main[1], bin_1_p_gap$out_main[1])

## Short Panel 
marginal_list_g <- list()
marginal_list_g[[1]] <- bin_1_sum_gap_dose
marginal_list_g[[2]] <- bin_1_p_gap_dose

title_c <- c("Predicted Probability: Sum", "Predicted Probability: Annual")

# Plot Dose function

pdf("figure_C8.pdf", height = 4, width = 11)
par(mfrow = c(1,3), mar = c(4.5, 2, 4, 1), oma = c(0, 2, 0, 0))
for(i in 1:2){
  plot_coef_all  <- do.call("rbind", marginal_list_g[[i]]$out_main)
  plot_x <-  marginal_list_g[[i]]$treat_range
  if(i <=1) ylim_u <- c(0.14, 0.22)
  if(i > 1) ylim_u <- c(0.06, 0.11)
  
  plot(plot_x, plot_coef_all[ ,2], pch = 19,
       main = paste("", title_c[i], sep = ""),
       ylim = ylim_u,
       xlab = "Male Disadvantage",
       ylab = "",
       col = "black", cex = 2, type = "l", lwd = 3, cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
  lines(plot_x, plot_coef_all[ ,1], col = "black", lty = 2)
  lines(plot_x, plot_coef_all[ ,3], col = "black", lty = 2)
  abline(h = marginal_list_g[[i]]$sample, lty = 2, col = "red", lwd = 2)
  polygon(c(plot_x, rev(plot_x)), c(plot_coef_all[ ,1], rev(plot_coef_all[ ,3])),
          col = adjustcolor("black", 0.2), border = NA)
  par(new=TRUE)
  hist(dat_2015_s$unemp_gendergap_2015, freq = FALSE,
       breaks = seq(from = 0, to = 6, by = 0.01), xlim = c(min(plot_x), max(plot_x)),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "", ylim = c(0, 40), main ="")
}
par(mar = c(4.5, 5, 4, 1))
plot(seq(1:2), point_g, ylim = c(-0.01, 0.035), 
     ylab = "Effects on Prob (hate crime)", 
     main = "Effects of Male Disadvantage",
     xlim = c(0.5, 2.5), xlab = "Outcome Types", xaxt = "n", pch = c(19, 15), 
     cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
segments(seq(1:6), low_g, seq(1:6), high_g, lwd = 2, c(rep("black",2), rep("black",2), rep("black",2)))
Axis(side = 1, at = c(1, 2), labels = c("Sum", "Annual"), cex.axis = 1.25)
abline(h = 0, lty = 2)
mtext(side = 2, at = 0.5, "Prob (hate crime)", outer = TRUE, line = 0.5)
dev.off()