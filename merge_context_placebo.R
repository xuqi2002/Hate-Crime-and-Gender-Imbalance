rm(list = ls())
# install.packages("readstata13") # readstata13_0.9.2
# install.packages("plm") # plm_2.2-3  

library(readstata13) # readstata13_0.9.2
library(plm) # plm_2.2-3  

setwd("source_data")

# 0. Base data that contains AGS identifies and Year 
base_pl <- read.dta13("base_pl.dta")

# Note: 
# Every data source we use below is fully described in "source_context.pdf" 

# 1. Hate Crime Data
hate <- read.dta13("hate.dta")
hate$Housing_all_muni <- hate$Arson_muni + hate$Other_muni
hate$Hate_all_muni <- hate$Housing_all_muni + hate$Physical_muni


context0 <- merge(base_pl, hate[, c("ags_muni", "year", 
                                    "Hate_all_muni")], by = c("ags_muni", "year"), all.x = TRUE)

# 2. Population Data 
pop_dat <- read.dta13("pop_gemeinde_2008_2018.dta")
pop_dat <- pop_dat[pop_dat$year >= 2011, ]

pop_dat$pop_m_25_44 <- pop_dat$pop_m_25_29 + pop_dat$pop_m_30_34 +  pop_dat$pop_m_35_39 + pop_dat$pop_m_40_44
pop_dat$pop_f_25_44 <- pop_dat$pop_f_25_29 + pop_dat$pop_f_30_34 +  pop_dat$pop_f_35_39 + pop_dat$pop_f_40_44

pop_dat$pop_m_15_44 <- pop_dat$pop_m_15_17 +  pop_dat$pop_m_18_19 +  pop_dat$pop_m_20_24 + pop_dat$pop_m_25_44
pop_dat$pop_f_15_44 <- pop_dat$pop_f_15_17 +  pop_dat$pop_f_18_19 +  pop_dat$pop_f_20_24 + pop_dat$pop_f_25_44

pop_dat$population_muni_anu <- pop_dat$pop_mf_total

pop_dat$pop_25_44_muni_gendergap_anu <- pop_dat$pop_m_25_44/pop_dat$pop_f_25_44
pop_dat$pop_15_44_muni_gendergap_anu <- pop_dat$pop_m_15_44/pop_dat$pop_f_15_44
pop_dat$pop_25_44_muni_gendergap_anu[is.infinite(pop_dat$pop_25_44_muni_gendergap_anu)] <- NA
pop_dat$pop_15_44_muni_gendergap_anu[is.infinite(pop_dat$pop_15_44_muni_gendergap_anu)] <- NA

pop_dat_pl <-  pop_dat[, c("year", "ags_muni","ags_county", "ags_state",
                           "population_muni_anu", "pop_15_44_muni_gendergap_anu")]

# creating pop_15_44_muni_gendergap_future
pop_dat_pl_p <- pdata.frame(pop_dat_pl, index = c("ags_muni", "year"))
pop_dat_pl_p$pop_15_44_muni_gendergap_future <- as.numeric(lead(pop_dat_pl_p$pop_15_44_muni_gendergap_anu, k = 1))
class(pop_dat_pl_p) <- "data.frame"
pop_dat_pl <- pop_dat_pl_p
rownames(pop_dat_pl) <- NULL

#  3. area  
area <- read.dta13("area_mun.dta")
area_use <- area[area$ags %in% base_pl$ags_muni, ]
colnames(area_use)[colnames(area_use) == "ags"] <- "ags_muni"

pop_dat_pl <- merge(pop_dat_pl, area_use[,c("ags_muni", "area_sqk")], all.x =  TRUE, all.y = FALSE)
pop_dat_pl$popdens_muni_anu <- pop_dat_pl$population_muni_anu/pop_dat_pl$area_sqk

pop_dat_2015 <- subset(pop_dat_pl, year ==  2015)
pop_dat_2011 <- subset(pop_dat_pl, year ==  2011)
pop_dat_2011 <- pop_dat_2011[match(pop_dat_2015$ags_muni, pop_dat_2011$ags_muni),]
pop_dat_2015$d_pop_muni_anu <- 
  (pop_dat_2015$population_muni - pop_dat_2011$population_muni)/pop_dat_2011$population_muni

pop_dat_2016 <- subset(pop_dat_pl, year ==  2016)
pop_dat_2012 <- subset(pop_dat_pl, year ==  2012)
pop_dat_2012 <- pop_dat_2012[match(pop_dat_2016$ags_muni, pop_dat_2012$ags_muni),]
pop_dat_2016$d_pop_muni_anu <- 
  (pop_dat_2016$population_muni - pop_dat_2012$population_muni)/pop_dat_2012$population_muni

pop_dat_2017 <- subset(pop_dat_pl, year ==  2017)
pop_dat_2013 <- subset(pop_dat_pl, year ==  2013)
pop_dat_2013 <- pop_dat_2013[match(pop_dat_2017$ags_muni, pop_dat_2013$ags_muni),]
pop_dat_2017$d_pop_muni_anu <- 
  (pop_dat_2017$population_muni - pop_dat_2013$population_muni)/pop_dat_2013$population_muni

pop_dat_d <- rbind(pop_dat_2015, pop_dat_2016, pop_dat_2017)

context0 <- merge(context0, pop_dat_d[, c("year", "ags_muni", 
                                          "pop_15_44_muni_gendergap_anu",
                                          "pop_15_44_muni_gendergap_future",
                                          "population_muni_anu",
                                          "popdens_muni_anu",
                                          "d_pop_muni_anu")], by = c("year", "ags_muni"), 
                  all.x = TRUE, all.y = FALSE)

# 4. Unemployment
pop_dat <- read.dta13("pop_gemeinde_2008_2018.dta")
colnames(pop_dat)[colnames(pop_dat) == "ags"] <- "ags_muni"
pop_dat$pop_m_25_44 <- pop_dat$pop_m_25_29 + pop_dat$pop_m_30_34 +  pop_dat$pop_m_35_39 + pop_dat$pop_m_40_44
pop_dat$pop_f_25_44 <- pop_dat$pop_f_25_29 + pop_dat$pop_f_30_34 +  pop_dat$pop_f_35_39 + pop_dat$pop_f_40_44
pop_dat$pop_m_15_44 <- pop_dat$pop_m_15_17 +  pop_dat$pop_m_18_19 +  pop_dat$pop_m_20_24 + pop_dat$pop_m_25_44
pop_dat$pop_f_15_44 <- pop_dat$pop_f_15_17 +  pop_dat$pop_f_18_19 +  pop_dat$pop_f_20_24 + pop_dat$pop_f_25_44
unemp_dat <- read.dta13("unempl_gemeinde_2008_2017.dta")
colnames(unemp_dat)[colnames(unemp_dat) == "ags"] <- "ags_muni"
colnames(unemp_dat)[colnames(unemp_dat) == "ags_dist"] <- "ags_county"

## unemployed as share of working age population (age 15-64)
pop_dat$pop_mf_15_64 <- pop_dat$pop_mf_15_17 +  pop_dat$pop_mf_18_19 +  pop_dat$pop_mf_20_24 + 
  pop_dat$pop_mf_25_29 + pop_dat$pop_mf_30_34 +  pop_dat$pop_mf_35_39 + pop_dat$pop_mf_40_44 + 
  pop_dat$pop_mf_45_49 + pop_dat$pop_mf_50_54 + pop_dat$pop_mf_55_59 + pop_dat$pop_mf_60_64

pop_dat$pop_m_15_64 <- pop_dat$pop_m_15_44 + pop_dat$pop_m_45_49 + pop_dat$pop_m_50_54 + 
  pop_dat$pop_m_55_59 + pop_dat$pop_m_60_64

pop_dat$pop_f_15_64 <- pop_dat$pop_f_15_44 + pop_dat$pop_f_45_49 + pop_dat$pop_f_50_54 + 
  pop_dat$pop_f_55_59 + pop_dat$pop_f_60_64

unemp_dat_use <- unemp_dat[, c("ags_muni", "ags_county",
                               "year", 
                               "unempl_all_total", 
                               "unempl_all_male_total", "unempl_all_fem_total")]
pop_dat_m <- pop_dat[pop_dat$year >= 2011, c("ags_muni", "year", "pop_mf_15_64", "pop_m_15_64", "pop_f_15_64")]
unemp_merge  <- merge(pop_dat_m, unemp_dat_use, by = c("ags_muni", "year"), all.x = TRUE, all.y = FALSE)

unemp_merge$unemp_all_muni  <- (unemp_merge$unempl_all_total/unemp_merge$pop_mf_15_64)*100

dat_2015 <- unemp_merge[unemp_merge$year == 2015, ]
dat_2016 <- unemp_merge[unemp_merge$year == 2016, ]
dat_2017 <- unemp_merge[unemp_merge$year == 2017, ]

dat_2015$log_unemp_all_muni_anu <- log(dat_2015$unemp_all_muni  + 1) # constants are chosen to make sure all values are positive
dat_2016$log_unemp_all_muni_anu <- log(dat_2016$unemp_all_muni  + 0.3)
dat_2017$log_unemp_all_muni_anu <- log(dat_2017$unemp_all_muni  + 0.3)
unemp_u <- rbind(dat_2015, dat_2016, dat_2017)

context0  <- merge(context0, unemp_u[, c("ags_muni", "year",
                                         "log_unemp_all_muni_anu")], 
                   by = c("ags_muni", "year"), all.x = TRUE, all.y = FALSE)

# 5 Unemployment Rate Gap
d20 <- read.dta13("merged_context_2.dta")
d2 <- unique(d20[, c("year", "ags_county", "unemp_gendergap")])
d2$unemp_gendergap_anu <- d2$unemp_gendergap

context0 <- merge(context0, d2[, c("year", "ags_county", "unemp_gendergap_anu")], 
                  all.x = TRUE, all.y = FALSE, by = c("year", "ags_county"))

# 6. Voting
voting <- read.dta13("voting.dta")

context0 <- merge(context0, voting[, c("ags_muni", 
                                       "vote_afd_2013_muni")], by = c("ags_muni"), 
                  all.x = TRUE, all.y = FALSE)

# 7. Refugee Data
ref_dat <- read.dta13("refugees_2008_2017.dta")
ref_dat <- ref_dat[ref_dat$year >= 2011, ]

# Creating pop_ref_anu
ref_dat_p <- pdata.frame(ref_dat, index = c("ags_county", "year"))
ref_dat_p$pop_ref_anu <- as.numeric(lag(ref_dat_p$pop_ref, k = 1))
class(ref_dat_p) <- "data.frame"
ref_dat <- ref_dat_p

ref_dat <- ref_dat[ref_dat$ags_county %in% unique(ref_dat$ags_county[ref_dat$year == 2014]),]
ref_2014 <- ref_dat[ref_dat$year == 2014, ]
ref_2015 <- ref_dat[ref_dat$year == 2015, ]
ref_2016 <- ref_dat[ref_dat$year == 2016, ]
ref_2017 <- ref_dat[ref_dat$year == 2017, ]

ref_2015$ref_inflow_1514 <- ref_2015$pop_ref - ref_2014$pop_ref
ref_2016$ref_inflow_1615 <- ref_2016$pop_ref - ref_2015$pop_ref
ref_2017$ref_inflow_1716 <- ref_2017$pop_ref - ref_2016$pop_ref

ref_2015$log_ref_inflow_anu <- log(1500 + ref_2015$ref_inflow_1514) # constants are chosen such that all values are positive
suppressWarnings(ref_2016$log_ref_inflow_anu <-  log(ref_2016$ref_inflow_1615 + 649))
suppressWarnings(ref_2017$log_ref_inflow_anu <-  log(ref_2017$ref_inflow_1716 + 1261))

ref_2015 <- ref_2015[, c("year", "ags_county", "pop_ref_anu", "log_ref_inflow_anu")]
ref_2016 <- ref_2016[, c("year", "ags_county", "pop_ref_anu", "log_ref_inflow_anu")]
ref_2017 <- ref_2017[, c("year", "ags_county", "pop_ref_anu", "log_ref_inflow_anu")]
ref_data_c <- rbind(ref_2015, ref_2016, ref_2017)

context0 <- merge(context0, ref_data_c[, c("year", "ags_county", 
                                           "pop_ref_anu",
                                           "log_ref_inflow_anu")], 
                  by = c("year", "ags_county"), all.x = TRUE, all.y = FALSE)

#  8. Violence
crime <- read.dta13("crime.dta")
pop <- read.dta13("pop_kreise_2015_2017.dta")

crime2 <- merge(crime[, c("year", "ags_county", "violence_num_cases")],
                pop[, c("year", "ags_county", "population")],
                by = c("year", "ags_county"), all.x = TRUE, all.y = FALSE)
crime2$violence_percap_anu <- crime2$violence_num_cases/crime2$population

context0 <- merge(context0, crime2[, c("year", "ags_county", 
                                       "violence_percap_anu")], by = c("year", "ags_county"), 
                  all.x = TRUE, all.y = FALSE)

# 9. Education
edu <- read.dta13("merged_context_2.dta")
edu  <- edu[edu$year ==  2011, ]
edu <- edu[, c("ags_county", "pc_hidegree_all2011")]


context0 <- merge(context0, edu[, c("ags_county", "pc_hidegree_all2011")], by = c("ags_county"), all.x = TRUE)

#  10. Industry 
manu0 <- read.dta13("merged_context_2.dta")
manu0 <- manu0[, c("year", "ags_county", "pc_manufacturing")]
manu <- manu0[manu0$year >= 2011 & manu0$year <= 2015, ]
rownames(manu) <- NULL
manu_orig <- manu

manu  <- manu_orig[manu_orig$year ==  2015, ]
manu <- manu[, c("ags_county", "pc_manufacturing")]
manu <- manu[is.element(manu$ags_county, unique(base_pl$ags_county)),]
manu$pc_manufacturing_2015 <- manu$pc_manufacturing

manu2011  <- manu_orig[manu_orig$year ==  2011, ]
manu2011 <- manu2011[, c("ags_county", "pc_manufacturing")]
manu2011 <- manu2011[is.element(manu2011$ags_county, unique(base_pl$ags_county)),]
manu2011$pc_manufacturing_2011 <- manu2011$pc_manufacturing

# d_manuf1115
manu$d_manuf1115 <- manu$pc_manufacturing_2015 - manu2011$pc_manufacturing_2011

context0 <- merge(context0, manu[, c("ags_county", 
                                     "pc_manufacturing_2015",
                                     "d_manuf1115")], by = c("ags_county"), all.x = TRUE)


# 11. Create additional variables 
context0$Hate_all_muni_bin <- as.numeric(context0$Hate_all_muni >  0)

context0 <- context0[order(context0$year, context0$ags_muni), ]
save.dta13(context0, file = "context_placebo.dta")