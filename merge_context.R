# Internal File for Merging datasets for producing "context.dta"
# R version 4.0.2 (2020-06-22)

rm(list=ls())
# install.packages("readstata13") # readstata13_0.9.2

require(readstata13)  #  readstata13_0.9.2

setwd("source_data")

# 0. Base data that contains AGS identifies and Year 
base <- read.dta13("base.dta")

# Note: 
# Every data source we use below is fully described in "source_context.pdf" 

# 1. Hate Crime Data 
hate <- read.dta13("hate.dta")
hate$Housing_all_muni <- hate$Arson_muni + hate$Other_muni
hate$Hate_all_muni <- hate$Housing_all_muni + hate$Physical_muni

context0 <- merge(base, hate[, c("ags_muni", "year", 
                                 "Hate_all_muni", 
                                 "Physical_muni")], by = c("ags_muni", "year"), all.x = TRUE)

# 2. Population Data 
pop_dat <- read.dta13("pop_gemeinde_2008_2018.dta")
colnames(pop_dat)[colnames(pop_dat) == "ags"] <- "ags_muni"

pop_dat$pop_m_25_44 <- pop_dat$pop_m_25_29 + pop_dat$pop_m_30_34 +  pop_dat$pop_m_35_39 + pop_dat$pop_m_40_44
pop_dat$pop_f_25_44 <- pop_dat$pop_f_25_29 + pop_dat$pop_f_30_34 +  pop_dat$pop_f_35_39 + pop_dat$pop_f_40_44

pop_dat$pop_m_15_44 <- pop_dat$pop_m_15_17 +  pop_dat$pop_m_18_19 +  pop_dat$pop_m_20_24 + pop_dat$pop_m_25_44
pop_dat$pop_f_15_44 <- pop_dat$pop_f_15_17 +  pop_dat$pop_f_18_19 +  pop_dat$pop_f_20_24 + pop_dat$pop_f_25_44

pop_dat$population_muni <- pop_dat$pop_mf_total

pop_dat$pop_25_44_muni_gendergap <- pop_dat$pop_m_25_44/pop_dat$pop_f_25_44
pop_dat$pop_15_44_muni_gendergap <- pop_dat$pop_m_15_44/pop_dat$pop_f_15_44
pop_dat$pop_25_44_muni_gendergap[is.infinite(pop_dat$pop_25_44_muni_gendergap)] <- NA
pop_dat$pop_15_44_muni_gendergap[is.infinite(pop_dat$pop_15_44_muni_gendergap)] <- NA

pop_dat_2015 <- subset(pop_dat, year ==  2015)
pop_dat_2015$pop_25_44_muni_gendergap_2015 <- pop_dat_2015$pop_m_25_44/pop_dat_2015$pop_f_25_44
pop_dat_2015$pop_15_44_muni_gendergap_2015 <- pop_dat_2015$pop_m_15_44/pop_dat_2015$pop_f_15_44
pop_dat_2015$pop_25_44_muni_gendergap_2015[is.infinite(pop_dat_2015$pop_25_44_muni_gendergap_2015)] <- NA
pop_dat_2015$pop_15_44_muni_gendergap_2015[is.infinite(pop_dat_2015$pop_15_44_muni_gendergap_2015)] <- NA

pop_dat_2015$population_muni_2015 <- pop_dat_2015$pop_mf_total


#  3. area  
area <- read.dta13("area_mun.dta")
area_use <- area[area$ags %in% context0$ags_muni, ]
colnames(area_use)[colnames(area_use) == "ags"] <- "ags_muni"

pop_dat_2015 <- merge(pop_dat_2015, area_use[,c("ags_muni", "area_sqk")], all.x =  TRUE)
pop_dat_2015$popdens_muni_2015 <- pop_dat_2015$population_muni_2015/pop_dat_2015$area_sqk

pop_dat <- merge(pop_dat, area_use[,c("ags_muni", "area_sqk")], all.x =  TRUE)
pop_dat$popdens_muni <- pop_dat$population_muni/pop_dat$area_sqk

context0 <- merge(context0, pop_dat_2015[, c("ags_muni", 
                                             "pop_25_44_muni_gendergap_2015",
                                             "pop_15_44_muni_gendergap_2015",
                                             "population_muni_2015",
                                             "popdens_muni_2015")], by = c("ags_muni"), all.x = TRUE)

# 4. Unemployment
pop_dat <- read.dta13("pop_gemeinde_2008_2018.dta")
colnames(pop_dat)[colnames(pop_dat) == "ags"] <- "ags_muni"
pop_dat$pop_m_25_44 <- pop_dat$pop_m_25_29 + pop_dat$pop_m_30_34 +  pop_dat$pop_m_35_39 + pop_dat$pop_m_40_44
pop_dat$pop_f_25_44 <- pop_dat$pop_f_25_29 + pop_dat$pop_f_30_34 +  pop_dat$pop_f_35_39 + pop_dat$pop_f_40_44
pop_dat$pop_m_15_44 <- pop_dat$pop_m_15_17 +  pop_dat$pop_m_18_19 +  pop_dat$pop_m_20_24 + pop_dat$pop_m_25_44
pop_dat$pop_f_15_44 <- pop_dat$pop_f_15_17 +  pop_dat$pop_f_18_19 +  pop_dat$pop_f_20_24 + pop_dat$pop_f_25_44
unemp_dat <-  read.dta13("unempl_gemeinde_2008_2017.dta")
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
                               "unempl_all_male_total", 
                               "unempl_all_fem_total")]
pop_dat_m <- pop_dat[pop_dat$year >= 2011, c("ags_muni", "year", "pop_mf_15_64", "pop_m_15_64", "pop_f_15_64")]
unemp_merge  <- merge(pop_dat_m, unemp_dat_use, by = c("ags_muni", "year"), all.x = TRUE, all.y = FALSE)
unemp_merge$unemp_all_muni  <- (unemp_merge$unempl_all_total/unemp_merge$pop_mf_15_64)*100

unemp_2015 <- unemp_merge[unemp_merge$year == 2015, ]
unemp_2015$unemp_all_muni_2015 <- unemp_2015$unemp_all_muni
unemp_2015$log_unemp_all_muni_2015  <- log(unemp_2015$unemp_all_muni_2015 + 1)

context0 <- merge(context0, unemp_2015[, c("ags_muni", 
                                           "log_unemp_all_muni_2015")], by = c("ags_muni"), 
                  all.x = TRUE, all.y = FALSE)

# 5. Unemployment Gender Gap
d2 <- read.dta13("merged_context_2.dta")  # we created this data set in "produce_context_data.do"
d2_15 <- d2[d2$year == 2015, ]
d2_15$unemp_gendergap_2015 <- round(d2_15$unemp_gendergap, 6)

context0 <- merge(context0, d2_15[, c("ags_county", "unemp_gendergap_2015")], 
                  all.x = TRUE, all.y = FALSE, by = "ags_county")

# 6. Population Change
pop_dat <- read.dta13("pop_gemeinde_2008_2018.dta")
colnames(pop_dat)[colnames(pop_dat) == "ags"] <- "ags_muni"
pop_dat$population_muni <- pop_dat$pop_mf_total
pop_dat_2015 <- subset(pop_dat, year ==  2015)
pop_dat_2011 <- subset(pop_dat, year ==  2011)
pop_dat_2015$d_pop1511_muni <- 
  (pop_dat_2015$population_muni - pop_dat_2011$population_muni)/pop_dat_2011$population_muni

context0 <- merge(context0, pop_dat_2015[, c("ags_muni", 
                                             "d_pop1511_muni")], by = c("ags_muni"), all.x = TRUE)

# 7. Voting
voting <- read.dta13("voting.dta")

context0 <- merge(context0, voting[, c("ags_muni", 
                                       "vote_afd_2013_muni")], by = c("ags_muni"), all.x = TRUE)

# 8. Refugee Data
ref_dat <- read.dta13("refugees_2008_2017.dta")
ref_2014 <- subset(ref_dat, year == 2014)
ref_2015 <- subset(ref_dat, year == 2015)
table(ref_2014$ags_county  == ref_2015$ags_county)

ref_2014$ref_inflow_1514 <- ref_2015$pop_ref - ref_2014$pop_ref
ref_2014$log_ref_inflow_1514 <- log(1500 + ref_2014$ref_inflow_1514)

ref_2014$pop_ref_2014 <- ref_2014$pop_ref
ref_2014$pop_ref_2015 <- ref_2015$pop_ref

# Proportion of male refugees
ref_prop <- read.dta13("merged_context_2.dta") # we created this data set in "produce_context_data.do"

context0 <- merge(context0, ref_2014[, c("ags_county", 
                                         "log_ref_inflow_1514",
                                         "pop_ref_2014")], by = c("ags_county"), all.x = TRUE)
context0 <- merge(context0, ref_prop[, c("year", "ags_county", 
                                         "pc_ref_male")], by = c("year", "ags_county"), all.x = TRUE)

#  9. Violence
crime <- read.dta13("crime.dta")
crime <- crime[crime$year == 2015, ]

pop_county <- read.dta13("pop_kreise_2015_2017.dta")
pop_county1 <- subset(pop_county, year == 2015)

crime2 <- merge(crime[, c("ags_county", "violence_num_cases")],
                pop_county1[, c("ags_county", "population")],
                by = "ags_county", all.x = TRUE, all.y = FALSE)
crime2$violence_percap_2015 <- crime2$violence_num_cases/crime2$population

context0 <- merge(context0, crime2[, c("ags_county", 
                                       "violence_percap_2015")], by = c("ags_county"), all.x = TRUE)

# 10. Education
edu <- read.dta13("merged_context_2.dta") # we created this data set in "produce_context_data.do"
edu  <- edu[edu$year ==  2011, ]
edu <- edu[, c("ags_county", "pc_hidegree_all2011")]

context0 <- merge(context0, edu[, c("ags_county", 
                                    "pc_hidegree_all2011")], by = c("ags_county"), all.x = TRUE)


#  10. Industry 
manu0 <- read.dta13("merged_context_2.dta") # we created this data set in "produce_context_data.do"
manu0 <- manu0[, c("year", "ags_county", "pc_manufacturing")]
manu <- manu0[manu0$year >= 2011 & manu0$year <= 2015, ]
rownames(manu) <- NULL
manu_orig <- manu

manu  <- manu_orig[manu_orig$year ==  2015, ]
manu <- manu[, c("ags_county", "pc_manufacturing")]
manu <- manu[is.element(manu$ags_county, unique(context0$ags_county)),]
manu$pc_manufacturing_2015 <- manu$pc_manufacturing

manu2011  <- manu_orig[manu_orig$year ==  2011, ]
manu2011 <- manu2011[, c("ags_county", "pc_manufacturing")]
manu2011 <- manu2011[is.element(manu2011$ags_county, unique(context0$ags_county)),]
manu2011$pc_manufacturing_2011 <- manu2011$pc_manufacturing

# d_manuf1115
manu$d_manuf1115 <- manu$pc_manufacturing_2015 - manu2011$pc_manufacturing_2011

context0 <- merge(context0, manu[, c("ags_county", 
                                     "pc_manufacturing_2015",
                                     "d_manuf1115")], by = c("ags_county"), all.x = TRUE)

# 11. East
context0$east <- 0
context0$east[context0$ags_state %in% c("11","12","13","14","15","16")]  <-  1

# 12. Create additional variables 
context0$Hate_all_muni_bin <- as.numeric(context0$Hate_all_muni >  0)
context0$Physical_muni_bin <- as.numeric(context0$Physical_muni >  0)

context0$log_population_muni_2015 <- log(context0$population_muni_2015)
context0$log_popdens_muni_2015 <- log(context0$popdens_muni_2015)
context0$log_pop_ref_2014 <- log(context0$pop_ref_2014)
context0$log_violence_percap_2015 <- log(context0$violence_percap_2015)

context0 <- context0[order(context0$year, context0$ags_muni), ]
save.dta13(context0, file = "context.dta")