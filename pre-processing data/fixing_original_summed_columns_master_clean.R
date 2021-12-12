#################################################
##  Creating my own version of summed columns  ##
#################################################
#
# Description:
#   Re-adding columns from "first principles" to keep consistency
#   Ommitting CHW counts
#
#
#  Sebastian Rodriguez (sebastian@rodriguez.cr)
#  Last edited Mar 09, 2021
#


rm(list = ls(all = TRUE))


library("plyr")
library("dplyr")



####################################################################

# Definite reading and writing directory

read_dir <- "~/Box/NU-malaria-team/data/burkina_who/Routine/Routine_data_from_country/BFA_Routine_case_data.csv"

write_dir <- "~/Box/NU-malaria-team/projects/smc_impact/data/outputs/BFA_Routine_case_data_HF_aggregate_Seb_sum_columns.csv"


####################################################################

# in this dataset, all HF have 48 obs (12 months x 4 yrs)
cases_master <- read.csv(read_dir, header = TRUE, strip.white = TRUE, stringsAsFactors = FALSE)


# removing all NA columns (organisationunitname is not NA but same as level6)
rm.cols <- c("Variable.name", "orgunitlevel1", "period", "reprate", "reptime", "llins_rout", "llins_anc", "llins_epi", "organisationunitname")
cases_new <- cases_master[, -which(names(cases_master) %in% rm.cols)]




####################################################################


cases_new$allout_age4 <- rowSums(cases_new[,c("allout_age4f", "allout_age4m")], na.rm = TRUE)
cases_new[which(is.na(cases_new$allout_age4f) & is.na(cases_new$allout_age4m)), "allout_age4"] <- NA
cases_new$allout <- rowSums(cases_new[,c("allout_u5", "allout_ov5")], na.rm = TRUE)
cases_new[which(is.na(cases_new$allout_u5) & is.na(cases_new$allout_ov5)), "allout"] <- NA


cases_new$susp_u5 <- rowSums(cases_new[,c("susp_age1", "susp_age2")], na.rm = TRUE)
cases_new[which(is.na(cases_new$susp_age1) & is.na(cases_new$susp_age2)), "susp_u5"] <- NA
cases_new$susp_ov5 <- rowSums(cases_new[,c("susp_age3", "susp_age4")], na.rm = TRUE)
cases_new[which(is.na(cases_new$susp_age3) & is.na(cases_new$susp_age4)), "susp_ov5"] <- NA
cases_new$susp <- rowSums(cases_new[,c("susp_u5", "susp_ov5")], na.rm = TRUE)
cases_new[which(is.na(cases_new$susp_u5) & is.na(cases_new$susp_ov5)), "susp"] <- NA


#############################################################################
#  Initially, sometimes test_rdt_u5 == test_rdt_ov5.
#       Even worse, in these cases test_rdt_age3 + 4 != ov5 (but it does for u5)
#       Also, test_rdt != test_rdt_u5 + test_rdt_ov5
#           But test_rdt_ov5 + test_rdt_age3 + test_rdt_age4 = test_rdt
#  weird example of how this messes up is below
#
# tmp <- cases_new[which(!is.na(cases_new$test_rdt_u5)),]
# View(tmp[which(tmp$test_rdt_ov5 != tmp$test_rdt_u5),])


tmp_test_rdt_u5 <- rowSums(cases_new[,c("test_rdt_age1", "test_rdt_age2")], na.rm = TRUE)
tmp_test_rdt_u5[which((tmp_test_rdt_u5 == 0 | is.na(tmp_test_rdt_u5)) & (!is.na(cases_new$test_rdt_u5) & cases_new$test_rdt_u5 > 0))] <- cases_new$test_rdt_u5[which((tmp_test_rdt_u5 == 0 | is.na(tmp_test_rdt_u5)) & (!is.na(cases_new$test_rdt_u5) & cases_new$test_rdt_u5 > 0))]
tmp_test_rdt_u5[which(is.na(cases_new$test_rdt_age1) & is.na(cases_new$test_rdt_age2) & (is.na(cases_new$test_rdt_u5) | cases_new$test_rdt_u5 == 0))] <- NA

cases_new$test_rdt_u5 <- tmp_test_rdt_u5
cases_new$test_rdt_ov5 <- rowSums(cases_new[,c("test_rdt_age3", "test_rdt_age4")], na.rm = TRUE)
cases_new[which(is.na(cases_new$test_rdt_age3) & is.na(cases_new$test_rdt_age4)), "test_rdt_ov5"] <- NA
cases_new$test_rdt <- rowSums(cases_new[,c("test_rdt_u5", "test_rdt_ov5")], na.rm = TRUE)
cases_new[which(is.na(cases_new$test_rdt_u5) & is.na(cases_new$test_rdt_ov5)), "test_rdt"] <- NA


####################################################################


cases_new$test_mic_u5 <- rowSums(cases_new[,c("test_mic_age1", "test_mic_age2")], na.rm = TRUE)
cases_new[which(is.na(cases_new$test_mic_age1) & is.na(cases_new$test_mic_age2)), "test_mic_u5"] <- NA
cases_new$test_mic_ov5 <- rowSums(cases_new[,c("test_mic_age3", "test_mic_age4")], na.rm = TRUE)
cases_new[which(is.na(cases_new$test_mic_age3) & is.na(cases_new$test_mic_age4)), "test_mic_ov5"] <- NA
cases_new$test_mic <- rowSums(cases_new[,c("test_mic_u5", "test_mic_ov5")], na.rm = TRUE)
cases_new[which(is.na(cases_new$test_mic_u5) & is.na(cases_new$test_mic_ov5)), "test_mic"] <- NA


#####################################
##  Weird stuff!!
##      fixed by using age1 + age2 where that wasnt 0 (with U5 being non-0) or NA
#
# tmpp <- cbind(cases_new$conf_rdt_age1, cases_new$conf_rdt_age2, tmp_conf_rdt_u5, cases_new$conf_rdt_u5)
# which(tmp_conf_rdt_u5 != cases_new$conf_rdt_u5)
# View(tmpp[which(tmp_conf_rdt_u5 != cases_new$conf_rdt_u5),])

tmp_conf_rdt_u5 <- rowSums(cases_new[,c("conf_rdt_age1", "conf_rdt_age2")], na.rm = TRUE)


## This seems to work
tmp_conf_rdt_u5[which((tmp_conf_rdt_u5 == 0 | is.na(tmp_conf_rdt_u5)) & (!is.na(cases_new$conf_rdt_u5) & cases_new$conf_rdt_u5 > 0))] <- cases_new$conf_rdt_u5[which((tmp_conf_rdt_u5 == 0 | is.na(tmp_conf_rdt_u5)) & (!is.na(cases_new$conf_rdt_u5) & cases_new$conf_rdt_u5 > 0))]
tmp_conf_rdt_u5[which(is.na(cases_new$conf_rdt_age1) & is.na(cases_new$conf_rdt_age2) & (is.na(cases_new$conf_rdt_u5) | cases_new$conf_rdt_u5 == 0))] <- NA


## Checking...
# tmp_tmpp <- tmp_conf_rdt_u5[which((tmp_conf_rdt_u5 != 0 & !is.na(tmp_conf_rdt_u5)) & (!is.na(cases_new$conf_rdt_u5) & cases_new$conf_rdt_u5 > 0))]
# tmp_original <- cases_new$conf_rdt_u5[which((tmp_conf_rdt_u5 != 0 & !is.na(tmp_conf_rdt_u5)) & (!is.na(cases_new$conf_rdt_u5) & cases_new$conf_rdt_u5 > 0))]
# View(cbind(tmp_tmpp, tmp_original)[which(tmp_tmpp < tmp_original),])




cases_new$conf_rdt_u5 <- tmp_conf_rdt_u5
cases_new$conf_rdt_ov5 <- rowSums(cases_new[,c("conf_rdt_age3", "conf_rdt_age4")], na.rm = TRUE)
cases_new[which(is.na(cases_new$conf_rdt_age3) & is.na(cases_new$conf_rdt_age4)), "conf_rdt_ov5"] <- NA
cases_new$conf_rdt <- rowSums(cases_new[,c("conf_rdt_u5", "conf_rdt_ov5")], na.rm = TRUE)
cases_new[which(is.na(cases_new$conf_rdt_u5) & is.na(cases_new$conf_rdt_ov5)), "conf_rdt"] <- NA


cases_new$conf_mic_u5 <- rowSums(cases_new[,c("conf_mic_age1", "conf_mic_age2")], na.rm = TRUE)
cases_new[which(is.na(cases_new$conf_mic_age1) & is.na(cases_new$conf_mic_age2)), "conf_mic_u5"] <- NA
cases_new$conf_mic_ov5 <- rowSums(cases_new[,c("conf_mic_age3", "conf_mic_age4")], na.rm = TRUE)
cases_new[which(is.na(cases_new$conf_mic_age3) & is.na(cases_new$conf_mic_age4)), "conf_mic_ov5"] <- NA
cases_new$conf_mic <- rowSums(cases_new[,c("conf_mic_u5", "conf_mic_ov5")], na.rm = TRUE)
cases_new[which(is.na(cases_new$conf_mic_u5) & is.na(cases_new$conf_mic_ov5)), "conf_mic"] <- NA




####################################################################


# Are these presumed variables correct? or should i recalculate from TPR and suspected?
cases_new$pres_u5 <- rowSums(cases_new[,c("pres_age1", "pres_age2")], na.rm = TRUE)
cases_new[which(is.na(cases_new$pres_age1) & is.na(cases_new$pres_age2)), "pres_u5"] <- NA
cases_new$pres_ov5 <- rowSums(cases_new[,c("pres_age3", "pres_age4")], na.rm = TRUE)
cases_new[which(is.na(cases_new$pres_age3) & is.na(cases_new$pres_age4)), "pres_ov5"] <- NA
cases_new$pres <- rowSums(cases_new[,c("pres_u5", "pres_ov5")], na.rm = TRUE)
cases_new[which(is.na(cases_new$pres_u5) & is.na(cases_new$pres_ov5)), "pres"] <- NA


####################################################################


cases_new$maltreat_u5 <- rowSums(cases_new[,c("maltreat_age1", "maltreat_age2")], na.rm = TRUE)
cases_new[which(is.na(cases_new$maltreat_age1) & is.na(cases_new$maltreat_age2)), "maltreat_u5"] <- NA
cases_new$maltreat_ov5 <- rowSums(cases_new[,c("maltreat_age3", "maltreat_age4")], na.rm = TRUE)
cases_new[which(is.na(cases_new$maltreat_age3) & is.na(cases_new$maltreat_age4)), "maltreat_ov5"] <- NA
cases_new$maltreat <- rowSums(cases_new[,c("maltreat_u5", "maltreat_ov5")], na.rm = TRUE)
cases_new[which(is.na(cases_new$maltreat_u5) & is.na(cases_new$maltreat_ov5)), "maltreat"] <- NA



####################################################################


cases_new$alladm_age4 <- rowSums(cases_new[,c("alladm_age4f", "alladm_age4m")], na.rm = TRUE)
cases_new[which(is.na(cases_new$alladm_age4f) & is.na(cases_new$alladm_age4m)), "alladm_age4"] <- NA
cases_new$alladm_u5 <- rowSums(cases_new[,c("alladm_age1", "alladm_age2")], na.rm = TRUE)
cases_new[which(is.na(cases_new$alladm_age1) & is.na(cases_new$alladm_age2)), "alladm_u5"] <- NA
cases_new$alladm_ov5 <- rowSums(cases_new[,c("alladm_age3", "alladm_age4")], na.rm = TRUE)
cases_new[which(is.na(cases_new$alladm_age3) & is.na(cases_new$alladm_age4)), "alladm_ov5"] <- NA
cases_new$alladm <- rowSums(cases_new[,c("alladm_u5", "alladm_ov5")], na.rm = TRUE)
cases_new[which(is.na(cases_new$alladm_u5) & is.na(cases_new$alladm_ov5)), "alladm"] <- NA



####################################################################


cases_new$maladm_age4 <- rowSums(cases_new[,c("maladm_age4f", "maladm_age4m")], na.rm = TRUE)
cases_new[which(is.na(cases_new$maladm_age4f) & is.na(cases_new$maladm_age4m)), "maladm_age4"] <- NA
cases_new$maladm_u5 <- rowSums(cases_new[,c("maladm_age1", "maladm_age2")], na.rm = TRUE)
cases_new[which(is.na(cases_new$maladm_age1) & is.na(cases_new$maladm_age2)), "maladm_u5"] <- NA
cases_new$maladm_ov5 <- rowSums(cases_new[,c("maladm_age3", "maladm_age4")], na.rm = TRUE)
cases_new[which(is.na(cases_new$maladm_age3) & is.na(cases_new$maladm_age4)), "maladm_ov5"] <- NA
cases_new$maladm <- rowSums(cases_new[,c("maladm_u5", "maladm_ov5")], na.rm = TRUE)
cases_new[which(is.na(cases_new$maladm_u5) & is.na(cases_new$maladm_ov5)), "maladm"] <- NA


####################################################################

# conf != conf_rdt + conf_mic
# Making conf_rdt_mic columns

cases_new$conf_rdt_mic_age1 <- rowSums(cases_new[,c("conf_rdt_age1", "conf_mic_age1")], na.rm = TRUE)
cases_new[which(is.na(cases_new$conf_rdt_age1) & is.na(cases_new$conf_mic_age1)), "conf_rdt_mic_age1"] <- NA
cases_new$conf_rdt_mic_age2 <- rowSums(cases_new[,c("conf_rdt_age2", "conf_mic_age2")], na.rm = TRUE)
cases_new[which(is.na(cases_new$conf_rdt_age2) & is.na(cases_new$conf_mic_age2)), "conf_rdt_mic_age2"] <- NA
cases_new$conf_rdt_mic_age3 <- rowSums(cases_new[,c("conf_rdt_age3", "conf_mic_age3")], na.rm = TRUE)
cases_new[which(is.na(cases_new$conf_rdt_age3) & is.na(cases_new$conf_mic_age3)), "conf_rdt_mic_age3"] <- NA
cases_new$conf_rdt_mic_age4 <- rowSums(cases_new[,c("conf_rdt_age4", "conf_mic_age4")], na.rm = TRUE)
cases_new[which(is.na(cases_new$conf_rdt_age4) & is.na(cases_new$conf_mic_age4)), "conf_rdt_mic_age4"] <- NA
cases_new$conf_rdt_mic_u5 <- rowSums(cases_new[,c("conf_rdt_u5", "conf_mic_u5")], na.rm = TRUE)
cases_new[which(is.na(cases_new$conf_rdt_u5) & is.na(cases_new$conf_mic_u5)), "conf_rdt_mic_u5"] <- NA
cases_new$conf_rdt_mic_ov5 <- rowSums(cases_new[,c("conf_rdt_ov5", "conf_mic_ov5")], na.rm = TRUE)
cases_new[which(is.na(cases_new$conf_rdt_ov5) & is.na(cases_new$conf_mic_ov5)), "conf_rdt_mic_ov5"] <- NA
cases_new$conf_rdt_mic <- rowSums(cases_new[,c("conf_rdt_mic_u5", "conf_rdt_mic_ov5")], na.rm = TRUE)
cases_new[which(is.na(cases_new$conf_rdt_mic_u5) & is.na(cases_new$conf_rdt_mic_ov5)), "conf_rdt_mic"] <- NA


####################################################################


# fixing order of columns by alphabetizing
cases_new <- cases_new[,c(1:8, (order(names(cases_new)[9:ncol(cases_new)]) + 8))]


####################################################################



# saving
write.csv(cases_new, write_dir, row.names = FALSE)



