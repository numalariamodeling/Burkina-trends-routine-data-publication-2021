##############################################
##  Clean HF data before aggregating to DS  ##
##############################################
#
# Description:
#   Cleaning health facility data for exclusion criteria prior to aggregation to health district
#
#
#  Sebastian Rodriguez (sebastian@rodriguez.cr)
#  Last edited Oct 31, 2021
#




rm(list = ls(all = TRUE))

require("plyr")
require("dplyr")
require("zoo")
require("lubridate")



# Loading health facility dataset

HF_cases <- read.csv("~/Box/NU-malaria-team/projects/smc_impact/data/outputs/Ov5_HF_cases_smc_coords_imputed_rdts_and_allout_MA_activeHFs.csv", stringsAsFactors = FALSE)
HF_cases$Date <- as.yearmon(HF_cases$Date)



tmp_total_HF <- ddply(HF_cases, c(.(District), .(Date), .(month)), summarise,
                      total_HF = length(UID),
                      total_HFs_active = length(which(HF_status == "active")))



average_HF_counts_per_month <- ddply(HF_cases, c(.(District), .(UID), .(month)), summarise,
                                     na_counts_HF = sum(is.na(conf_rdt_ov5)),
                                     average_counts_HF = (sum(conf_rdt_mic_ov5, na.rm = T)/(4 - sum(is.na(conf_rdt_ov5)))),
                                     na_all_counts_HF = sum(is.na(allout_ov5)),
                                     average_all_counts_HF = (sum(allout_ov5, na.rm = T)/(4 - sum(is.na(allout_ov5)))))

average_HF_counts_per_month[is.nan(average_HF_counts_per_month$average_counts_HF), "average_counts_HF"] <- 0
average_HF_counts_per_month[is.infinite(average_HF_counts_per_month$average_counts_HF), "average_counts_HF"] <- 0

average_HF_counts_per_month[is.nan(average_HF_counts_per_month$average_all_counts_HF), "average_all_counts_HF"] <- 0
average_HF_counts_per_month[is.infinite(average_HF_counts_per_month$average_all_counts_HF), "average_all_counts_HF"] <- 0


average_District_counts_per_month <- ddply(average_HF_counts_per_month, c(.(District), .(month)), summarise,
                                           sum_avg_counts_HF = sum(average_counts_HF),
                                           sum_avg_all_counts_HF = sum(average_all_counts_HF),)


average_HF_counts_per_date <- left_join(average_HF_counts_per_month, tmp_total_HF,
                                        by = c("District", "month"))



## add active stuff here ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #

average_HF_counts_per_date <- left_join(average_HF_counts_per_date, HF_cases[, c("UID", "Date", "HF_status")],
                                        by = c("UID", "Date"))

average_HF_counts_per_date[which(average_HF_counts_per_date$HF_status == "inactive"), c("average_counts_HF", "average_all_counts_HF")] <- 0


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 


## This is still average per month, just doing by Date to account for active/inactive HFs

average_District_counts_per_month <- ddply(average_HF_counts_per_date, c(.(District), .(Date)), summarise,
                                           sum_avg_counts_HF = sum(average_counts_HF),
                                           sum_avg_all_counts_HF = sum(average_all_counts_HF),)



average_counts_per_date <- left_join(average_District_counts_per_month, average_HF_counts_per_date,
                                     by = c("District", "Date"))



average_counts_per_date$weights <- average_counts_per_date$average_counts_HF / average_counts_per_date$sum_avg_counts_HF
average_counts_per_date$weights_all_cause <- average_counts_per_date$average_all_counts_HF / average_counts_per_date$sum_avg_all_counts_HF



tmp_total_HF_weighted <- ddply(average_counts_per_date, c(.(District), .(Date)), summarise,
                               total_HF_weighted = sum(weights),
                               total_HF_weighted_all_cause = sum(weights_all_cause))



##################################################################################################


HF_cases <- left_join(HF_cases, average_counts_per_date[,c("UID", "Date", "weights", "weights_all_cause")],
                      by = c("UID", "Date"))


##################################################################################################



good_rows <- which(!is.na(HF_cases$conf_rdt_ov5) &
                       !is.na(HF_cases$allout_ov5) &
                       HF_cases$allout_ov5 != 0 &
                       HF_cases$conf_rdt_mic_ov5 <= HF_cases$allout_ov5)



HF_cases_good <- HF_cases[good_rows,]

bad_rows_age1 <- which(is.na(HF_cases_good$conf_rdt_age1) & !is.na(HF_cases_good$conf_rdt_age2) &
                           HF_cases_good$conf_rdt_age2 == HF_cases_good$conf_rdt_ov5)
bad_rows_age2 <- which(is.na(HF_cases_good$conf_rdt_age2) & !is.na(HF_cases_good$conf_rdt_age1) &
                           HF_cases_good$conf_rdt_age1 == HF_cases_good$conf_rdt_ov5)
HF_cases_good <- HF_cases_good[-c(bad_rows_age1, bad_rows_age2),]

# 44633 removed


HF_reporting_weighted <- ddply(HF_cases_good, c(.(District), .(Date)), summarise,
                               reporting_HF = length(UID),
                               reporting_HFs_active = length(which(HF_status == "active")),
                               reporting_HF_weighted = sum(weights),
                               reporting_HF_weighted_all_cause = sum(weights_all_cause))

HF_reporting_weighted <- left_join(HF_reporting_weighted, tmp_total_HF, by = c("District", "Date"))

HF_reporting_weighted$weighted_rep_rate <- HF_reporting_weighted$reporting_HF_weighted
HF_reporting_weighted$rep_rate_old <- HF_reporting_weighted$reporting_HF / HF_reporting_weighted$total_HF
HF_reporting_weighted$rep_rate <- HF_reporting_weighted$reporting_HFs_active / HF_reporting_weighted$total_HFs_active

HF_reporting_weighted$weighted_rep_rate_all_cause <- HF_reporting_weighted$reporting_HF_weighted_all_cause






##################################################################################################




D_cases <- ddply(HF_cases_good[,-c(11, 164:170)], 
                 c(.(Region), .(District), .(periodname), .(month), .(year), .(Date)),
                 numcolwise(sum, na.rm = TRUE))


# Get unique data for precip, air.temp, Pop, SMC_rec, num_children_smc we took off above
# merge with health district data
HF_cases_good$Number.of.children.treated.with.SMC <- round(HF_cases_good$Number.of.children.treated.with.SMC)
unique_rows <- unique(HF_cases_good[,c(1,3,7:10,11,164:168)])

D_cases <- left_join(D_cases, unique_rows,
                     by = c("Region", "District", "periodname", "month", "year", "Date"))







D_cases <- left_join(D_cases, HF_reporting_weighted, by = c("District", "Date", "month"))



# Saving

write.csv(D_cases, "~/Box/NU-malaria-team/projects/smc_impact/data/outputs/Ov5_DS_cases_seasonal_smc_good_rows_MA_imputes_w_rep_weights_activeHFs.csv", row.names = FALSE)



