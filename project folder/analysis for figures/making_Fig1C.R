#######################################################
##  Plotting health facility reporting for figure 1  ##
#######################################################
#
# Description:
#   Making Figure 1C on health facility reports received (complete or not)
#
#
#  Sebastian Rodriguez (sebastian@rodriguez.cr)
#  Last edited Oct 31, 2021
#






rm(list = ls(all = TRUE))

require("plyr")
require("dplyr")
require("zoo")
require("reshape2")
require("ggplot2")


# Loading health facility dataset

HF_cases <- read.csv("~/Box/NU-malaria-team/projects/smc_impact/data/outputs/U5_HF_cases_smc_coords_imputed_rdts_and_allout_MA_activeHFs.csv", stringsAsFactors = FALSE)


HF_cases$Date <- as.Date(as.yearmon(HF_cases$Date))
HF_cases <- HF_cases[order(HF_cases$UID, HF_cases$Date),]

HF_cases <- HF_cases[which(HF_cases$HF_status == "active"),]


#########################################################################################



bad_rows_rdt_age1 <- which(is.na(HF_cases$conf_rdt_age1) & !is.na(HF_cases$conf_rdt_age2) &
                               !is.na(HF_cases$conf_rdt_u5) &
                               HF_cases$conf_rdt_age2 == HF_cases$conf_rdt_u5)
bad_rows_rdt_age2 <- which(is.na(HF_cases$conf_rdt_age2) & !is.na(HF_cases$conf_rdt_age1) &
                               !is.na(HF_cases$conf_rdt_u5) &
                               HF_cases$conf_rdt_age1 == HF_cases$conf_rdt_u5)
bad_rows_rdt_u5 <- which(is.na(HF_cases$conf_rdt_u5))

bad_rdt_rows <- unique(c(bad_rows_rdt_age1, bad_rows_rdt_age2, bad_rows_rdt_u5))


bad_rows_allout <- which(is.na(HF_cases$allout_u5) | HF_cases$allout_u5 == 0)


bad_rows_inconsistent <- which(HF_cases$conf_rdt_mic_u5 > HF_cases$allout_u5)


bad_rows_missing <- unique(c(bad_rdt_rows, bad_rows_allout))

bad_rows_all_2 <- unique(c(bad_rows_missing, bad_rows_inconsistent))

bad_rows_missing_allout_only <- bad_rows_allout[which(!(bad_rows_allout %in% bad_rows_inconsistent) &
                                                          !(bad_rows_allout %in% bad_rdt_rows))]
bad_rows_missing_rdt_only <- bad_rdt_rows[which(!(bad_rdt_rows %in% bad_rows_inconsistent) &
                                                          !(bad_rdt_rows %in% bad_rows_allout))]
bad_rows_missing_both <- bad_rows_missing[which(!(bad_rows_missing %in% bad_rows_missing_allout_only) &
                                                    !(bad_rows_missing %in% bad_rows_missing_rdt_only))]
bad_rows_inconsistent_only <- bad_rows_inconsistent[which(!(bad_rows_inconsistent %in% bad_rows_missing))]


HF_cases_area <- HF_cases[,c("Region", "UID", "Date")]

HF_cases_area$bad_rows <- "good report"
HF_cases_area[bad_rows_missing_allout_only, "bad_rows"] <- "missing all-cause outpatient visits"
HF_cases_area[bad_rows_missing_rdt_only, "bad_rows"] <- "missing confirmed malaria cases"
HF_cases_area[bad_rows_missing_both, "bad_rows"] <- "missing both values"
HF_cases_area[bad_rows_inconsistent_only, "bad_rows"] <- "inconsistent report"


HF_cases_area_casted <- dcast(HF_cases_area, Region + Date ~ bad_rows)
HF_cases_area_casted$total_HFs <- rowSums(HF_cases_area_casted[,3:7])
HF_cases_area_by_Region <- melt(HF_cases_area_casted, id.vars = c("Region", "Date", "total_HFs"),
                                variable.name = "Type", value.name = "reports")
HF_cases_area_by_Region$Type <- factor(HF_cases_area_by_Region$Type,
                                       levels = c("inconsistent report",
                                                  "missing both values",
                                                  "missing confirmed malaria cases",
                                                  "missing all-cause outpatient visits",
                                                  "good report"))



ggplot(HF_cases_area_by_Region, aes(fill = Type, y = reports/total_HFs, x = Date))  + 
    geom_area() +
    scale_x_date("", expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    facet_wrap(~Region, nrow = 2) +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), panel.spacing.x = unit(2, "mm"))







###################################################################################################




HF_cases_area <- HF_cases[,c("Region", "District", "UID", "Date")]

HF_cases_area$bad_rows <- "good report"
HF_cases_area[bad_rows_missing_allout_only, "bad_rows"] <- "missing all-cause outpatient visits"
HF_cases_area[bad_rows_missing_rdt_only, "bad_rows"] <- "missing confirmed malaria cases"
HF_cases_area[bad_rows_missing_both, "bad_rows"] <- "missing both values"
HF_cases_area[bad_rows_inconsistent_only, "bad_rows"] <- "inconsistent report"


HF_cases_area_casted <- dcast(HF_cases_area, Region + District + Date ~ bad_rows)
HF_cases_area_casted$total_HFs <- rowSums(HF_cases_area_casted[,4:8])
HF_cases_area_by_District <- melt(HF_cases_area_casted, id.vars = c("Region", "District", "Date", "total_HFs"),
                                  variable.name = "Type", value.name = "reports")
HF_cases_area_by_District$Type <- factor(HF_cases_area_by_District$Type,
                                         levels = c("inconsistent report",
                                                    "missing both values",
                                                    "missing confirmed malaria cases",
                                                    "missing all-cause outpatient visits",
                                                    "good report"))





















