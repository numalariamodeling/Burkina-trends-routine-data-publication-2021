#######################################################
##  Plotting health facility reporting for figure 1  ##
#######################################################
#
# Description:
#   Making Figure 1A on health facility reports received (complete or not)
#
#
#  Sebastian Rodriguez (sebastian@rodriguez.cr)
#  Last edited Oct 31, 2021
#





rm(list = ls(all = TRUE))


library("ggplot2")
library("plyr")
library("dplyr")
library("zoo")
library("gridExtra")

library("lubridate")
library("reshape2")


## Grabbing data and cleaning for making figure 1A


HF_cases <- read.csv("~/Box/NU-malaria-team/projects/smc_impact/data/outputs/U5_HF_cases_smc_coords_imputed_rdts_and_allout_MA_activeHFs.csv")


HF_cases$Date <- as.Date(as.yearmon(HF_cases$Date))
HF_cases <- HF_cases[order(HF_cases$UID, HF_cases$Date),]



HFs_not_reporting <- which((is.na(HF_cases$conf_rdt_u5) & is.na(HF_cases$allout_u5)) |
                               (HF_cases$conf_rdt_mic_u5 == 0 & HF_cases$allout_u5 == 0) |
                               (HF_cases$conf_rdt_mic_u5 == 0 & is.na(HF_cases$allout_u5)))

HF_cases$HF_reporting <- 1
HF_cases[HFs_not_reporting, "HF_reporting"] <- 0



# Find rows which fail to meet the inclusion criteria
# These rows are considered "incomplete" or "inconsistent" reports/rows
bad_rows_age1 <- which(is.na(HF_cases$conf_rdt_age1) & !is.na(HF_cases$conf_rdt_age2) &
                           !is.na(HF_cases$conf_rdt_u5) &
                           HF_cases$conf_rdt_age2 == HF_cases$conf_rdt_u5)
bad_rows_age2 <- which(is.na(HF_cases$conf_rdt_age2) & !is.na(HF_cases$conf_rdt_age1) &
                           !is.na(HF_cases$conf_rdt_u5) &
                           HF_cases$conf_rdt_age1 == HF_cases$conf_rdt_u5)
bad_rows_u5 <- which(is.na(HF_cases$conf_rdt_u5) |
                         is.na(HF_cases$allout_u5) |
                         HF_cases$allout_u5 == 0 |
                         HF_cases$conf_rdt_mic_u5 > HF_cases$allout_u5)

bad_rows <- unique(c(bad_rows_age1, bad_rows_age2, bad_rows_u5))


HF_cases$good_obs <- 1
HF_cases[bad_rows, "good_obs"] <- 0

reporting_DF <- ddply(HF_cases, .(Date), summarize,
                      "good reports" = sum(good_obs),
                      "any reports" = sum(HF_reporting),
                      total_facilities = length(unique(UID)),
                      "active HFs" = table(HF_status)[1])
reporting_DF$Date <- as.Date(as.yearmon(reporting_DF$Date))
reporting_DF <- reporting_DF[order(reporting_DF$Date),]







reporting_DF_melted <- melt(reporting_DF[,-4], id = "Date")

reporting_DF_melted$variable <- factor(as.factor(reporting_DF_melted$variable),
                                       levels = c("any reports", "good reports", "active HFs"))




## Figure 1


ggplot(reporting_DF_melted,
       aes(x = Date, y = value, color = as.factor(variable))) +
    geom_line(size = 1) + scale_color_manual(values = c("black", "darkgreen", "blue"),
                                     breaks = c("any reports", "good reports", "active HFs"),
                                     name = "") +
    scale_x_yearmon("", breaks = sort(unique(reporting_DF_melted$Date))[c(seq(1,48,6), 48)],
                    labels = as.yearmon(sort(unique(reporting_DF_melted$Date))[c(seq(1,48,6), 48)])) +
    scale_y_continuous(name = "Number of health facilities with complete reporting") +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))





