##########################################
##  Plotting district level timeseries  ##
##########################################
#
# Description:
#   Making timeseries figures for figure 1, 2, and 4
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
library("maditr")



cases <- read.csv("~/Box/NU-malaria-team/projects/smc_impact/data/outputs/U5_DS_cases_seasonal_smc_good_rows_MA_imputes_w_rep_weights_activeHFs.csv", header  = TRUE, strip.white = TRUE, stringsAsFactors = FALSE)
cases$U5_pop <- cases$District.Pop * .18
cases$Date <- as.yearmon(cases$Date)



##################################################################################################


## Figure 1D

ggplot(cases, aes(x = Date, y = rep_rate,
                  group = as.factor(District))) +
    geom_line(alpha = 0.25, size = 1, show.legend = FALSE, color = "blue") + ylab("") +
    ggtitle("Health facility reporting rate by District") +
    scale_x_yearmon("Date", breaks = sort(unique(cases$Date))[c(seq(1,48,6), 48)],
                    labels = sort(unique(cases$Date))[c(seq(1,48,6), 48)]) +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1),
                       panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


## Figure 1E

ggplot(cases, aes(x = Date, y = weighted_rep_rate,
                  group = as.factor(District))) +
    geom_line(alpha = 0.25, size = 1, show.legend = FALSE, color = "blue") + ylab("") +
    ggtitle("Health facility weighted reporting rate by District") +
    scale_x_yearmon("Date", breaks = sort(unique(cases$Date))[c(seq(1,48,6), 48)],
                    labels = sort(unique(cases$Date))[c(seq(1,48,6), 48)]) +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1),
                       panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



##################################################################################################


## Figure 2A

ggplot(cases, aes(x = Date, y = conf_rdt_mic_u5 / (U5_pop/1000),
                  group = as.factor(District))) +
    geom_line(alpha = 0.25, size = 1, show.legend = FALSE, color = "blue") + ylab("Test positivity rate") +
    ggtitle("") +
    scale_x_yearmon("Date", breaks = sort(unique(cases$Date))[c(seq(1,48,6), 48)],
                    labels = sort(unique(cases$Date))[c(seq(1,48,6), 48)]) +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1),
                       panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))





## Figure 2D

cases$cases_rep_weighted_adj <- cases$conf_rdt_mic_u5 / cases$weighted_rep_rate

ggplot(cases, aes(x = Date, y = cases_rep_weighted_adj / (U5_pop/1000),
                  group = as.factor(District))) +
    geom_line(alpha = 0.25, size = 1, show.legend = FALSE, color = "blue") + ylab("Cases per 1000") +
    ggtitle("Reporting adjusted malaria cases among u5 (w/ rep weights)") +
    scale_x_yearmon("Date", breaks = sort(unique(cases$Date))[c(seq(1,48,6), 48)],
                    labels = sort(unique(cases$Date))[c(seq(1,48,6), 48)]) +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1),
                       panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))





##################################################################################################



cases$allout_rep_weighted_adj <- cases$allout_u5 / cases$weighted_rep_rate_all_cause



## Figure 4A

ggplot(cases, aes(x = Date, y = allout_rep_weighted_adj / (U5_pop/1000),
                  group = as.factor(District))) +
    geom_line(alpha = 0.25, size = 1, show.legend = FALSE, color = "blue") + ylab("Cases per 1000") +
    ggtitle("All-cause outpatient visits among u5 (w/ rep weights)") +
    scale_x_yearmon("Date", breaks = sort(unique(cases$Date))[c(seq(1,48,6), 48)],
                    labels = sort(unique(cases$Date))[c(seq(1,48,6), 48)]) +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1),
                       panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))






## Figure 4B

ggplot(cases, aes(x = Date, y = (allout_rep_weighted_adj - cases_rep_weighted_adj) / (U5_pop/1000),
                  group = as.factor(District))) +
    geom_line(alpha = 0.25, size = 1, show.legend = FALSE, color = "blue") + ylab("Cases per 1000") +
    ggtitle("Non-malarial outpatient visits among u5 (w/ rep weights)") +
    scale_x_yearmon("Date", breaks = sort(unique(cases$Date))[c(seq(1,48,6), 48)],
                    labels = sort(unique(cases$Date))[c(seq(1,48,6), 48)]) +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1),
                       panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))







## Figure 4C

ggplot(cases, aes(x = Date, y = cases_rep_weighted_adj / allout_rep_weighted_adj,
                  group = as.factor(District))) +
    geom_line(alpha = 0.25, size = 1, show.legend = FALSE, color = "blue") + ylab("") +
    ggtitle("Health facility weighted reporting rate by District") +
    scale_x_yearmon("", breaks = sort(unique(cases$Date))[c(seq(1,48,6), 48)],
                    labels = sort(unique(cases$Date))[c(seq(1,48,6), 48)]) +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1),
                       panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))






