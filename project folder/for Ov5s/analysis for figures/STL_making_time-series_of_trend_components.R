########################################################
##  Seasonal Trend Decomposition for ov-5s main text  ##
########################################################
#
# Description:
#   Making main text figures for Ov-5 trend component time-series
#
#
#
#
#  Sebastian Rodriguez (sebastian@rodriguez.cr)
#  Last edited Oct 31, 2021
#



rm(list = ls(all = TRUE))


require("ggplot2")
require("plyr")
require("dplyr")
require("zoo")
require("gridExtra")
require("lubridate")


require("stringr")
require("stlplus")
require("maditr")




# Function for standardizing malaria variables

getNormalized <- function(vec)
{
    norm_vec <- (vec - mean(vec))/sd(vec)
    return(norm_vec)
}




# Loading health district dataset
# Creating under-5 population column and fixing date column


cases <- read.csv("~/Box/NU-malaria-team/projects/smc_impact/data/outputs/Ov5_DS_cases_seasonal_smc_good_rows_MA_imputes_w_rep_weights_activeHFs.csv", header  = TRUE, strip.white = TRUE, stringsAsFactors = FALSE)
cases$Ov5_pop <- cases$District.Pop * .82
cases$Date <- as.yearmon(cases$Date)


cases <- cases[order(cases$District, cases$Date),]


################################################################################


cases$mal_cases <- cases$conf_rdt_mic_ov5 / (cases$Ov5_pop/1000)
cases$all_cases <- cases$allout_ov5 / (cases$Ov5_pop/1000)


################################################################################

## Loading DHS & MIS febrile treatment seeking rates

# Section for Linear functional estimates


med_2014 <- read.csv("~/OneDrive/Desktop/medfever_region_2014.csv", stringsAsFactors = FALSE)
med_2017 <- read.csv("~/OneDrive/Desktop/medfever_region_2017.csv", stringsAsFactors = FALSE)



med_2014 <- med_2014[,-4]
names(med_2014)[3] <- "medfever_2014"
med_2017 <- med_2017[,-4]
names(med_2017)[3] <- "medfever_2017"

medfever_DHS <- inner_join(med_2014, med_2017, by = c("NOMDEP", "NOMREGION"))
medfever_DHS <- cbind(medfever_DHS[,1:3], medfever_DHS[,4])
names(medfever_DHS) <- c("District", "Region", "Aug 2014", "Dec 2017")

medfever_DHS <- melt(medfever_DHS, id.vars = c("District", "Region"),
                     variable.name = "Date", value.name = "medfever_regional")



district_map <- cbind(sort(unique(cases$District)),
                      sort(unique(medfever_DHS$District))[c(1:40,42,41,43:70)])

for (i in 1:nrow(district_map))
{
    DS <- district_map[i, 2]
    new_name_DS <- district_map[i, 1]
    
    medfever_DHS[which(medfever_DHS$District == DS), "District"] <- new_name_DS
}

medfever_DHS$Date <- as.yearmon(medfever_DHS$Date)
medfever_DHS <- medfever_DHS[order(medfever_DHS$District, medfever_DHS$Date),]



medfever_DHS <- as.data.frame(medfever_DHS)

dates <- seq(as.Date("2014-08-01"), as.Date("2018-12-01"), by="months")
medfever_DHS_fitted <- data.frame()

for (D in unique(cases$District))
{
    D_data <- medfever_DHS[medfever_DHS$District == D,]
    D_data$ind <- c(1, 41)
    
    D_lm <- lm(medfever_regional ~ ind, data = D_data)
    D_fitted <- predict(D_lm, data.frame("ind" = c(1:53)))
    
    medfever_DHS_fitted_D <- data.frame("District" = rep(D, 53),
                                        "Date" = dates,
                                        "fitted_regional_medfever" = D_fitted)
    medfever_DHS_fitted <- rbind(medfever_DHS_fitted, medfever_DHS_fitted_D)
}
medfever_DHS_fitted$Date <- as.yearmon(medfever_DHS_fitted$Date)



cases <- left_join(cases, medfever_DHS_fitted, by = c("District", "Date"))

cases$cases_linear_trtseeking_adj_raw <- (cases$conf_rdt_mic_ov5 / cases$fitted_regional_medfever) / cases$weighted_rep_rate
cases$cases_linear_trtseeking_adj <- cases$cases_linear_trtseeking_adj_raw / (cases$Ov5_pop / 1000)


################################################################################


# Section for step-function estimates

medfever_DHS <- inner_join(med_2014, med_2017, by = c("NOMDEP", "NOMREGION"))
medfever_DHS <- cbind(medfever_DHS[,1:3], medfever_DHS[,3], medfever_DHS[,4], medfever_DHS[,4])
names(medfever_DHS) <- c("District", "Region", "2015", "2016", "2017", "2018")

medfever_DHS <- melt(medfever_DHS, id.vars = c("District", "Region"),
                     variable.name = "year", value.name = "medfever_regional")



district_map <- cbind(sort(unique(cases$District)),
                      sort(unique(medfever_DHS$District))[c(1:40,42,41,43:70)])

for (i in 1:nrow(district_map))
{
    DS <- district_map[i, 2]
    new_name_DS <- district_map[i, 1]
    
    medfever_DHS[which(medfever_DHS$District == DS), "District"] <- new_name_DS
}

medfever_DHS$year <- as.numeric(as.character(medfever_DHS$year))



cases <- left_join(cases, medfever_DHS[,-2], by = c("District", "year"))

cases$medfever_regional_step1 <- cases$medfever_regional
for (D in unique(cases$District))
{
    cases[which(cases$District == D & cases$Date %in% as.yearmon(seq(as.Date("2016-05-01"), as.Date("2016-12-01"), by="months"))), "medfever_regional_step1"] <- cases[which(cases$District == D & cases$Date == "Jan 2017"), "medfever_regional_step1"]
}


cases$cases_step1_trtseeking_adj_raw <- (cases$conf_rdt_mic_ov5 / cases$medfever_regional_step1) / cases$weighted_rep_rate
cases$cases_step1_trtseeking_adj <- cases$cases_step1_trtseeking_adj_raw / (cases$Ov5_pop / 1000)

cases$cases_step2_trtseeking_adj_raw <- (cases$conf_rdt_mic_ov5 / cases$medfever_regional) / cases$weighted_rep_rate
cases$cases_step2_trtseeking_adj <- cases$cases_step2_trtseeking_adj_raw / (cases$Ov5_pop / 1000)




################################################################################




cases$cases_rep_weighted_adj_raw <- cases$conf_rdt_mic_ov5 / cases$weighted_rep_rate
cases$cases_rep_weighted_adj <- cases$cases_rep_weighted_adj_raw / (cases$Ov5_pop / 1000)


cases$all_cases_rep_weighted_adj <- cases$all_cases / cases$weighted_rep_rate_all_cause
cases$all_nonMal_cases_rep_weighted_adj <- cases$all_cases_rep_weighted_adj - cases$cases_rep_weighted_adj

cases$mal_ratio_weighted <- cases$cases_rep_weighted_adj / cases$all_cases_rep_weighted_adj




################################################################################



## Fixing pouytenga district for NA value in Nov 2017
# pouytenga has NA cases in Nov 2017
# Imputing with mean of previous and subsequent months

pouytenga_ind_Oct_2017 <- which(cases$Date == "Oct 2017" & cases$District == "pouytenga")
pouytenga_ind_Dec_2017 <- which(cases$Date == "Dec 2017" & cases$District == "pouytenga")


mal_cases_pouytenga_Nov_2017 <- mean(c(cases$mal_cases[pouytenga_ind_Oct_2017],
                                       cases$mal_cases[pouytenga_ind_Dec_2017]))
all_cases_pouytenga_Nov_2017 <- mean(c(cases$all_cases[pouytenga_ind_Oct_2017],
                                       cases$all_cases[pouytenga_ind_Dec_2017]))
all_nonMal_cases_pouytenga_Nov_2017 <- mean(c(cases$all_nonMal_cases[pouytenga_ind_Oct_2017],
                                              cases$all_nonMal_cases[pouytenga_ind_Dec_2017]))
mal_ratio_pouytenga_Nov_2017 <- mean(c(cases$mal_ratio[pouytenga_ind_Oct_2017],
                                       cases$mal_ratio[pouytenga_ind_Dec_2017]))

linear_trt_adj_pouytenga_Nov_2017 <- mean(c(cases$cases_linear_trtseeking_adj[pouytenga_ind_Oct_2017],
                                            cases$cases_linear_trtseeking_adj[pouytenga_ind_Dec_2017]))
step1_trt_adj_pouytenga_Nov_2017 <- mean(c(cases$cases_step1_trtseeking_adj[pouytenga_ind_Oct_2017],
                                           cases$cases_step1_trtseeking_adj[pouytenga_ind_Dec_2017]))
step2_trt_adj_pouytenga_Nov_2017 <- mean(c(cases$cases_step2_trtseeking_adj[pouytenga_ind_Oct_2017],
                                           cases$cases_step2_trtseeking_adj[pouytenga_ind_Dec_2017]))
weight_rep_adj_pouytenga_Nov_2017 <- mean(c(cases$cases_rep_weighted_adj[pouytenga_ind_Oct_2017],
                                            cases$cases_rep_weighted_adj[pouytenga_ind_Dec_2017]))

mal_ratio_weighted_pouytenga_Nov_2017 <- mean(c(cases$mal_ratio_weighted[pouytenga_ind_Oct_2017],
                                                cases$mal_ratio_weighted[pouytenga_ind_Dec_2017]))
all_cases_weight_rep_adj_pouytenga_Nov_2017 <- mean(c(cases$all_cases_rep_weighted_adj[pouytenga_ind_Oct_2017],
                                                      cases$all_cases_rep_weighted_adj[pouytenga_ind_Dec_2017]))
all_nonMal_weight_rep_adj_pouytenga_Nov_2017 <- mean(c(cases$all_nonMal_cases_rep_weighted_adj[pouytenga_ind_Oct_2017],
                                                       cases$all_nonMal_cases_rep_weighted_adj[pouytenga_ind_Dec_2017]))


pouytenga_Nov_2017_row_tmp <- cases[pouytenga_ind_Oct_2017,]
pouytenga_Nov_2017_row_tmp$month <- 12
pouytenga_Nov_2017_row_tmp$Date <- as.yearmon("Nov 2017")
pouytenga_Nov_2017_row_tmp$mal_cases <- mal_cases_pouytenga_Nov_2017
pouytenga_Nov_2017_row_tmp$cases_linear_trtseeking_adj <- linear_trt_adj_pouytenga_Nov_2017
pouytenga_Nov_2017_row_tmp$cases_step1_trtseeking_adj <- step1_trt_adj_pouytenga_Nov_2017
pouytenga_Nov_2017_row_tmp$cases_step2_trtseeking_adj <- step2_trt_adj_pouytenga_Nov_2017
pouytenga_Nov_2017_row_tmp$cases_rep_weighted_adj <- weight_rep_adj_pouytenga_Nov_2017
pouytenga_Nov_2017_row_tmp$mal_ratio_weighted <- mal_ratio_weighted_pouytenga_Nov_2017
pouytenga_Nov_2017_row_tmp$all_cases_rep_weighted_adj <- all_cases_weight_rep_adj_pouytenga_Nov_2017
pouytenga_Nov_2017_row_tmp$all_nonMal_cases_rep_weighted_adj <- all_nonMal_weight_rep_adj_pouytenga_Nov_2017



cases <- rbind(cases[1:pouytenga_ind_Oct_2017,],
               pouytenga_Nov_2017_row_tmp,
               cases[pouytenga_ind_Dec_2017:nrow(cases),])






############################################################################



cases$mal_cases_norm <- getNormalized(cases$mal_cases)
cases$cases_linear_trtseeking_adj_norm <- getNormalized(cases$cases_linear_trtseeking_adj)
cases$cases_step1_trtseeking_adj_norm <- getNormalized(cases$cases_step1_trtseeking_adj)
cases$cases_step2_trtseeking_adj_norm <- getNormalized(cases$cases_step2_trtseeking_adj)
cases$cases_rep_weighted_adj_norm <- getNormalized(cases$cases_rep_weighted_adj)
cases$mal_ratio_weighted_norm <- getNormalized(cases$mal_ratio_weighted)
cases$all_cases_rep_weighted_adj_norm <- getNormalized(cases$all_cases_rep_weighted_adj)
cases$all_nonMal_rep_weighted_adj_norm <- getNormalized(cases$all_nonMal_cases_rep_weighted_adj)


############################################################################





# Performing STL on each individual district
# For both malaria variables

STL_result_DF <- data.frame()

for (DS in sort(unique(cases$District)))
{
    cases_dist <- cases[which(cases$District == DS),]
    cases_dist <- cases_dist[order(cases_dist$Date),]
    
 
    
    mal_cases_ts <- ts(cases_dist$mal_cases_norm, start = c(2015, 1), deltat = 1/12)
    cases_stl <- stlplus(mal_cases_ts, s.window = "periodic")

    
    cases_linear_trtseeking_adj_ts <- ts(cases_dist$cases_linear_trtseeking_adj_norm, start = c(2015, 1), deltat = 1/12)
    cases_linear_trtseeking_adj_stl <- stlplus(cases_linear_trtseeking_adj_ts, s.window = "periodic")
    
    
    cases_step1_trtseeking_adj_ts <- ts(cases_dist$cases_step1_trtseeking_adj_norm, start = c(2015, 1), deltat = 1/12)
    cases_step1_trtseeking_adj_stl <- stlplus(cases_step1_trtseeking_adj_ts, s.window = "periodic")
    
    
    cases_step2_trtseeking_adj_ts <- ts(cases_dist$cases_step2_trtseeking_adj_norm, start = c(2015, 1), deltat = 1/12)
    cases_step2_trtseeking_adj_stl <- stlplus(cases_step2_trtseeking_adj_ts, s.window = "periodic")
    
    
    cases_rep_weighted_adj_ts <- ts(cases_dist$cases_rep_weighted_adj_norm, start = c(2015, 1), deltat = 1/12)
    cases_rep_weighted_adj_stl <- stlplus(cases_rep_weighted_adj_ts, s.window = "periodic")
    
    
    mal_ratio_weighted_ts <- ts(cases_dist$mal_ratio_weighted_norm, start = c(2015, 1), deltat = 1/12)
    ratio_weighted_stl <- stlplus(mal_ratio_weighted_ts, s.window="periodic")
    
    
    all_cases_rep_weighted_adj_ts <- ts(cases_dist$all_cases_rep_weighted_adj_norm, start = c(2015, 1), deltat = 1/12)
    all_cases_rep_weighted_adj_stl <- stlplus(all_cases_rep_weighted_adj_ts, s.window="periodic")
    
    
    all_nonMal_rep_weighted_adj_ts <- ts(cases_dist$all_nonMal_rep_weighted_adj_norm, start = c(2015, 1), deltat = 1/12)
    all_nonMal_rep_weighted_adj_stl <- stlplus(all_nonMal_rep_weighted_adj_ts, s.window="periodic")
    
    
    
    # Assembling data frame for decompositon of all districts
    
    dates <- seq(as.Date("2015-01-01"), as.Date("2018-12-01"), by = "months")
    
    
    cases_stl_ts <- as.data.frame(cases_stl$data[,1:4])
    cases_stl_ts$type <- rep("cases", nrow(cases_stl_ts))
    cases_stl_ts$dates <- dates
    
    
    cases_linear_trtseeking_adj_stl_ts <- as.data.frame(cases_linear_trtseeking_adj_stl$data[,1:4])
    cases_linear_trtseeking_adj_stl_ts$type <- rep("linear_trtseeking_adj", nrow(cases_linear_trtseeking_adj_stl_ts))
    cases_linear_trtseeking_adj_stl_ts$dates <- dates
    
    
    cases_step1_trtseeking_adj_stl_ts <- as.data.frame(cases_step1_trtseeking_adj_stl$data[,1:4])
    cases_step1_trtseeking_adj_stl_ts$type <- rep("step1_trtseeking_adj", nrow(cases_step1_trtseeking_adj_stl_ts))
    cases_step1_trtseeking_adj_stl_ts$dates <- dates
    
    
    cases_step2_trtseeking_adj_stl_ts <- as.data.frame(cases_step2_trtseeking_adj_stl$data[,1:4])
    cases_step2_trtseeking_adj_stl_ts$type <- rep("step2_trtseeking_adj", nrow(cases_step2_trtseeking_adj_stl_ts))
    cases_step2_trtseeking_adj_stl_ts$dates <- dates
    
    
    cases_rep_weighted_adj_stl_ts <- as.data.frame(cases_rep_weighted_adj_stl$data[,1:4])
    cases_rep_weighted_adj_stl_ts$type <- rep("rep_weighted_adj", nrow(cases_rep_weighted_adj_stl_ts))
    cases_rep_weighted_adj_stl_ts$dates <- dates
    

    ratio_weighted_stl_ts <- as.data.frame(ratio_weighted_stl$data[,1:4]) 
    ratio_weighted_stl_ts$type <- rep("ratio_weighted", nrow(ratio_weighted_stl_ts))
    ratio_weighted_stl_ts$dates <- dates
    
    
    all_cases_rep_weighted_adj_stl_ts <- as.data.frame(all_cases_rep_weighted_adj_stl$data[,1:4]) 
    all_cases_rep_weighted_adj_stl_ts$type <- rep("allout_rep_weighted_adj", nrow(all_cases_rep_weighted_adj_stl_ts))
    all_cases_rep_weighted_adj_stl_ts$dates <- dates
    
    
    all_nonMal_rep_weighted_adj_stl_ts <- as.data.frame(all_nonMal_rep_weighted_adj_stl$data[,1:4]) 
    all_nonMal_rep_weighted_adj_stl_ts$type <- rep("all_nonMal_rep_weighted_adj", nrow(all_nonMal_rep_weighted_adj_stl_ts))
    all_nonMal_rep_weighted_adj_stl_ts$dates <- dates
    
    
    
    
    STL_result_DF_DS <- rbind(cases_stl_ts,
                              cases_rep_weighted_adj_stl_ts,
                              ratio_weighted_stl_ts,
                              all_cases_rep_weighted_adj_stl_ts,
                              all_nonMal_rep_weighted_adj_stl_ts)
    STL_result_DF_DS$District <- DS
    STL_result_DF_DS$Region <- unique(cases_dist$Region)
    STL_result_DF <- rbind(STL_result_DF,
                           STL_result_DF_DS)
    
    
}





################################################################################


STL_result_DF$type <- factor(STL_result_DF$type,
                             levels = c("cases", "rep_weighted_adj",
                                        "allout_rep_weighted_adj",
                                        "all_nonMal_rep_weighted_adj",
                                        "ratio_weighted"))


ggplot(data = STL_result_DF, aes(x = dates, y = trend)) +
    geom_line(aes(group = District),
              show.legend = FALSE, col = "blue") + ylab("") + 
    facet_wrap(~type, ncol = 3, scales = "free_y") +
    scale_x_yearmon("", breaks = sort(unique(STL_result_DF$dates))[c(seq(1,48,6), 48)],
                    labels = as.yearmon(sort(unique(STL_result_DF$dates))[c(seq(1,48,6), 48)])) +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1),
                       panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))





################################################################################
# 
# 
# 
# for (i in seq(1,70,4))
# {
#     # Select 4 district for plotting
# 
# 
#     DS_list <- sort(unique(STL_result_DF$District))[i:(i+3)]
# 
#     plotting_DF <- STL_result_DF[which(STL_result_DF$District %in% DS_list),]
# 
# 
#     plot_trend <- ggplot(plotting_DF, aes(x = dates, y = trend)) +
#         geom_line(aes(group = type, linetype = "solid",
#                       color = factor(type, levels = c("cases", "rep_weighted_adj",
#                                                       "allout_rep_weighted_adj", "all_nonMal_rep_weighted_adj",
#                                                       "ratio_weighted"))),
#                   show.legend = F) +
#         scale_color_manual("", values = c("#913058", "#F6851F",
#                                           "#5393C3", "#F1A31F", "#98B548")) + scale_linetype_identity("") +
#         xlab("") + facet_wrap(~District, ncol = 4) +
#         theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#               panel.background = element_blank(), axis.line = element_line(colour = "black"),
#               panel.spacing.x = unit(6, "mm"))
# 
# 
# 
# 
#     # Saving SI plots
#     pdf(paste("~/OneDrive/Desktop/SI_figures_STL_paper_ov5/SI_decomp_", i, ".pdf", sep = ""))
#     print(plot_trend)
#     dev.off()
# 

}



