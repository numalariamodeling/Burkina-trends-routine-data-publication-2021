################################
##  Sens slope for SI figures ##
################################
#
# Description:
#   Making Sens slope figures for SI figures and main text too
#   Using this to make TPR and testing maps
#
#
#
#  Sebastian Rodriguez (sebastian@rodriguez.cr)
#  Last edited Oct 31, 2021
#
#



rm(list = ls(all = TRUE))


require("ggplot2")
require("plyr")
require("dplyr")
require("zoo")
require("gridExtra")
require("lubridate")


require("stlplus")
require("reshape2")
require("trend")


require("RColorBrewer")
require("spdep")
require("rgdal")
require("sf")
require("rgeos")



# Function for standardizing malaria variables

getNormalized <- function(vec)
{
    norm_vec <- (vec - mean(vec))/sd(vec)
    return(norm_vec)
}






# Loading health district dataset
# Creating under-5 population column and fixing date column

cases <- read.csv("~/Box/NU-malaria-team/projects/smc_impact/data/outputs/U5_DS_cases_seasonal_smc_good_rows_MA_imputes_testing_w_rep_weights.csv", header  = TRUE, strip.white = TRUE, stringsAsFactors = FALSE)
cases$U5_pop <- cases$District.Pop * .18
cases$Date <- as.yearmon(cases$Date)




################################################################################




cases <- cases[order(cases$District, cases$Date),]


cases$mal_cases <- cases$conf_rdt_mic_u5 / (cases$U5_pop/1000)
cases$mal_test <- cases$test_rdt_mic_u5 / (cases$U5_pop/1000)
cases$all_cases <- cases$allout_u5 / (cases$U5_pop/1000)





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

cases$cases_linear_trtseeking_adj <- (cases$mal_cases / cases$fitted_regional_medfever) / cases$weighted_rep_rate




################################################################################

# Section for both Step-function estimates

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


cases$cases_step1_trtseeking_adj <- (cases$mal_cases / cases$medfever_regional_step1) / cases$weighted_rep_rate

cases$cases_step2_trtseeking_adj <- (cases$mal_cases / cases$medfever_regional) / cases$weighted_rep_rate



cases$cases_rep_weighted_adj <- cases$mal_cases / cases$weighted_rep_rate
cases$tests_rep_weighted_adj <- cases$mal_test / cases$weighted_rep_rate
cases$TPR <- cases$conf_rdt_mic_u5 / cases$test_rdt_mic_u5


cases$all_cases_rep_weighted_adj <- cases$all_cases / cases$weighted_rep_rate_all_cause
cases$all_nonMal_cases_rep_weighted_adj <- cases$all_cases_rep_weighted_adj - cases$cases_rep_weighted_adj




cases$mal_ratio_weighted <- cases$cases_rep_weighted_adj / cases$all_cases_rep_weighted_adj



################################################################################


## Fixing pouytenga district for NA value in Nov 2016
# pouytenga has NA cases in Nov 2016
# Imputing with mean of previous and subsequent months

pouytenga_ind_Nov_2017 <- which(cases$Date == "Nov 2017" & cases$District == "pouytenga")
pouytenga_ind_Jan_2018 <- which(cases$Date == "Jan 2018" & cases$District == "pouytenga")


mal_cases_pouytenga_Dec_2017 <- mean(c(cases$mal_cases[pouytenga_ind_Nov_2017],
                                       cases$mal_cases[pouytenga_ind_Jan_2018]))

linear_trt_adj_pouytenga_Dec_2017 <- mean(c(cases$cases_linear_trtseeking_adj[pouytenga_ind_Nov_2017],
                                            cases$cases_linear_trtseeking_adj[pouytenga_ind_Jan_2018]))
step1_trt_adj_pouytenga_Dec_2017 <- mean(c(cases$cases_step1_trtseeking_adj[pouytenga_ind_Nov_2017],
                                           cases$cases_step1_trtseeking_adj[pouytenga_ind_Jan_2018]))
step2_trt_adj_pouytenga_Dec_2017 <- mean(c(cases$cases_step2_trtseeking_adj[pouytenga_ind_Nov_2017],
                                           cases$cases_step2_trtseeking_adj[pouytenga_ind_Jan_2018]))

weight_rep_adj_pouytenga_Dec_2017 <- mean(c(cases$cases_rep_weighted_adj[pouytenga_ind_Nov_2017],
                                            cases$cases_rep_weighted_adj[pouytenga_ind_Jan_2018]))
mal_tests_pouytenga_Dec_2017 <- mean(c(cases$tests_rep_weighted_adj[pouytenga_ind_Nov_2017],
                                       cases$tests_rep_weighted_adj[pouytenga_ind_Jan_2018]))
TPR_pouytenga_Dec_2017 <- mean(c(cases$TPR[pouytenga_ind_Nov_2017],
                                 cases$TPR[pouytenga_ind_Jan_2018]))

all_cases_weight_rep_adj_pouytenga_Dec_2017 <- mean(c(cases$all_cases_rep_weighted_adj[pouytenga_ind_Nov_2017],
                                                      cases$all_cases_rep_weighted_adj[pouytenga_ind_Jan_2018]))
all_nonMal_weight_rep_adj_pouytenga_Dec_2017 <- mean(c(cases$all_nonMal_cases_rep_weighted_adj[pouytenga_ind_Nov_2017],
                                                       cases$all_nonMal_cases_rep_weighted_adj[pouytenga_ind_Jan_2018]))
mal_ratio_weighted_pouytenga_Dec_2017 <- mean(c(cases$mal_ratio_weighted[pouytenga_ind_Nov_2017],
                                                cases$mal_ratio_weighted[pouytenga_ind_Jan_2018]))



pouytenga_Dec_2017_row_tmp <- cases[pouytenga_ind_Nov_2017,]
pouytenga_Dec_2017_row_tmp$month <- 12
pouytenga_Dec_2017_row_tmp$Date <- as.yearmon("Dec 2017")
pouytenga_Dec_2017_row_tmp$mal_cases <- mal_cases_pouytenga_Dec_2017
pouytenga_Dec_2017_row_tmp$cases_linear_trtseeking_adj <- linear_trt_adj_pouytenga_Dec_2017
pouytenga_Dec_2017_row_tmp$cases_step1_trtseeking_adj <- step1_trt_adj_pouytenga_Dec_2017
pouytenga_Dec_2017_row_tmp$cases_step2_trtseeking_adj <- step2_trt_adj_pouytenga_Dec_2017
pouytenga_Dec_2017_row_tmp$cases_rep_weighted_adj <- weight_rep_adj_pouytenga_Dec_2017
pouytenga_Dec_2017_row_tmp$tests_rep_weighted_adj <- mal_tests_pouytenga_Dec_2017
pouytenga_Dec_2017_row_tmp$TPR <- TPR_pouytenga_Dec_2017
pouytenga_Dec_2017_row_tmp$all_cases_rep_weighted_adj <- all_cases_weight_rep_adj_pouytenga_Dec_2017
pouytenga_Dec_2017_row_tmp$all_nonMal_cases_rep_weighted_adj <- all_nonMal_weight_rep_adj_pouytenga_Dec_2017
pouytenga_Dec_2017_row_tmp$mal_ratio_weighted <- mal_ratio_weighted_pouytenga_Dec_2017


cases <- rbind(cases[1:pouytenga_ind_Nov_2017,],
               pouytenga_Dec_2017_row_tmp,
               cases[pouytenga_ind_Jan_2018:nrow(cases),])





# Pre-allocating data.frame for STL results

STL_result_DF_norm <- data.frame()


# Decompose each district

for (DS in sort(unique(cases$District)))
{
    # Selecting current district + ordering by date
    
    cases_dist <- cases[which(cases$District == DS),]
    cases_dist <- cases_dist[order(cases_dist$Date),]
    
    
    
    # Create crude incidence variable
    # Standardize it, make it a timeseries object, and decompose it
    mal_cases_norm <- getNormalized(cases_dist$mal_cases)
    mal_cases_norm_ts <- ts(mal_cases_norm, start = c(2015, 1), deltat = 1/12)
    cases_stl <- stlplus(mal_cases_norm_ts, s.window="periodic")
    
    
    linear_trt_adj_norm <- getNormalized(cases_dist$cases_linear_trtseeking_adj)
    linear_trt_adj_norm_ts <- ts(linear_trt_adj_norm, start = c(2015, 1), deltat = 1/12)
    linear_trt_adj_stl <- stlplus(linear_trt_adj_norm_ts, s.window="periodic")
    
    
    step1_trt_adj_norm <- getNormalized(cases_dist$cases_step1_trtseeking_adj)
    step1_trt_adj_norm_ts <- ts(step1_trt_adj_norm, start = c(2015, 1), deltat = 1/12)
    step1_trt_adj_stl <- stlplus(step1_trt_adj_norm_ts, s.window="periodic")
    
    
    step2_trt_adj_norm <- getNormalized(cases_dist$cases_step2_trtseeking_adj)
    step2_trt_adj_norm_ts <- ts(step2_trt_adj_norm, start = c(2015, 1), deltat = 1/12)
    step2_trt_adj_stl <- stlplus(step2_trt_adj_norm_ts, s.window="periodic")
    
    
    rep_weighted_adj_norm <- getNormalized(cases_dist$cases_rep_weighted_adj)
    rep_weighted_adj_norm_ts <- ts(rep_weighted_adj_norm, start = c(2015, 1), deltat = 1/12)
    rep_weighted_adj_stl <- stlplus(rep_weighted_adj_norm_ts, s.window="periodic")
    
    
    tests_rep_weighted_adj_norm <- getNormalized(cases_dist$tests_rep_weighted_adj)
    tests_rep_weighted_adj_norm_ts <- ts(tests_rep_weighted_adj_norm, start = c(2015, 1), deltat = 1/12)
    tests_rep_weighted_adj_stl <- stlplus(tests_rep_weighted_adj_norm_ts, s.window = "periodic")
    
    
    TPR_norm <- getNormalized(cases_dist$TPR)
    TPR_norm_ts <- ts(TPR_norm, start = c(2015, 1), deltat = 1/12)
    TPR_stl <- stlplus(TPR_norm_ts, s.window = "periodic")
    
    
    all_cases_rep_weighted_adj_norm <- getNormalized(cases_dist$all_cases_rep_weighted_adj)
    all_cases_rep_weighted_adj_norm_ts <- ts(all_cases_rep_weighted_adj_norm, start = c(2015, 1), deltat = 1/12)
    all_cases_rep_weighted_adj_stl <- stlplus(all_cases_rep_weighted_adj_norm_ts, s.window="periodic")
    
    
    all_nonMal_rep_weighted_adj_norm <- getNormalized(cases_dist$all_nonMal_cases_rep_weighted_adj)
    all_nonMal_rep_weighted_adj_norm_ts <- ts(all_nonMal_rep_weighted_adj_norm, start = c(2015, 1), deltat = 1/12)
    all_nonMal_rep_weighted_adj_stl <- stlplus(all_nonMal_rep_weighted_adj_norm_ts, s.window="periodic")
    
    
    mal_ratio_weighted_norm <- getNormalized(cases_dist$mal_ratio_weighted)
    mal_ratio_weighted_norm_ts <- ts(mal_ratio_weighted_norm, start = c(2015, 1), deltat = 1/12)
    ratio_weighted_stl <- stlplus(mal_ratio_weighted_norm_ts, s.window="periodic")
    
    
    
    # Create date object to create master data-frame for decomposition outputs
    
    dates <- seq(as.Date("2015-01-01"), as.Date("2018-12-01"), by="months")
    
    cases_stl_ts <- as.data.frame(cases_stl$data[,1:4]) 
    cases_stl_ts$type <- rep("cases", nrow(cases_stl_ts))
    cases_stl_ts$dates <- dates
    cases_stl_ts$MK_p <- smk.test(mal_cases_norm_ts)$p.value
    cases_stl_ts$sens_slope <- sea.sens.slope(mal_cases_norm_ts)
    
    
    linear_trt_adj_stl_ts <- as.data.frame(linear_trt_adj_stl$data[,1:4])
    linear_trt_adj_stl_ts$type <- rep("linear_trt_adj", nrow(linear_trt_adj_stl_ts))
    linear_trt_adj_stl_ts$dates <- dates
    linear_trt_adj_stl_ts$MK_p <- smk.test(linear_trt_adj_norm_ts)$p.value
    linear_trt_adj_stl_ts$sens_slope <- sea.sens.slope(linear_trt_adj_norm_ts)
    
    
    step1_trt_adj_stl_ts <- as.data.frame(step1_trt_adj_stl$data[,1:4])
    step1_trt_adj_stl_ts$type <- rep("step1_trt_adj", nrow(step1_trt_adj_stl_ts))
    step1_trt_adj_stl_ts$dates <- dates
    step1_trt_adj_stl_ts$MK_p <- smk.test(step1_trt_adj_norm_ts)$p.value
    step1_trt_adj_stl_ts$sens_slope <- sea.sens.slope(step1_trt_adj_norm_ts)
    
    
    step2_trt_adj_stl_ts <- as.data.frame(step2_trt_adj_stl$data[,1:4])
    step2_trt_adj_stl_ts$type <- rep("step2_trt_adj", nrow(step2_trt_adj_stl_ts))
    step2_trt_adj_stl_ts$dates <- dates
    step2_trt_adj_stl_ts$MK_p <- smk.test(step2_trt_adj_norm_ts)$p.value
    step2_trt_adj_stl_ts$sens_slope <- sea.sens.slope(step2_trt_adj_norm_ts)
    
    
    rep_weighted_adj_stl_ts <- as.data.frame(rep_weighted_adj_stl$data[,1:4]) 
    rep_weighted_adj_stl_ts$type <- rep("rep_weighted_adj", nrow(rep_weighted_adj_stl_ts))
    rep_weighted_adj_stl_ts$dates <- dates
    rep_weighted_adj_stl_ts$MK_p <- smk.test(rep_weighted_adj_norm_ts)$p.value
    rep_weighted_adj_stl_ts$sens_slope <- sea.sens.slope(rep_weighted_adj_norm_ts)
    
    
    tests_rep_weighted_adj_stl_ts <- as.data.frame(tests_rep_weighted_adj_stl$data[,1:4])
    tests_rep_weighted_adj_stl_ts$type <- rep("tests_rep_weighted_adj", nrow(tests_rep_weighted_adj_stl_ts))
    tests_rep_weighted_adj_stl_ts$dates <- dates
    tests_rep_weighted_adj_stl_ts$MK_p <- smk.test(tests_rep_weighted_adj_norm_ts)$p.value
    tests_rep_weighted_adj_stl_ts$sens_slope <- sea.sens.slope(tests_rep_weighted_adj_norm_ts)
    
    
    TPR_stl_ts <- as.data.frame(TPR_stl$data[,1:4])
    TPR_stl_ts$type <- rep("TPR", nrow(TPR_stl_ts))
    TPR_stl_ts$dates <- dates
    TPR_stl_ts$MK_p <- smk.test(TPR_norm_ts)$p.value
    TPR_stl_ts$sens_slope <- sea.sens.slope(TPR_norm_ts)
    
    
    all_cases_rep_weighted_adj_stl_ts <- as.data.frame(all_cases_rep_weighted_adj_stl$data[,1:4]) 
    all_cases_rep_weighted_adj_stl_ts$type <- rep("allout_rep_weighted_adj", nrow(all_cases_rep_weighted_adj_stl_ts))
    all_cases_rep_weighted_adj_stl_ts$dates <- dates
    all_cases_rep_weighted_adj_stl_ts$MK_p <- smk.test(all_cases_rep_weighted_adj_norm_ts)$p.value
    all_cases_rep_weighted_adj_stl_ts$sens_slope <- sea.sens.slope(all_cases_rep_weighted_adj_norm_ts)
    
    
    all_nonMal_rep_weighted_adj_stl_ts <- as.data.frame(all_nonMal_rep_weighted_adj_stl$data[,1:4]) 
    all_nonMal_rep_weighted_adj_stl_ts$type <- rep("all_nonMal_rep_weighted_adj", nrow(all_nonMal_rep_weighted_adj_stl_ts))
    all_nonMal_rep_weighted_adj_stl_ts$dates <- dates
    all_nonMal_rep_weighted_adj_stl_ts$MK_p <- smk.test(all_nonMal_rep_weighted_adj_norm_ts)$p.value
    all_nonMal_rep_weighted_adj_stl_ts$sens_slope <- sea.sens.slope(all_nonMal_rep_weighted_adj_norm_ts)
    
    
    ratio_weighted_stl_ts <- as.data.frame(ratio_weighted_stl$data[,1:4]) 
    ratio_weighted_stl_ts$type <- rep("ratio_weighted", nrow(ratio_weighted_stl_ts))
    ratio_weighted_stl_ts$dates <- dates
    ratio_weighted_stl_ts$MK_p <- smk.test(mal_ratio_weighted_norm_ts)$p.value
    ratio_weighted_stl_ts$sens_slope <- sea.sens.slope(mal_ratio_weighted_norm_ts)
    
    
    
    
    # Create output data-frame from stl outputs of both malaria variables
    
    STL_result_DF_DS_norm <- rbind(cases_stl_ts,
                                   linear_trt_adj_stl_ts,
                                   step1_trt_adj_stl_ts,
                                   step2_trt_adj_stl_ts,
                                   rep_weighted_adj_stl_ts,
                                   tests_rep_weighted_adj_stl_ts,
                                   TPR_stl_ts,
                                   all_cases_rep_weighted_adj_stl_ts,
                                   all_nonMal_rep_weighted_adj_stl_ts,
                                   ratio_weighted_stl_ts)
    STL_result_DF_DS_norm$District <- DS
    
    
    
    # Appending to District data-frame to master data-frame and continuing
    
    STL_result_DF_norm <- rbind(STL_result_DF_norm, STL_result_DF_DS_norm)
}

# Adding SMC rollout group info for each district
STL_result_DF_norm <- left_join(STL_result_DF_norm, unique(cases[,c("District", "Region")]),
                                by = "District")

# Ordering by rollout group and then by district
STL_result_DF_norm <- STL_result_DF_norm[order(STL_result_DF_norm$Region, STL_result_DF_norm$District),]


# Setting line-types and district factors for plotting
STL_result_DF_norm$District <- factor(STL_result_DF_norm$District, levels = sort(unique(cases$District)))






################################################################################


STL_result_DF_slopes <- unique(STL_result_DF_norm[,c(5,7:10)])




burkina_shape <- readOGR("~/Box/NU-malaria-team/data/burkina_shapefiles/Burkina Faso Health Districts SHP (130715)/BFA.shp")
burkina_shape_DF <- fortify(burkina_shape, region = "District")

burkina_shape_R <- readOGR("~/Box/NU-malaria-team/data/burkina_shapefiles/BFA_adm_shp/BFA_adm1.shp")
burkina_shape_Region <- fortify(burkina_shape_R, region = "NAME_1")



district_list <- cbind(sort(levels(STL_result_DF_slopes$District)), sort(unique(burkina_shape_DF$id)))
setDistrict <- function(Row)
{
    district <- district_list[which(district_list[,1] == Row[4]), 2]
    
    return(district)
}
STL_result_DF_slopes$id <- apply(STL_result_DF_slopes, 1, setDistrict)


STL_result_DF_slopes$plotting_sens_slope <- STL_result_DF_slopes$sens_slope
STL_result_DF_slopes[which(STL_result_DF_slopes$MK_p > .05), "plotting_sens_slope"] <- NA


burkina_shape_DF_sens <- inner_join(burkina_shape_DF, STL_result_DF_slopes, by = "id")



ggplot(data = burkina_shape_DF_sens, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = plotting_sens_slope), color = "white") +
    geom_polygon(data = burkina_shape_Region, inherit.aes = F,
                 aes(x = long, y = lat, group = group),
                 color = "black", fill = NA) + coord_equal() + 
    scale_fill_gradient2("Sen's slope coefficient", low = "#4575B4", mid = "#FFFFBF", high = "#D73027",
                         midpoint = 0,
                         limits = range(burkina_shape_DF_sens$plotting_sens_slope, na.rm = T)) +
    theme_void() + theme(plot.title = element_text(hjust = 0.5)) + facet_wrap(~type, ncol = 3)





STL_result_DF_slopes_new <- dcast(STL_result_DF_slopes, District ~ type, value.var = "plotting_sens_slope", sum)

View(STL_result_DF_slopes_new)


