
rm(list = ls(all = TRUE))


x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "stringr", "sp", "rgdal", "raster",
       "foreign", "RColorBrewer","sf", "tmap", "knitr", "plotrix", "arules", "fuzzyjoin",
       "splitstackshape", "ggpubr", "gridExtra", "pbapply", "plyr")

lapply(x, require, character.only = TRUE)


require("maditr")
require("INLA")
require("spdep")

require("spatstat")
require("maptools")



require("deldir")
require("SDraw")

require("rgeos")


#####################################################################################################

## Load covariate layers

load("~/OneDrive/Desktop/SAE project/scaled_raster_layers/cov_IM_list.RData")


#####################################################################################################


med_2014 <- read.csv("~/OneDrive/Desktop/cluster_medfever_14_with_dates.csv", stringsAsFactors = FALSE)[,-1]
med_2014$year <- 2014
med_2014$row_num <- 1:nrow(med_2014)



med_2017 <- read.csv("~/OneDrive/Desktop/cluster_medfever_17_with_dates.csv", stringsAsFactors = FALSE)[,-1]
med_2017$year <- 2017
med_2017$row_num <- 1:nrow(med_2017)


medfev_clust <- rbind(med_2014, med_2017)[,-21]
names(medfev_clust)[22:23] <- c("med_fever", "se")

medfev_clust$N_med_fever <- round(medfev_clust$num_kids * medfev_clust$med_fever, 0)


#####################################################################################################

## Load routine case data for regional pops

pop_data <- read.csv("~/Box/NU-malaria-team/projects/smc_impact/data/outputs/cases_seasonal_smc.csv",
                     header = TRUE, strip.white = TRUE, stringsAsFactors = FALSE)

pop_data <- unique(pop_data[which(pop_data$Year %in% c(2014, 2017)),
                            c("Admin1", "Admin2", "Year", "Population")])


pop_data_DS <- ddply(pop_data, c(.(Admin2), .(Year)), summarise,
                     pop = sum(Population))


pop_data <- ddply(pop_data, c(.(Admin1), .(Year)), summarise,
                  pop = sum(Population))


#####################################################################################################



pts_14 <- readOGR("~/Box/NU-malaria-team/data/burkina_dhs/data analysis/data/BF_2014_MIS_06192019/BFGE71FL", layer = "BFGE71FL")
pts <- readOGR("~/Box/NU-malaria-team/data/burkina_dhs/data analysis/data/BF_2017-18_MIS_07252019_1531_86355/BFGE7AFL", layer = "BFGE7AFL")

#DS file
DS_shape <- readOGR("~/Box/NU-malaria-team/data/burkina_shapefiles/Health Districts Burkina",
                    layer = "70ds_", use_iconv=TRUE, encoding= "UTF-8")
DS_shape <- spTransform(DS_shape, "+proj=longlat +datum=WGS84 +no_defs")


burkina_shape <- readOGR("~/Box/NU-malaria-team/data/burkina_shapefiles/BFA_adm_shp/BFA_adm0.shp")
# burkina_shape_f <- fortify(burkina_shape_R)


burkina_shape_Region <- readOGR("~/Box/NU-malaria-team/data/burkina_shapefiles/BFA_adm_shp/BFA_adm1.shp")


burkina_shape_pts <- st_as_sf(burkina_shape)
burkina_shape_sp <- spTransform(burkina_shape, crs(pts))


#converting the sp objects to sf object for easy plotting with tmap later 
pt_sf_14 <- st_as_sf(pts_14)
pt_sf_17 <- st_as_sf(pts)
DSshape_sf <- st_as_sf(DS_shape)

Rshape_sf <- st_as_sf(burkina_shape_Region)



DS_shape_W <- spTransform(DS_shape, crs(pts))


# plot(DS_shape_W, main = 'Administrative boundary: Health Districts with DHS 2017 clusters')
# plot(pts, add=T, col=4) #this mis 17 




key_14 <- over(SpatialPoints(coordinates(pts_14), proj4string = pts_14@proj4string), DS_shape_W)

#add in the cluster variable
key_14$DHSCLUST <- pts_14@data[,"DHSCLUST"]


#if pts@data is 245 by 20 and district is 70 by 10, then the corresponding data frame will be 245 by 10

key <- over(SpatialPoints(coordinates(pts), proj4string = pts@proj4string), DS_shape_W)

#add in the cluster variable
key$DHSCLUST <- pts@data[,"DHSCLUST"]



#####################################################################################################

Y <- 2014

## Look at just 2014 data and remove 4 clusters with GPS on origin (clean later?)
medfev_clust_Y <- medfev_clust[medfev_clust$DHSYEAR == Y,]

medfev_clust_Y <- medfev_clust_Y[which(medfev_clust_Y$LONGNUM != 0 &
                                           medfev_clust_Y$LATNUM != 0),]


pop_data_Y <- pop_data[which(pop_data$Year == Y), -2]

pop_data_DS_Y <- pop_data_DS[which(pop_data_DS$Year == Y), -2]


##################################################################################

## Model diags functions

slcpo <- function(m, na.rm = TRUE)
{
    - sum(log(m$cpo$cpo), na.rm = na.rm)
}


## Getting MAE

ground_truth_R <- data.frame(Region = str_replace(burkina_shape_Region$NAME_1, "-", " "),
                             tru_val = c(0.526528792, 0.658314442, 0.711536042, 0.607393806,
                                         0.709816261, 0.685751818, 0.636558476, 0.542230157,
                                         0.470578059, 0.659278671, 0.671089911, 0.367941859,
                                         0.455681602))
ground_truth_R[9, "Region"] <- "Hauts Bassins"


sp.medfever <- medfev_clust_Y[, c("med_fever", "LATNUM", "LONGNUM")]
names(sp.medfever) <- c("med_fever", "lat", "long")
coordinates(sp.medfever) <- ~long+lat
crs(sp.medfever) <- crs(burkina_shape_Region)


get_MAE <- function(X)
{
    spdf$fit <- X
    
    GT_R <- data.frame(ground_truth_R,
                       "fitted" = over(burkina_shape_Region,
                                       spdf, fn = "mean")[,4])
    GT_R$Region <- as.character(GT_R$Region)
    GT_R[9, "Region"] <- "Hauts Bassins"
    
    GT_R$err <- abs(GT_R$tru_val - GT_R$fitted)
    
    GT_R <- left_join(GT_R, pop_data_Y, by = c("Region" = "Admin1"))
    
    
    
    GT_C <- data.frame("medfever" = sp.medfever$med_fever,
                       "N_clust" = medfev_clust_Y$U5.in.DHS.clusters,
                       over(sp.medfever, spdf, fn = "mean")[,c(1,2,4)])
    
    ## Manual, for some reason, one cluster point cant be matched automatically to the pixels map
    # X$SPDE[c(33, 34, 53)]
    
    names(GT_C)[3:5] <- c("long", "lat", "fitted")
    
    GT_C$err <- abs(GT_C$medfever - GT_C$fitted)
    
    
    
    R_err <- mean(GT_R$err)
    R_err_weighted <- sum(GT_R$pop * GT_R$err, na.rm = T) / sum(GT_R$pop)
    
    
    C_err <- mean(GT_C$err, na.rm = T)
    C_err_weighted <- sum(GT_C$N_clust * GT_C$err, na.rm = T) / sum(GT_C$N_clust[!is.na(GT_C$N_clust)])
    
    
    # MAE
    return(c(R_err_weighted,
             C_err_weighted))
}


med_2014 <- read.csv("~/OneDrive/Desktop/medfever_DS_2014.csv", stringsAsFactors = FALSE)[,-1]
med_2014 <- med_2014[c(1:29, 31:35, 69, 36:43, 70, 44:64, 30, 65:68),]

pop_2014 <- raster("~/OneDrive/Desktop/SAE project/pop/bfa_ppp_2014_1km_Aggregated.tif")


pop_weighted_DS <- function(X)
{
    
    spdf$fit <- X
    
    r_spdf <- raster(spdf["fit"])
    r_spdf <- setExtent(r_spdf, extent(-5.517917, 2.407083, 9.407917, 15.09125))

    
    r_spdf_higher_res <- projectRaster(r_spdf,
                                       res = c(0.008333333, 0.008333333),
                                       crs = "+proj=longlat +datum=WGS84 +no_defs")
    r_spdf_higher_res <- flip(flip(t(r_spdf_higher_res)), direction="x")
    
    
    
    
    r_spdf_higher_res_int <- raster::intersect(r_spdf_higher_res, pop_2014)
    r_spdf_higher_res_int_clip <- mask(x = r_spdf_higher_res_int, mask = pop_2014)
    
    DS_extracted_fits <- raster::extract(r_spdf_higher_res_int_clip, DS_shape_W)
    DS_extracted_pop <- raster::extract(pop_2014, DS_shape_W)
    
    DS_pop_weighted_fits <- data.frame(Admin2 = DS_shape_W$NOMDEP,
                                       pop_weighted_fits = rep(0, 70))
    for (i in 1:70)
    {
        if (length(DS_extracted_pop[[i]]) > length(DS_extracted_fits[[i]]))
        {
            DS_pop_weighted_fits[i,2] <- sum(DS_extracted_pop[[i]][1:length(DS_extracted_fits[[i]])] * DS_extracted_fits[[i]], na.rm = T)
        } else if (length(DS_extracted_pop[[i]]) < length(DS_extracted_fits[[i]]))
        {
            DS_pop_weighted_fits[i,2] <- sum(DS_extracted_pop[[i]] * DS_extracted_fits[[i]][1:length(DS_extracted_pop[[i]])], na.rm = T)
        } else
        {
            DS_pop_weighted_fits[i,2] <- sum(DS_extracted_pop[[i]] * DS_extracted_fits[[i]], na.rm = T)
        }
    }
    
    DS_pop_weighted_fits <- cbind(DS_pop_weighted_fits, sapply(DS_extracted_pop, function(x) { sum(x, na.rm = T) } ))
    names(DS_pop_weighted_fits)[3] <- "pop"
    
    DS_pop_weighted_fits <- DS_pop_weighted_fits[order(DS_pop_weighted_fits$Admin2),]
    DS_pop_weighted_fits <- DS_pop_weighted_fits[c(1:29, 31:66, 30, 67:70),]
    
    
    
    DS_pop_weighted_fits$rate <- DS_pop_weighted_fits$pop_weighted_fits/ DS_pop_weighted_fits$pop
    
}



ggplot() + geom_raster(data = rasterToPoints(pop_2014), aes(x = x, y = y, fill = bfa_ppp_2014_1km_Aggregated))


pop_2014_spdf <- rasterToPoints(pop_2014) %>% data.frame

ggplot() + geom_raster(data = pop_2014_spdf,
                       aes(x = x, y = y, fill=bfa_ppp_2014_1km_Aggregated)) +
    geom_polygon(data = fortify(DS_shape_W, region = "NOMDEP"),
                 aes(x = long, y = lat, group = "id"), fill = NA)
    

ggplot() + geom_raster(data = pop_2014_spdf, inherit.aes = F,
                       aes(x = x, y = y, fill=bfa_ppp_2014_1km_Aggregated)) +
    scale_fill_viridis() +
    geom_sf(data = DSshape_sf, fill = NA, color = "black")


ggplot() + geom_sf(data = DSshape_sf)


#####################################################################################################

## Loading INLA stack data

load("~/Desktop/SAE_with_more_lags/medfever_stack_2014_norm_w_dates.RData")



medfever.spde <- save_list[[1]]
join.stack <- save_list[[2]]
covs <- save_list[[3]]
cov_df <- save_list[[4]]
covs.pred <- save_list[[5]]
clust_medfev_binom_key <- save_list[[6]]
spdf <- save_list[[7]]
scaling_factors <- save_list[[8]]


idx <- inla.stack.index(join.stack, 'pred')$data


#####################################################################################################

## Testing precip

pp.res_precip <- inla(y ~ 0 + b0 + precip +
                               f(spatial.field, model = medfever.spde),
                           family = "binomial", Ntrials = total,
                           data = inla.stack.data(join.stack),
                           control.predictor = list(A = inla.stack.A(join.stack),
                                                    compute = TRUE,
                                                    link = 1),
                           control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE),
                           control.inla = list(int.strategy = "eb"))


pp.res_precip_lag1 <- inla(y ~ 0 + b0 + precip_lag1 +
                               f(spatial.field, model = medfever.spde),
                           family = "binomial", Ntrials = total,
                           data = inla.stack.data(join.stack),
                           control.predictor = list(A = inla.stack.A(join.stack),
                                                    compute = TRUE,
                                                    link = 1),
                           control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE),
                           control.inla = list(int.strategy = "eb"))


pp.res_precip_lag2 <- inla(y ~ 0 + b0 + precip_lag2 +
                               f(spatial.field, model = medfever.spde),
                           family = "binomial", Ntrials = total,
                           data = inla.stack.data(join.stack),
                           control.predictor = list(A = inla.stack.A(join.stack),
                                                    compute = TRUE,
                                                    link = 1),
                           control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE),
                           control.inla = list(int.strategy = "eb"))


pp.res_precip_lag3 <- inla(y ~ 0 + b0 + precip_lag3 +
                               f(spatial.field, model = medfever.spde),
                           family = "binomial", Ntrials = total,
                           data = inla.stack.data(join.stack),
                           control.predictor = list(A = inla.stack.A(join.stack),
                                                    compute = TRUE,
                                                    link = 1),
                           control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE),
                           control.inla = list(int.strategy = "eb"))


## Adding ones that are significant
#  None signifcant here lol
pp.res_precips_all <- inla(y ~ 0 + b0 + precip_lag1 + precip_lag2 +
                               f(spatial.field, model = medfever.spde),
                           family = "binomial", Ntrials = total,
                           data = inla.stack.data(join.stack),
                           control.predictor = list(A = inla.stack.A(join.stack),
                                                    compute = TRUE,
                                                    link = 1),
                           control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE),
                           control.inla = list(int.strategy = "eb"))




pp.res_precip_lag1_2 <- inla(y ~ 0 + b0 + precip_lag1_grouped_q2 +
                                 f(spatial.field, model = medfever.spde),
                             family = "binomial", Ntrials = total,
                             data = inla.stack.data(join.stack),
                             control.predictor = list(A = inla.stack.A(join.stack),
                                                      compute = TRUE,
                                                      link = 1),
                             control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE),
                             control.inla = list(int.strategy = "eb"))


#####################################################################################################

## Testing air.temp

# pp.res_a.t <- inla(y ~ 0 + b0 + air.temp +
#                        f(spatial.field, model = medfever.spde),
#                    family = "binomial", Ntrials = total,
#                    data = inla.stack.data(join.stack),
#                    control.predictor = list(A = inla.stack.A(join.stack),
#                                             compute = TRUE,
#                                             link = 1),
#                    control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE),
#                    control.inla = list(int.strategy = "eb"))


pp.res_a.t_lag1 <- inla(y ~ 0 + b0 + air.temp_lag1 +
                            f(spatial.field, model = medfever.spde),
                        family = "binomial", Ntrials = total,
                        data = inla.stack.data(join.stack),
                        control.predictor = list(A = inla.stack.A(join.stack),
                                                 compute = TRUE,
                                                 link = 1),
                        control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE),
                        control.inla = list(int.strategy = "eb"))


pp.res_a.t_lag2 <- inla(y ~ 0 + b0 + air.temp_lag2 +
                            f(spatial.field, model = medfever.spde),
                        family = "binomial", Ntrials = total,
                        data = inla.stack.data(join.stack),
                        control.predictor = list(A = inla.stack.A(join.stack),
                                                 compute = TRUE,
                                                 link = 1),
                        control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE),
                        control.inla = list(int.strategy = "eb"))


pp.res_a.t_lag3 <- inla(y ~ 0 + b0 + air.temp_lag3 +
                            f(spatial.field, model = medfever.spde),
                        family = "binomial", Ntrials = total,
                        data = inla.stack.data(join.stack),
                        control.predictor = list(A = inla.stack.A(join.stack),
                                                 compute = TRUE,
                                                 link = 1),
                        control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE),
                        control.inla = list(int.strategy = "eb"))


## Only lag1 signif here
pp.res_a.t_all <- inla(y ~ 0 + b0 + air.temp_lag1 + air.temp_lag2 +
                            f(spatial.field, model = medfever.spde),
                        family = "binomial", Ntrials = total,
                        data = inla.stack.data(join.stack),
                        control.predictor = list(A = inla.stack.A(join.stack),
                                                 compute = TRUE,
                                                 link = 1),
                        control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE),
                        control.inla = list(int.strategy = "eb"))


# pp.res_a.t_lag1_2 <- inla(y ~ 0 + b0 + air.temp_lag1_grouped_q4 +
#                               f(spatial.field, model = medfever.spde),
#                           family = "binomial", Ntrials = total,
#                           data = inla.stack.data(join.stack),
#                           control.predictor = list(A = inla.stack.A(join.stack),
#                                                    compute = TRUE,
#                                                    link = 1),
#                           control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE),
#                           control.inla = list(int.strategy = "eb"))


#####################################################################################################

tmp_slcpo <- c("precip_lag1" = slcpo(pp.res_precip_lag1),
               "precip_lag2" = slcpo(pp.res_precip_lag2),
               "air.temp_lag1" = slcpo(pp.res_a.t_lag1),
               "air.temp_lag2" = slcpo(pp.res_a.t_lag2))


tmp_err <- t(data.frame(precip_lag1 = get_MAE(pp.res_precip_lag1$summary.fitted.values[idx, "mean"]),
                        precip_lag2 = get_MAE(pp.res_precip_lag2$summary.fitted.values[idx, "mean"]),
                        air.temp_lag1 = get_MAE(pp.res_a.t_lag1$summary.fitted.values[idx, "mean"]),
                        air.temp_lag2 = get_MAE(pp.res_a.t_lag2$summary.fitted.values[idx, "mean"]),
                        row.names = c("Region err", "Cluster err")))

cbind("slcpo" = tmp_slcpo, tmp_err)


#####################################################################################################


pp.res_tt <- inla(y ~ 0 + b0 + public_tt +
                      f(spatial.field, model = medfever.spde), 
                  family = "binomial", Ntrials = total,
                  data = inla.stack.data(join.stack),
                  control.predictor = list(A = inla.stack.A(join.stack),
                                           compute = TRUE,
                                           link = 1),
                  control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE),
                  control.inla = list(int.strategy = "eb"))



## For TT, cut may make more sense as a grouping criterion.
#   Also performs much better than quant method
#   Cut method isnt excluding lower and upper 10th %
pp.res_tt_grouped <- inla(y ~ 0 + b0 + TT_grouped_c7 +
                              f(spatial.field, model = medfever.spde),
                          family = "binomial", Ntrials = total,
                          data = inla.stack.data(join.stack),
                          control.predictor = list(A = inla.stack.A(join.stack),
                                                   compute = TRUE,
                                                   link = 1),
                          control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE),
                          control.inla = list(int.strategy = "eb"))



#####################################################################################################


pp.res0 <- inla(y ~ 0 + b0 + f(spatial.field, model = medfever.spde),
                family = "binomial", Ntrials = total,
                data = inla.stack.data(join.stack),
                # control.link("logit"), same with/without. Pretty sure its default
                control.predictor = list(A = inla.stack.A(join.stack),
                                         compute = TRUE,
                                         link = 1), # Option link = 1 in the control.predictor is used to set the function link to be considered in the computation of the fitted values that form the vector of available links
                control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE),
                control.inla = list(int.strategy = "eb")) # using empirical Bayes numerical integration


pp.res_1 <- inla(y ~ 0 + b0 + public_tt +
                     precip_lag1 + air.temp_lag1 +
                     f(spatial.field, model = medfever.spde), 
                 family = "binomial", Ntrials = total,
                 data = inla.stack.data(join.stack),
                 control.predictor = list(A = inla.stack.A(join.stack),
                                          compute = TRUE,
                                          link = 1),
                 control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE),
                 control.inla = list(int.strategy = "eb"))


pp.res_2 <- inla(y ~ 0 + b0 + TT_grouped_c7 +
                     precip_lag1_grouped_q2 + air.temp_lag1_grouped_q4 +
                     f(spatial.field, model = medfever.spde), 
                 family = "binomial", Ntrials = total,
                 data = inla.stack.data(join.stack),
                 control.predictor = list(A = inla.stack.A(join.stack),
                                          compute = TRUE,
                                          link = 1),
                 control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE),
                 control.inla = list(int.strategy = "eb"))


pp.res_3 <- inla(y ~ 0 + b0 + public_tt +
                     air.temp_lag1 + 
                     f(spatial.field, model = medfever.spde),
                 family = "binomial", Ntrials = total,
                 data = inla.stack.data(join.stack),
                 control.predictor = list(A = inla.stack.A(join.stack),
                                          compute = TRUE,
                                          link = 1),
                 control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE),
                 control.inla = list(int.strategy = "eb"))


pp.res_4 <- inla(y ~ 0 + b0 + TT_grouped_c7 +
                     air.temp_lag1_grouped_q4 + 
                     f(spatial.field, model = medfever.spde),
                 family = "binomial", Ntrials = total,
                 data = inla.stack.data(join.stack),
                 control.predictor = list(A = inla.stack.A(join.stack),
                                          compute = TRUE,
                                          link = 1),
                 control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE),
                 control.inla = list(int.strategy = "eb"))


pp.res_5 <- inla(y ~ 0 + b0 + public_tt +
                     air.temp_lag1_grouped_q4 + 
                     f(spatial.field, model = medfever.spde),
                 family = "binomial", Ntrials = total,
                 data = inla.stack.data(join.stack),
                 control.predictor = list(A = inla.stack.A(join.stack),
                                          compute = TRUE,
                                          link = 1),
                 control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE),
                 control.inla = list(int.strategy = "eb"))



##################################################################################


pp.res_4_with_D_RE <- inla(y ~ 0 + b0 + TT_grouped_c7 +
                                  air.temp_lag1_grouped_q4 +
                                  f(District, model = "iid") +
                                  f(spatial.field, model = medfever.spde),
                              family = "binomial", Ntrials = total,
                              data = inla.stack.data(join.stack),
                              control.predictor = list(A = inla.stack.A(join.stack),
                                                       compute = TRUE,
                                                       link = 1),
                              control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE),
                              control.inla = list(int.strategy = "eb"))


pp.res_5_with_D_RE <- inla(y ~ 0 + b0 + public_tt +
                               air.temp_lag1_grouped_q4 +
                               f(District, model = "iid") +
                               f(spatial.field, model = medfever.spde),
                           family = "binomial", Ntrials = total,
                           data = inla.stack.data(join.stack),
                           control.predictor = list(A = inla.stack.A(join.stack),
                                                    compute = TRUE,
                                                    link = 1),
                           control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE),
                           control.inla = list(int.strategy = "eb"))



pp.res_5_with_D_REs_2 <- inla(y ~ 0 + b0 + public_tt +
                                air.temp_lag1_grouped_q4 +
                                f(District, model = "iid") +
                                f(air.temp_lag1_grouped_q4, model = "iid") +
                                f(spatial.field, model = medfever.spde),
                            family = "binomial", Ntrials = total,
                            data = inla.stack.data(join.stack),
                            verbose = TRUE,
                            control.predictor = list(A = inla.stack.A(join.stack),
                                                     compute = TRUE,
                                                     link = 1),
                            control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE),
                            control.inla = list(int.strategy = "eb"))


pp.res_3_with_R_REs <- inla(y ~ 0 + b0 + public_tt +
                                air.temp_lag1 + 
                                f(Region, model = "iid") +
                                f(spatial.field, model = medfever.spde),
                            family = "binomial", Ntrials = total,
                            data = inla.stack.data(join.stack),
                            control.predictor = list(A = inla.stack.A(join.stack),
                                                     compute = TRUE,
                                                     link = 1),
                            control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE),
                            control.inla = list(int.strategy = "eb"))





# pp.res_3_with_D_REs_2 <- inla(y ~ 0 + b0 + public_tt +
#                                 air.temp_lag1 + District +
#                                 f(District, model = "iid") +
#                                 f(spatial.field, model = medfever.spde),
#                             family = "binomial", Ntrials = total,
#                             data = inla.stack.data(join.stack),
#                             control.predictor = list(A = inla.stack.A(join.stack),
#                                                      compute = TRUE,
#                                                      link = 1),
#                             control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE),
#                             control.inla = list(int.strategy = "eb"))



##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################


pp.res <- pp.res_5_with_D_RE


idx <- inla.stack.index(join.stack, 'pred')$data


spdf$SPDE <- pp.res$summary.fitted.values[idx, "mean"]

spdf$SPDE.sd <- pp.res$summary.fitted.values[idx, "sd"]




plot(spdf["SPDE"])
lines(burkina_shape_Region, col = 1, lwd = 2)


plot(spdf["SPDE.sd"])
lines(burkina_shape_Region, col = 1, lwd = 2)







round(pp.res_tt_grouped$summary.fixed[,c(1,3,5)], 3) %>% apply(1, function(X) { paste(X[1], " [", X[2], ", ", X[3], "]", sep = "") }) %>% as.data.frame()






##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################


load(file = "~/OneDrive/Desktop/MIS_cluster_pts_2014.RData")

names(pt_m14)[c(23,24)] <- c("u5_size", "med_fever") 

spdf_sf <- st_as_sf(spdf)




ggplot() + geom_sf(data = spdf_sf,
                   aes(color = SPDE), pch = 15, size = .05) +
    scale_color_stepsn(limits = c(0,1),
                       breaks = c(0,.2,.3,.4,.5,.6,.7,.8,.9,1),
                       colors = brewer.pal(n = 10, name = "RdYlBu")) +
    geom_sf(data = Rshape_sf, fill = NA, color = "black")

    



DS_merge <- DSshape_sf %>% left_join(DS_pop_weighted_fits, by = c("NOMDEP" = "Admin2"))



ggplot() + geom_sf(data = DS_merge,
                   aes(fill = rate)) +
    scale_fill_stepsn(limits = c(0,1),
                      breaks = c(0,.2,.3,.4,.5,.6,.7,.8,.9,1),
                      colors = brewer.pal(n = 10, name = "RdYlBu")) +
    geom_sf(data = DSshape_sf, fill = NA, color = "black")
    # geom_sf(data = pt_m14, inherit.aes = F,
    #         aes(fill = med_fever, size = u5_size),
    #         color = "black", pch = 21)







ggplot() + geom_sf(data = DS_merge,
                   aes(fill = rate)) +
    scale_fill_stepsn(limits = c(0,1),
                      breaks = c(0,.2,.3,.4,.5,.6,.7,.8,.9,1),
                      colors = brewer.pal(n = 10, name = "RdYlBu")) +
    geom_sf(data = DSshape_sf, fill = NA, color = "grey") +
    geom_sf(data = Rshape_sf, fill = NA, color = "black")
    








Rshape_sf$NAME_1[c(3:6, 9, 11, 13)] <- c("Centre Est", "Centre Nord", "Centre Ouest",
                                         "Centre Sud", "Hauts Bassins", "Plateau Central", "Sud Ouest")
R_merge <- Rshape_sf %>% left_join(GT_R, by = c("NAME_1" = "Region"))

ggplot() + geom_sf(data = R_merge,
                   aes(fill = fitted)) +
    scale_fill_viridis(limits = c(0,1)) +
    geom_sf(data = Rshape_sf, fill = NA, color = "black")






##################################################################################


# saveRDS(spdf, file="~/Desktop/SAE_with_more_lags/2014_1x1km_spdf.rds")




X <- pp.res_5_with_D_RE$summary.fitted.values[idx, "mean"]
DS_INLA_fits <- DS_pop_weighted_fits[,c("Admin2", "rate")]
DS_INLA_fits <- as.data.frame(cbind(DS_INLA_fits[,1], rep(2014, nrow(DS_INLA_fits)),
                                    DS_INLA_fits[,2]))
names(DS_INLA_fits) <- c("NOMDEP", "year", "saep.est")


X <- pp.res_5_with_D_RE$summary.fitted.values[idx, "0.025quant"]
DS_INLA_fits <- cbind(DS_INLA_fits, DS_pop_weighted_fits[,"rate"])
names(DS_INLA_fits) <- c("NOMDEP", "year", "saep.est", "ci_l")

X <- pp.res_5_with_D_RE$summary.fitted.values[idx, "0.975quant"]
DS_INLA_fits <- cbind(DS_INLA_fits, DS_pop_weighted_fits[,"rate"])
names(DS_INLA_fits) <- c("NOMDEP", "year", "saep.est", "ci_l", "ci_u")



write.csv(DS_INLA_fits, "~/OneDrive/Desktop/medfever_DS_2014_INLA_fitted_1x1km.csv")



