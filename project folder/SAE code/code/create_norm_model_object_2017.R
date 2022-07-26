#############################################
##  Creating INLA SPDE stack model object  ##
#############################################
#
#
#   For 2017 right now
#   Including following covariate layers:
#       precip, air.temp, NDVI, public_tt, pop
#
#
#


rm(list = ls(all = TRUE))


x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "stringr", "sp", "rgdal", "raster",
       "foreign", "RColorBrewer","sf", "tmap", "knitr", "plotrix", "arules", "fuzzyjoin",
       "splitstackshape", "ggpubr", "nngeo", "gridExtra", "pbapply", "zoo")

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

## Normalizing function

get_norm <- function(X)
{
    X_mean <- mean(X, na.rm = T)
    X_sd <- sd(X, na.rm = T)
    
    X <- (X - X_mean) / X_sd
    
    ret_list <- list(X, c(X_mean, X_sd))
    return(ret_list)
}


scale_preds <- function(Y, X_mean, X_sd)
{
    Y <- (Y - X_mean) / X_sd
    
    return(Y)
}


group_preds <- function(X, unique_groups)
{
    Y <- rep(0, length(X))
    
    for ( i in 1:length(X) )
    {
        if ( is.na(X[i]) )
        {
            Y[i] <- NA
        } else {
            Y[i] <- unique_groups[which.min(abs(unique_groups - X[i]))]   
        }
    }
    
    return(Y)
}


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

##

Y = 2017

## Look at just 2014 data and remove 4 clusters with GPS on origin (clean later?)
medfev_clust_Y <- medfev_clust[medfev_clust$DHSYEAR == Y,]

medfev_clust_Y <- medfev_clust_Y[which(medfev_clust_Y$LONGNUM != 0 &
                                           medfev_clust_Y$LATNUM != 0),]




clust_medfev_key <- left_join(medfev_clust_Y, key)

clust_medfev_binom_key <- cbind(clust_medfev_key,
                                success = clust_medfev_key$N_med_fever,
                                N = clust_medfev_key$U5.in.DHS.clusters)





medfever.mesh <- inla.mesh.2d(boundary = inla.sp2segment(burkina_shape),
                              max.edge = c(.05, 1),
                              cutoff = .05)




# Create SPDE
medfever.spde <- inla.spde2.matern(mesh = medfever.mesh)




# Points
medfever.pts <- as.matrix(clust_medfev_binom_key[, c("LONGNUM", "LATNUM")])
medfever.mesh.pts <- as.matrix(medfever.mesh$loc[, 1:2])
allpts <- rbind(medfever.mesh.pts, medfever.pts)


# Number of vertices in the mesh
nv <- medfever.mesh$n

# Number of points in the data
n <- nrow(medfever.pts)




#####################################################################################################



# Prepare data

# y.pp = rep(0:1, c(nv, n))
y.pp = cbind(c(rep(NA, nv), clust_medfev_binom_key$success),
             c(rep(NA, nv), clust_medfev_binom_key$N))

# e.pp = c(w, rep(0, n))

A.pp <- inla.spde.make.A(medfever.mesh, medfever.pts)
# imat <- Diagonal(nv, rep(1, nv))

# A.pp <- rbind(imat, lmat)

medfever.spde.index <- inla.spde.make.index(name = "spatial.field",
                                            n.spde = medfever.spde$n.spde)



#####################################################################################################



# Covs for spatial model

cov_IM_list_Y <- cov_IM_list[c(names(cov_IM_list)[grepl(paste("precip_", Y, "-", sep=""),
                                                        names(cov_IM_list))],
                               names(cov_IM_list)[grepl(paste("air.temp_", Y, "-", sep=""),
                                                        names(cov_IM_list))],
                               names(cov_IM_list)[grepl("NDVI_", names(cov_IM_list))][-1],
                               "public_tt", paste("pop_", Y, sep = ""),
                               "build_up")]




## Comment out for normalized covs
cov_IM_list_Y[["public_tt"]]$v <- log(1 + cov_IM_list_Y[["public_tt"]]$v)

cov_IM_list_Y[["precip_2017-08"]]$v <- log(1 + cov_IM_list_Y[["precip_2017-08"]]$v)
cov_IM_list_Y[["precip_2017-09"]]$v <- log(1 + cov_IM_list_Y[["precip_2017-09"]]$v)
cov_IM_list_Y[["precip_2017-10"]]$v <- log(1 + cov_IM_list_Y[["precip_2017-10"]]$v)
cov_IM_list_Y[["precip_2017-11"]]$v <- log(1 + cov_IM_list_Y[["precip_2017-11"]]$v)
cov_IM_list_Y[["precip_2017-12"]]$v <- log(1 + cov_IM_list_Y[["precip_2017-12"]]$v)
cov_IM_list_Y[["precip_2017-13"]]$v <- log(1 + cov_IM_list_Y[["precip_2017-13"]]$v)
cov_IM_list_Y[["precip_2017-14"]]$v <- log(1 + cov_IM_list_Y[["precip_2017-14"]]$v)
cov_IM_list_Y[["precip_2017-15"]]$v <- log(1 + cov_IM_list_Y[["precip_2017-15"]]$v)


cov_IM_list_Y[["air.temp_2017-08"]]$v <- log(1 + cov_IM_list_Y[["air.temp_2017-08"]]$v)
cov_IM_list_Y[["air.temp_2017-09"]]$v <- log(1 + cov_IM_list_Y[["air.temp_2017-09"]]$v)
cov_IM_list_Y[["air.temp_2017-10"]]$v <- log(1 + cov_IM_list_Y[["air.temp_2017-10"]]$v)
cov_IM_list_Y[["air.temp_2017-11"]]$v <- log(1 + cov_IM_list_Y[["air.temp_2017-11"]]$v)
cov_IM_list_Y[["air.temp_2017-12"]]$v <- log(1 + cov_IM_list_Y[["air.temp_2017-12"]]$v)
cov_IM_list_Y[["air.temp_2017-13"]]$v <- log(1 + cov_IM_list_Y[["air.temp_2017-13"]]$v)
cov_IM_list_Y[["air.temp_2017-14"]]$v <- log(1 + cov_IM_list_Y[["air.temp_2017-14"]]$v)
cov_IM_list_Y[["air.temp_2017-15"]]$v <- log(1 + cov_IM_list_Y[["air.temp_2017-15"]]$v)


cov_IM_list_Y$log_pop <- cov_IM_list_Y$pop_2017
cov_IM_list_Y[["log_pop"]]$v <- log(1 + cov_IM_list_Y[["log_pop"]]$v)


cov_IM_list_Y$log_build_up <- cov_IM_list_Y$build_up
cov_IM_list_Y[["log_build_up"]]$v <- log(1 + cov_IM_list_Y[["log_build_up"]]$v)

names(cov_IM_list_Y)[17:21] <- c("NDVI_2017-08", "NDVI_2017-09", "NDVI_2017-10",
                                 "NDVI", "NDVI_2017-12")


################################################################################################


# Covariates
delta <- .05
medfever.ppp <- ppp(medfever.pts[,1], medfever.pts[,2],
                    owin(xrange = range(medfever.pts[,1]) + c(-delta, delta),
                         yrange = range(medfever.pts[,2]) + c(-delta, delta)))

covs <- lapply(cov_IM_list_Y, function(X){
    pixels <- nearest.pixel(medfever.ppp$x, medfever.ppp$y, X)
    sapply(1:npoints(medfever.ppp), function(i) {
        X[pixels$row[i], pixels$col[i]]
    })
})


covs$long <- medfever.ppp$x
covs$lat <- medfever.ppp$y


################################################################################################


covs_df <- as.data.frame(covs)
names(covs_df) <- names(covs)


# covs with no dates

covs_df_no_dates <- covs_df[,c(20,22:28)]
clust_medfev_binom_key_fitting <- cbind(clust_medfev_binom_key, covs_df_no_dates)


# covs with dates
covs_df_w_dates <- covs_df[,-c(17:28)]


clust_medfev_binom_key_fitting$precip <- 0
clust_medfev_binom_key_fitting$precip_lag1 <- 0
clust_medfev_binom_key_fitting$precip_lag2 <- 0
clust_medfev_binom_key_fitting$precip_lag3 <- 0
clust_medfev_binom_key_fitting$air.temp <- 0
clust_medfev_binom_key_fitting$air.temp_lag1 <- 0
clust_medfev_binom_key_fitting$air.temp_lag2 <- 0
clust_medfev_binom_key_fitting$air.temp_lag3 <- 0


covs_df_D1 <- covs_df_w_dates[, grepl("2017-08", names(covs_df_w_dates))]

clust_medfev_binom_key_fitting[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2017-11")), grep("lag3", names(clust_medfev_binom_key_fitting))] <- covs_df_D1[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2017-11")),]


covs_df_D2 <- covs_df_w_dates[, grepl("2017-09", names(covs_df_w_dates))]

clust_medfev_binom_key_fitting[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2017-12")), grep("lag3", names(clust_medfev_binom_key_fitting))] <- covs_df_D2[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2017-12")),]
clust_medfev_binom_key_fitting[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2017-11")), grep("lag2", names(clust_medfev_binom_key_fitting))] <- covs_df_D2[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2017-11")),]


covs_df_D3 <- covs_df_w_dates[, grepl("2017-10", names(covs_df_w_dates))]

clust_medfev_binom_key_fitting[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2018-01")), grep("lag3", names(clust_medfev_binom_key_fitting))] <- covs_df_D3[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2018-01")),]
clust_medfev_binom_key_fitting[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2017-12")), grep("lag2", names(clust_medfev_binom_key_fitting))] <- covs_df_D3[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2017-12")),]
clust_medfev_binom_key_fitting[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2017-11")), grep("lag1", names(clust_medfev_binom_key_fitting))] <- covs_df_D3[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2017-11")),]


covs_df_D4 <- covs_df_w_dates[, grepl("2017-11", names(covs_df_w_dates))]

clust_medfev_binom_key_fitting[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2018-02")), grep("lag3", names(clust_medfev_binom_key_fitting))] <- covs_df_D4[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2018-02")),]
clust_medfev_binom_key_fitting[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2018-01")), grep("lag2", names(clust_medfev_binom_key_fitting))] <- covs_df_D4[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2018-01")),]
clust_medfev_binom_key_fitting[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2017-12")), grep("lag1", names(clust_medfev_binom_key_fitting))] <- covs_df_D4[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2017-12")),]
clust_medfev_binom_key_fitting[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2017-11")), (grep("lag1", names(clust_medfev_binom_key_fitting))-1)] <- covs_df_D4[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2017-11")),]


covs_df_D5 <- covs_df_w_dates[, grepl("2017-12", names(covs_df_w_dates))]

clust_medfev_binom_key_fitting[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2018-03")), grep("lag3", names(clust_medfev_binom_key_fitting))] <- covs_df_D5[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2018-03")),]
clust_medfev_binom_key_fitting[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2018-02")), grep("lag2", names(clust_medfev_binom_key_fitting))] <- covs_df_D5[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2018-02")),]
clust_medfev_binom_key_fitting[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2018-01")), grep("lag1", names(clust_medfev_binom_key_fitting))] <- covs_df_D5[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2018-01")),]
clust_medfev_binom_key_fitting[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2017-12")), (grep("lag1", names(clust_medfev_binom_key_fitting))-1)] <- covs_df_D5[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2017-12")),]


covs_df_D6 <- covs_df_w_dates[, grepl("2017-13", names(covs_df_w_dates))]

clust_medfev_binom_key_fitting[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2018-03")), grep("lag2", names(clust_medfev_binom_key_fitting))] <- covs_df_D6[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2018-03")),]
clust_medfev_binom_key_fitting[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2018-02")), grep("lag1", names(clust_medfev_binom_key_fitting))] <- covs_df_D6[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2018-02")),]
clust_medfev_binom_key_fitting[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2018-01")), (grep("lag1", names(clust_medfev_binom_key_fitting))-1)] <- covs_df_D6[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2018-01")),]


covs_df_D7 <- covs_df_w_dates[, grepl("2017-14", names(covs_df_w_dates))]

clust_medfev_binom_key_fitting[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2018-03")), grep("lag1", names(clust_medfev_binom_key_fitting))] <- covs_df_D7[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2018-03")),]
clust_medfev_binom_key_fitting[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2018-02")), (grep("lag1", names(clust_medfev_binom_key_fitting))-1)] <- covs_df_D7[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2018-02")),]


covs_df_D8 <- covs_df_w_dates[, grepl("2017-15", names(covs_df_w_dates))]

clust_medfev_binom_key_fitting[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2018-03")), (grep("lag1", names(clust_medfev_binom_key_fitting))-1)] <- covs_df_D8[which(clust_medfev_binom_key_fitting$interview_date == as.yearmon("2018-03")),]


################################################################################################

## Normalizing covs

norm_list <- apply(clust_medfev_binom_key_fitting[,40:55], 2, get_norm)

clust_medfev_binom_key_fitting[,40:55] <- do.call(cbind, lapply(norm_list, function(X) X[[1]]))

scaling_factors <- as.data.frame(do.call(rbind, lapply(norm_list, function(X) X[[2]])))
names(scaling_factors) <- c("mean", "sd")


################################################################################################

## Making grouped covariates with inla.group

for ( i in 2:20 )
{
    clust_medfev_binom_key_fitting[,paste("precip_grouped_c", i, sep = "")] <- inla.group(clust_medfev_binom_key_fitting[["precip"]],
                                                                                          method = "cut",
                                                                                          n = i)
}

for ( i in 2:20 )
{
    clust_medfev_binom_key_fitting[,paste("precip_grouped_q", i, sep = "")] <- inla.group(clust_medfev_binom_key_fitting[["precip"]],
                                                                                          method = "quantile",
                                                                                          n = i)
}

for ( i in 2:20 )
{
    clust_medfev_binom_key_fitting[,paste("precip_lag1_grouped_c", i, sep = "")] <- inla.group(clust_medfev_binom_key_fitting[["precip_lag1"]],
                                                                                               method = "cut",
                                                                                               n = i)
}

for ( i in 2:20 )
{
    clust_medfev_binom_key_fitting[,paste("precip_lag1_grouped_q", i, sep = "")] <- inla.group(clust_medfev_binom_key_fitting[["precip_lag1"]],
                                                                                               method = "quantile",
                                                                                               n = i)
}

for ( i in 2:20 )
{
    clust_medfev_binom_key_fitting[,paste("precip_lag2_grouped_c", i, sep = "")] <- inla.group(clust_medfev_binom_key_fitting[["precip_lag2"]],
                                                                                               method = "cut",
                                                                                               n = i)
}

for ( i in 2:20 )
{
    clust_medfev_binom_key_fitting[,paste("precip_lag2_grouped_q", i, sep = "")] <- inla.group(clust_medfev_binom_key_fitting[["precip_lag2"]],
                                                                                               method = "quantile",
                                                                                               n = i)
}

for ( i in 2:20 )
{
    clust_medfev_binom_key_fitting[,paste("precip_lag3_grouped_c", i, sep = "")] <- inla.group(clust_medfev_binom_key_fitting[["precip_lag3"]],
                                                                                               method = "cut",
                                                                                               n = i)
}

for ( i in 2:20 )
{
    clust_medfev_binom_key_fitting[,paste("precip_lag3_grouped_q", i, sep = "")] <- inla.group(clust_medfev_binom_key_fitting[["precip_lag3"]],
                                                                                               method = "quantile",
                                                                                               n = i)
}




for ( i in 2:20 )
{
    clust_medfev_binom_key_fitting[,paste("air.temp_grouped_c", i, sep = "")] <- inla.group(clust_medfev_binom_key_fitting[["air.temp"]],
                                                                                            method = "cut",
                                                                                            n = i)
}

for ( i in 2:20 )
{
    clust_medfev_binom_key_fitting[,paste("air.temp_grouped_q", i, sep = "")] <- inla.group(clust_medfev_binom_key_fitting[["air.temp"]],
                                                                                            method = "quantile",
                                                                                            n = i)
}

for ( i in 2:20 )
{
    clust_medfev_binom_key_fitting[,paste("air.temp_lag1_grouped_c", i, sep = "")] <- inla.group(clust_medfev_binom_key_fitting[["air.temp_lag1"]],
                                                                                                 method = "cut",
                                                                                                 n = i)
}

for ( i in 2:20 )
{
    clust_medfev_binom_key_fitting[,paste("air.temp_lag1_grouped_q", i, sep = "")] <- inla.group(clust_medfev_binom_key_fitting[["air.temp_lag1"]],
                                                                                                 method = "quantile",
                                                                                                 n = i)
}

for ( i in 2:20 )
{
    clust_medfev_binom_key_fitting[,paste("air.temp_lag2_grouped_c", i, sep = "")] <- inla.group(clust_medfev_binom_key_fitting[["air.temp_lag2"]],
                                                                                                 method = "cut",
                                                                                                 n = i)
}

for ( i in 2:20 )
{
    clust_medfev_binom_key_fitting[,paste("air.temp_lag2_grouped_q", i, sep = "")] <- inla.group(clust_medfev_binom_key_fitting[["air.temp_lag2"]],
                                                                                                 method = "quantile",
                                                                                                 n = i)
}

for ( i in 2:20 )
{
    clust_medfev_binom_key_fitting[,paste("air.temp_lag3_grouped_c", i, sep = "")] <- inla.group(clust_medfev_binom_key_fitting[["air.temp_lag3"]],
                                                                                                 method = "cut",
                                                                                                 n = i)
}

for ( i in 2:20 )
{
    clust_medfev_binom_key_fitting[,paste("air.temp_lag3_grouped_q", i, sep = "")] <- inla.group(clust_medfev_binom_key_fitting[["air.temp_lag3"]],
                                                                                                 method = "quantile",
                                                                                                 n = i)
}





for ( i in 2:20 )
{
    clust_medfev_binom_key_fitting[,paste("long_grouped_c", i, sep = "")] <- inla.group(clust_medfev_binom_key_fitting$long,
                                                                                        method = "cut",
                                                                                        n = i)
}

for ( i in 2:20 )
{
    clust_medfev_binom_key_fitting[,paste("lat_grouped_c", i, sep = "")] <- inla.group(clust_medfev_binom_key_fitting$lat,
                                                                                       method = "cut",
                                                                                       n = i)
}


for ( i in 2:20 )
{
    clust_medfev_binom_key_fitting[,paste("TT_grouped_c", i, sep = "")] <- inla.group(clust_medfev_binom_key_fitting$public_tt,
                                                                                      method = "cut",
                                                                                      n = i)
}

for ( i in 2:20 )
{
    clust_medfev_binom_key_fitting[,paste("TT_grouped_q", i, sep = "")] <- inla.group(clust_medfev_binom_key_fitting$public_tt,
                                                                                      method = "quantile",
                                                                                      n = i)
}


################################################################################################

# Create data structure
cov_df <- cbind(b0 = 1, clust_medfev_binom_key_fitting[,40:435])
cov_df <- cbind(cov_df, clust_medfev_binom_key_fitting[,c("ADM1NAME", "NOMDEP")])

names(cov_df)[c(3,398:399)] <- c("pop", "Region", "District")


medfever.stack <- inla.stack(data = list(y = clust_medfev_binom_key_fitting$success,
                                         total = clust_medfev_binom_key_fitting$N),
                             A = list(A.pp, 1),
                             effects = list(medfever.spde.index, cov_df),
                             tag = "pp")


################################################################################################

# Data structure for prediction

sp.bdy <- as(burkina_shape, "SpatialPolygons")
p <- slot(as(burkina_shape, "SpatialPolygons"), "polygons")
v <- lapply(p, function(z) { SpatialPolygons(list(z)) })
BFA_win <- as.owin(v[[1]])


pred_pts_mesh.x <- seq(from = -5.52,
                       to = 2.41,
                       length.out = 951)
pred_pts_mesh.y <- seq(from = 9.39,
                       to = 15.09,
                       length.out = 682)


pred_pts_mesh <- expand.grid(x = pred_pts_mesh.x, y = pred_pts_mesh.y)
pred_pts_mesh_df <- data.frame(x = pred_pts_mesh$x, y = pred_pts_mesh$y, data = NA)

spdf_raw <- SpatialPixelsDataFrame(points = pred_pts_mesh_df[,c("x", "y")],
                                   data = pred_pts_mesh_df)

sgdf_raw <- as(spdf_raw, "SpatialGridDataFrame")
crs(sgdf_raw) <- crs(sp.bdy)
idx <- over(sgdf_raw, sp.bdy)
spdf <- as(sgdf_raw[!is.na(idx), ], "SpatialPixelsDataFrame")

pts.pred <- coordinates(spdf)
n.pred <- nrow(pts.pred)


# Get covariates (using subsetting operator in spatstat)

ppp.pred <- ppp(pts.pred[, 1], pts.pred[, 2], window = BFA_win)

cov_IM_list_Y_preds <- cov_IM_list_Y[c("precip_2017-11", "precip_2017-12",
                                       "precip_2017-13", "precip_2017-14",
                                       "air.temp_2017-11", "air.temp_2017-12",
                                       "air.temp_2017-13", "air.temp_2017-14",
                                       "NDVI", "public_tt", "pop_2017",
                                       "build_up", "log_pop", "log_build_up")]

covs.pred <- lapply(cov_IM_list_Y_preds, function(X){
    pixels <- nearest.pixel(ppp.pred$x, ppp.pred$y, X)
    sapply(1:npoints(ppp.pred), function(i) {
        X[pixels$row[i], pixels$col[i]]
    })
})

covs.pred$long <- ppp.pred$x
covs.pred$lat <- ppp.pred$y


################################################################################################

## Scaling pred covs

scaling_factors_new <- scaling_factors[c(12:9,16:13, 1:8),]


covs.pred <- lapply(1:length(covs.pred), function(i) {
    scale_preds(covs.pred[[i]], scaling_factors_new[i, "mean"], scaling_factors_new[i, "sd"])
})

names(covs.pred) <- c("precip_lag3", "precip_lag2",
                      "precip_lag1", "precip", 
                      "air.temp_lag3", "air.temp_lag2",
                      "air.temp_lag1", "air.temp",
                      "NDVI", "public_tt", "pop", "build_up",
                      "log_pop", "log_build_up", "long", "lat")


################################################################################################

for ( i in 2:20 )
{
    covs.pred[[paste("precip_lag3_grouped_c", i, sep = "")]] <- group_preds(covs.pred[["precip_lag3"]],
                                                                            unique(cov_df[,paste("precip_lag3_grouped_c", i, sep = "")]))
}

for ( i in 2:20 )
{
    covs.pred[[paste("precip_lag2_grouped_c", i, sep = "")]] <- group_preds(covs.pred[["precip_lag2"]],
                                                                            unique(cov_df[,paste("precip_lag2_grouped_c", i, sep = "")]))
}

for ( i in 2:20 )
{
    covs.pred[[paste("precip_lag1_grouped_c", i, sep = "")]] <- group_preds(covs.pred[["precip_lag1"]],
                                                                            unique(cov_df[,paste("precip_lag1_grouped_c", i, sep = "")]))
}

for ( i in 2:20 )
{
    covs.pred[[paste("precip_grouped_c", i, sep = "")]] <- group_preds(covs.pred[["precip"]],
                                                                       unique(cov_df[,paste("precip_grouped_c", i, sep = "")]))
}

for ( i in 2:20 )
{
    covs.pred[[paste("precip_lag3_grouped_q", i, sep = "")]] <- group_preds(covs.pred[["precip_lag3"]],
                                                                            unique(cov_df[,paste("precip_lag3_grouped_q", i, sep = "")]))
}

for ( i in 2:20 )
{
    covs.pred[[paste("precip_lag2_grouped_q", i, sep = "")]] <- group_preds(covs.pred[["precip_lag2"]],
                                                                            unique(cov_df[,paste("precip_lag2_grouped_q", i, sep = "")]))
}

for ( i in 2:20 )
{
    covs.pred[[paste("precip_lag1_grouped_q", i, sep = "")]] <- group_preds(covs.pred[["precip_lag1"]],
                                                                            unique(cov_df[,paste("precip_lag1_grouped_q", i, sep = "")]))
}

for ( i in 2:20 )
{
    covs.pred[[paste("precip_grouped_q", i, sep = "")]] <- group_preds(covs.pred[["precip"]],
                                                                       unique(cov_df[,paste("precip_grouped_q", i, sep = "")]))
}


for ( i in 2:20 )
{
    covs.pred[[paste("air.temp_lag3_grouped_c", i, sep = "")]] <- group_preds(covs.pred[["air.temp_lag3"]],
                                                                              unique(cov_df[,paste("air.temp_lag3_grouped_c", i, sep = "")]))
}

for ( i in 2:20 )
{
    covs.pred[[paste("air.temp_lag2_grouped_c", i, sep = "")]] <- group_preds(covs.pred[["air.temp_lag2"]],
                                                                              unique(cov_df[,paste("air.temp_lag2_grouped_c", i, sep = "")]))
}

for ( i in 2:20 )
{
    covs.pred[[paste("air.temp_lag1_grouped_c", i, sep = "")]] <- group_preds(covs.pred[["air.temp_lag1"]],
                                                                              unique(cov_df[,paste("air.temp_lag1_grouped_c", i, sep = "")]))
}

for ( i in 2:20 )
{
    covs.pred[[paste("air.temp_grouped_c", i, sep = "")]] <- group_preds(covs.pred[["air.temp"]],
                                                                         unique(cov_df[,paste("air.temp_grouped_c", i, sep = "")]))
}

for ( i in 2:20 )
{
    covs.pred[[paste("air.temp_lag3_grouped_q", i, sep = "")]] <- group_preds(covs.pred[["air.temp_lag3"]],
                                                                              unique(cov_df[,paste("air.temp_lag3_grouped_q", i, sep = "")]))
}

for ( i in 2:20 )
{
    covs.pred[[paste("air.temp_lag2_grouped_q", i, sep = "")]] <- group_preds(covs.pred[["air.temp_lag2"]],
                                                                              unique(cov_df[,paste("air.temp_lag2_grouped_q", i, sep = "")]))
}

for ( i in 2:20 )
{
    covs.pred[[paste("air.temp_lag1_grouped_q", i, sep = "")]] <- group_preds(covs.pred[["air.temp_lag1"]],
                                                                              unique(cov_df[,paste("air.temp_lag1_grouped_q", i, sep = "")]))
}

for ( i in 2:20 )
{
    covs.pred[[paste("air.temp_grouped_q", i, sep = "")]] <- group_preds(covs.pred[["air.temp"]],
                                                                         unique(cov_df[,paste("air.temp_grouped_q", i, sep = "")]))
}



for ( i in 2:20 )
{
    covs.pred[[paste("long_grouped_c", i, sep = "")]] <- group_preds(covs.pred[["long"]],
                                                                     unique(cov_df[,paste("long_grouped_c", i, sep = "")]))
}

for ( i in 2:20 )
{
    covs.pred[[paste("lat_grouped_c", i, sep = "")]] <- group_preds(covs.pred[["lat"]],
                                                                    unique(cov_df[,paste("lat_grouped_c", i, sep = "")]))
}


for ( i in 2:20 )
{
    covs.pred[[paste("TT_grouped_c", i, sep = "")]] <- group_preds(covs.pred[["public_tt"]],
                                                                   unique(cov_df[,paste("TT_grouped_c", i, sep = "")]))
}

for ( i in 2:20 )
{
    covs.pred[[paste("TT_grouped_q", i, sep = "")]] <- group_preds(covs.pred[["public_tt"]],
                                                                   unique(cov_df[,paste("TT_grouped_q", i, sep = "")]))
}


################################################################################################

# Prediction points
A.pred <- inla.spde.make.A(mesh = medfever.mesh,
                           loc = cbind(ppp.pred$x, ppp.pred$y))

covs.pred$b0 <- rep(1, npoints(ppp.pred))



## Add districts and regions here

ppp.pred_df <- coords(ppp.pred)
coordinates(ppp.pred_df) <- ~x+y
crs(ppp.pred_df) <- crs(DS_shape)

## Finding NAs twice because sometimes there are 2 in a row
#  This methodology isnt perfect but works. Spatial correlation
#  of District/Region names should be super high
covs.pred$District <- over(ppp.pred_df, DS_shape)$NOMDEP
covs.pred$District[which(is.na(covs.pred$District))] <- covs.pred$District[which(is.na(covs.pred$District))+1]
covs.pred$District[which(is.na(covs.pred$District))] <- covs.pred$District[which(is.na(covs.pred$District))+1]

covs.pred$Region <- over(ppp.pred_df, DS_shape)$NOMREGION
covs.pred$Region[which(is.na(covs.pred$Region))] <- covs.pred$Region[which(is.na(covs.pred$Region))+1]
covs.pred$Region[which(is.na(covs.pred$Region))] <- covs.pred$Region[which(is.na(covs.pred$Region))+1]



medfever.stack.pred <- inla.stack(data = list(y = NA, total = NA),
                                  A = list(A.pred, 1),
                                  effects = list(medfever.spde.index, covs.pred), 
                                  tag = "pred")


################################################################################################

join.stack <- inla.stack(medfever.stack, medfever.stack.pred)

save_list <- list(medfever.spde,
                  join.stack,
                  covs, cov_df,
                  covs.pred,
                  clust_medfev_binom_key_fitting,
                  spdf,
                  scaling_factors_new)


save(save_list, file = "~/Desktop/SAE_with_more_lags/medfever_stack_2017_norm_w_dates.RData")





 

























