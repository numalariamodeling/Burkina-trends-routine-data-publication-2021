
rm(list = ls(all = TRUE))


x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "stringr",
       "sp", "rgdal", "raster", "foreign",
       "lubridate", "RColorBrewer","sf")

lapply(x, require, character.only = TRUE)


require("maditr")
require("INLA")
require("spdep")

require("spatstat")
require("maptools")



#####################################################################################################

## Loading

burkina_shape <- readOGR("~/Box/NU-malaria-team/data/burkina_shapefiles/BFA_adm_shp/BFA_adm0.shp")


precip_2014_06 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/precip_era5/precip_era5_year_2014_month_06.tif")
precip_2014_07 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/precip_era5/precip_era5_year_2014_month_07.tif")
precip_2014_08 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/precip_era5/precip_era5_year_2014_month_08.tif")
precip_2014_09 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/precip_era5/precip_era5_year_2014_month_09.tif")
precip_2014_10 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/precip_era5/precip_era5_year_2014_month_10.tif")
precip_2014_11 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/precip_era5/precip_era5_year_2014_month_11.tif")
precip_2014_12 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/precip_era5/precip_era5_year_2014_month_12.tif")


precip_2017_08 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/precip_era5/precip_era5_year_2017_month_08.tif")
precip_2017_09 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/precip_era5/precip_era5_year_2017_month_09.tif")
precip_2017_10 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/precip_era5/precip_era5_year_2017_month_10.tif")
precip_2017_11 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/precip_era5/precip_era5_year_2017_month_11.tif")
precip_2017_12 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/precip_era5/precip_era5_year_2017_month_12.tif")
precip_2017_13 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/precip_era5/precip_era5_year_2018_month_01.tif")
precip_2017_14 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/precip_era5/precip_era5_year_2018_month_02.tif")
precip_2017_15 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/precip_era5/precip_era5_year_2018_month_03.tif")




air.temp_2014_06 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/air_temp_era5/air_temp_era5_year_2014_month_06.tif")
air.temp_2014_07 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/air_temp_era5/air_temp_era5_year_2014_month_07.tif")
air.temp_2014_08 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/air_temp_era5/air_temp_era5_year_2014_month_08.tif")
air.temp_2014_09 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/air_temp_era5/air_temp_era5_year_2014_month_09.tif")
air.temp_2014_10 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/air_temp_era5/air_temp_era5_year_2014_month_10.tif")
air.temp_2014_11 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/air_temp_era5/air_temp_era5_year_2014_month_11.tif")
air.temp_2014_12 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/air_temp_era5/air_temp_era5_year_2014_month_12.tif")


air.temp_2017_08 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/air_temp_era5/air_temp_era5_year_2017_month_08.tif")
air.temp_2017_09 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/air_temp_era5/air_temp_era5_year_2017_month_09.tif")
air.temp_2017_10 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/air_temp_era5/air_temp_era5_year_2017_month_10.tif")
air.temp_2017_11 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/air_temp_era5/air_temp_era5_year_2017_month_11.tif")
air.temp_2017_12 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/air_temp_era5/air_temp_era5_year_2017_month_12.tif")
air.temp_2017_13 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/air_temp_era5/air_temp_era5_year_2018_month_01.tif")
air.temp_2017_14 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/air_temp_era5/air_temp_era5_year_2018_month_02.tif")
air.temp_2017_15 <- raster("~/Box/NU-malaria-team/data/africa_health_district_climate/climate/global/air_temp_era5/air_temp_era5_year_2018_month_03.tif")




pop_2014 <- raster("~/OneDrive/Desktop/SAE project/pop/bfa_ppp_2014_1km_Aggregated.tif")
pop_2017 <- raster("~/OneDrive/Desktop/SAE project/pop/bfa_ppp_2017_1km_Aggregated.tif")


FB_pop <- raster("~/Box/NU-malaria-team/data/burkina_rasterfiles/BF_pop_raster_Facebook_20181001.tif")


BFA_NDVI_182 <- raster("~/Desktop/20140701_20141231/date 182/merge_raster.tif")
BFA_NDVI_213 <- raster("~/Desktop/20140701_20141231/date 213/merge_raster.tif")
BFA_NDVI_244 <- raster("~/Desktop/20140701_20141231/date 244/merge_raster.tif")
BFA_NDVI_274 <- raster("~/Desktop/20140701_20141231/date 274/merge_raster.tif")
BFA_NDVI_305 <- raster("~/Desktop/20140701_20141231/date 305/merge_raster.tif")
BFA_NDVI_335 <- raster("~/Desktop/20140701_20141231/date 335/merge_raster.tif")



# BFA_friction_official <- raster("~/Box/NU-malaria-team/data/burkina_rasterfiles/accessibility/friction_surface_MAP_official.tif")
# 
# 
# BFA_friction_walking <- raster("~/Box/NU-malaria-team/data/burkina_rasterfiles/accessibility/friction_surface_MAP_walking.tif")


BFA_public_tt <- raster("~/Box/NU-malaria-team/data/burkina_rasterfiles/accessibility/public_hf_tt_surface_Burkina Faso.tif")


# BFA_inpatient_tt <- raster("~/Box/NU-malaria-team/data/burkina_rasterfiles/accessibility/inpatient_hf_tt_surface_Burkina Faso.tif")


BFA_build_up <- raster("~/OneDrive/Desktop/SAE project/build-up/BFA_build_up.tif")


#####################################################################################################


precip_2014_06_BFA <- crop(precip_2014_06, burkina_shape)
precip_2014_07_BFA <- crop(precip_2014_07, burkina_shape)
precip_2014_08_BFA <- crop(precip_2014_08, burkina_shape)
precip_2014_09_BFA <- crop(precip_2014_09, burkina_shape)
precip_2014_10_BFA <- crop(precip_2014_10, burkina_shape)
precip_2014_11_BFA <- crop(precip_2014_11, burkina_shape)
precip_2014_12_BFA <- crop(precip_2014_12, burkina_shape)

precip_2017_08_BFA <- crop(precip_2017_08, burkina_shape)
precip_2017_09_BFA <- crop(precip_2017_09, burkina_shape)
precip_2017_10_BFA <- crop(precip_2017_10, burkina_shape)
precip_2017_11_BFA <- crop(precip_2017_11, burkina_shape)
precip_2017_12_BFA <- crop(precip_2017_12, burkina_shape)
precip_2017_13_BFA <- crop(precip_2017_13, burkina_shape)
precip_2017_14_BFA <- crop(precip_2017_14, burkina_shape)
precip_2017_15_BFA <- crop(precip_2017_15, burkina_shape)


air.temp_2014_06_BFA <- crop(air.temp_2014_06, burkina_shape)
air.temp_2014_07_BFA <- crop(air.temp_2014_07, burkina_shape)
air.temp_2014_08_BFA <- crop(air.temp_2014_08, burkina_shape)
air.temp_2014_09_BFA <- crop(air.temp_2014_09, burkina_shape)
air.temp_2014_10_BFA <- crop(air.temp_2014_10, burkina_shape)
air.temp_2014_11_BFA <- crop(air.temp_2014_11, burkina_shape)
air.temp_2014_12_BFA <- crop(air.temp_2014_12, burkina_shape)

air.temp_2017_08_BFA <- crop(air.temp_2017_08, burkina_shape)
air.temp_2017_09_BFA <- crop(air.temp_2017_09, burkina_shape)
air.temp_2017_10_BFA <- crop(air.temp_2017_10, burkina_shape)
air.temp_2017_11_BFA <- crop(air.temp_2017_11, burkina_shape)
air.temp_2017_12_BFA <- crop(air.temp_2017_12, burkina_shape)
air.temp_2017_13_BFA <- crop(air.temp_2017_13, burkina_shape)
air.temp_2017_14_BFA <- crop(air.temp_2017_14, burkina_shape)
air.temp_2017_15_BFA <- crop(air.temp_2017_15, burkina_shape)




## Scaling


# making higher res image
precip_2014_06_BFA_hiRes <- projectRaster(precip_2014_06_BFA,
                                          # res = c(.1, .1),
                                          # res = c(0.07989773, 0.05732323),
                                          res = c(0.008333333, 0.008333333),
                                          crs = "+proj=longlat +datum=WGS84 +no_defs")
# cropping BFA shape
precip_2014_06_BFA_clip <- mask(x = precip_2014_06_BFA_hiRes, mask = burkina_shape)

precip_2014_07_BFA_hiRes <- projectRaster(precip_2014_07_BFA,
                                          res = c(0.008333333, 0.008333333),
                                          crs = "+proj=longlat +datum=WGS84 +no_defs")
precip_2014_07_BFA_clip <- mask(x = precip_2014_07_BFA_hiRes, mask = burkina_shape)

precip_2014_08_BFA_hiRes <- projectRaster(precip_2014_08_BFA,
                                          res = c(0.008333333, 0.008333333),
                                          crs = "+proj=longlat +datum=WGS84 +no_defs")
precip_2014_08_BFA_clip <- mask(x = precip_2014_08_BFA_hiRes, mask = burkina_shape)

precip_2014_09_BFA_hiRes <- projectRaster(precip_2014_09_BFA,
                                           res = c(0.008333333, 0.008333333),
                                           crs = "+proj=longlat +datum=WGS84 +no_defs")
precip_2014_09_BFA_clip <- mask(x = precip_2014_09_BFA_hiRes, mask = burkina_shape)

precip_2014_10_BFA_hiRes <- projectRaster(precip_2014_10_BFA,
                                           res = c(0.008333333, 0.008333333),
                                           crs = "+proj=longlat +datum=WGS84 +no_defs")
precip_2014_10_BFA_clip <- mask(x = precip_2014_10_BFA_hiRes, mask = burkina_shape)

precip_2014_11_BFA_hiRes <- projectRaster(precip_2014_11_BFA,
                                           res = c(0.008333333, 0.008333333),
                                           crs = "+proj=longlat +datum=WGS84 +no_defs")
precip_2014_11_BFA_clip <- mask(x = precip_2014_11_BFA_hiRes, mask = burkina_shape)

precip_2014_12_BFA_hiRes <- projectRaster(precip_2014_12_BFA,
                                           res = c(0.008333333, 0.008333333),
                                           crs = "+proj=longlat +datum=WGS84 +no_defs")
precip_2014_12_BFA_clip <- mask(x = precip_2014_12_BFA_hiRes, mask = burkina_shape)





# making higher res image
precip_2017_08_BFA_hiRes <- projectRaster(precip_2017_08_BFA,
                                          # res = c(.1, .1),
                                          # res = c(0.07989773, 0.05732323),
                                          res = c(0.008333333, 0.008333333),
                                          crs = "+proj=longlat +datum=WGS84 +no_defs")
# cropping BFA shape
precip_2017_08_BFA_clip <- mask(x = precip_2017_08_BFA_hiRes, mask = burkina_shape)

precip_2017_09_BFA_hiRes <- projectRaster(precip_2017_09_BFA,
                                          res = c(0.008333333, 0.008333333),
                                          crs = "+proj=longlat +datum=WGS84 +no_defs")
precip_2017_09_BFA_clip <- mask(x = precip_2017_09_BFA_hiRes, mask = burkina_shape)

precip_2017_10_BFA_hiRes <- projectRaster(precip_2017_10_BFA,
                                          res = c(0.008333333, 0.008333333),
                                          crs = "+proj=longlat +datum=WGS84 +no_defs")
precip_2017_10_BFA_clip <- mask(x = precip_2017_10_BFA_hiRes, mask = burkina_shape)

precip_2017_11_BFA_hiRes <- projectRaster(precip_2017_11_BFA,
                                          res = c(0.008333333, 0.008333333),
                                          crs = "+proj=longlat +datum=WGS84 +no_defs")
precip_2017_11_BFA_clip <- mask(x = precip_2017_11_BFA_hiRes, mask = burkina_shape)

precip_2017_12_BFA_hiRes <- projectRaster(precip_2017_12_BFA,
                                           res = c(0.008333333, 0.008333333),
                                           crs = "+proj=longlat +datum=WGS84 +no_defs")
precip_2017_12_BFA_clip <- mask(x = precip_2017_12_BFA_hiRes, mask = burkina_shape)

precip_2017_13_BFA_hiRes <- projectRaster(precip_2017_13_BFA,
                                           res = c(0.008333333, 0.008333333),
                                           crs = "+proj=longlat +datum=WGS84 +no_defs")
precip_2017_13_BFA_clip <- mask(x = precip_2017_13_BFA_hiRes, mask = burkina_shape)

precip_2017_14_BFA_hiRes <- projectRaster(precip_2017_14_BFA,
                                           res = c(0.008333333, 0.008333333),
                                           crs = "+proj=longlat +datum=WGS84 +no_defs")
precip_2017_14_BFA_clip <- mask(x = precip_2017_14_BFA_hiRes, mask = burkina_shape)

precip_2017_15_BFA_hiRes <- projectRaster(precip_2017_15_BFA,
                                           res = c(0.008333333, 0.008333333),
                                           crs = "+proj=longlat +datum=WGS84 +no_defs")
precip_2017_15_BFA_clip <- mask(x = precip_2017_15_BFA_hiRes, mask = burkina_shape)






# making higher res image
air.temp_2014_06_BFA_hiRes <- projectRaster(air.temp_2014_06_BFA,
                                            res = c(0.008333333, 0.008333333),
                                            crs = "+proj=longlat +datum=WGS84 +no_defs")
air.temp_2014_06_BFA_clip <- mask(x = air.temp_2014_06_BFA_hiRes, mask = burkina_shape)

air.temp_2014_07_BFA_hiRes <- projectRaster(air.temp_2014_07_BFA,
                                            res = c(0.008333333, 0.008333333),
                                            crs = "+proj=longlat +datum=WGS84 +no_defs")
air.temp_2014_07_BFA_clip <- mask(x = air.temp_2014_07_BFA_hiRes, mask = burkina_shape)

air.temp_2014_08_BFA_hiRes <- projectRaster(air.temp_2014_08_BFA,
                                          res = c(0.008333333, 0.008333333),
                                          crs = "+proj=longlat +datum=WGS84 +no_defs")
air.temp_2014_08_BFA_clip <- mask(x = air.temp_2014_08_BFA_hiRes, mask = burkina_shape)

air.temp_2014_09_BFA_hiRes <- projectRaster(air.temp_2014_09_BFA,
                                          res = c(0.008333333, 0.008333333),
                                          crs = "+proj=longlat +datum=WGS84 +no_defs")
air.temp_2014_09_BFA_clip <- mask(x = air.temp_2014_09_BFA_hiRes, mask = burkina_shape)

air.temp_2014_10_BFA_hiRes <- projectRaster(air.temp_2014_10_BFA,
                                          res = c(0.008333333, 0.008333333),
                                          crs = "+proj=longlat +datum=WGS84 +no_defs")
air.temp_2014_10_BFA_clip <- mask(x = air.temp_2014_10_BFA_hiRes, mask = burkina_shape)

air.temp_2014_11_BFA_hiRes <- projectRaster(air.temp_2014_11_BFA,
                                          res = c(0.008333333, 0.008333333),
                                          crs = "+proj=longlat +datum=WGS84 +no_defs")
air.temp_2014_11_BFA_clip <- mask(x = air.temp_2014_11_BFA_hiRes, mask = burkina_shape)

air.temp_2014_12_BFA_hiRes <- projectRaster(air.temp_2014_12_BFA,
                                          res = c(0.008333333, 0.008333333),
                                          crs = "+proj=longlat +datum=WGS84 +no_defs")
air.temp_2014_12_BFA_clip <- mask(x = air.temp_2014_12_BFA_hiRes, mask = burkina_shape)

##

air.temp_2017_08_BFA_hiRes <- projectRaster(air.temp_2017_08_BFA,
                                            res = c(0.008333333, 0.008333333),
                                            crs = "+proj=longlat +datum=WGS84 +no_defs")
air.temp_2017_08_BFA_clip <- mask(x = air.temp_2017_08_BFA_hiRes, mask = burkina_shape)

air.temp_2017_09_BFA_hiRes <- projectRaster(air.temp_2017_09_BFA,
                                            res = c(0.008333333, 0.008333333),
                                            crs = "+proj=longlat +datum=WGS84 +no_defs")
air.temp_2017_09_BFA_clip <- mask(x = air.temp_2017_09_BFA_hiRes, mask = burkina_shape)

air.temp_2017_10_BFA_hiRes <- projectRaster(air.temp_2017_10_BFA,
                                            res = c(0.008333333, 0.008333333),
                                            crs = "+proj=longlat +datum=WGS84 +no_defs")
air.temp_2017_10_BFA_clip <- mask(x = air.temp_2017_10_BFA_hiRes, mask = burkina_shape)

air.temp_2017_11_BFA_hiRes <- projectRaster(air.temp_2017_11_BFA,
                                          res = c(0.008333333, 0.008333333),
                                          crs = "+proj=longlat +datum=WGS84 +no_defs")
air.temp_2017_11_BFA_clip <- mask(x = air.temp_2017_11_BFA_hiRes, mask = burkina_shape)

air.temp_2017_12_BFA_hiRes <- projectRaster(air.temp_2017_12_BFA,
                                          res = c(0.008333333, 0.008333333),
                                          crs = "+proj=longlat +datum=WGS84 +no_defs")
air.temp_2017_12_BFA_clip <- mask(x = air.temp_2017_12_BFA_hiRes, mask = burkina_shape)

air.temp_2017_13_BFA_hiRes <- projectRaster(air.temp_2017_13_BFA,
                                          res = c(0.008333333, 0.008333333),
                                          crs = "+proj=longlat +datum=WGS84 +no_defs")
air.temp_2017_13_BFA_clip <- mask(x = air.temp_2017_13_BFA_hiRes, mask = burkina_shape)

air.temp_2017_14_BFA_hiRes <- projectRaster(air.temp_2017_14_BFA,
                                          res = c(0.008333333, 0.008333333),
                                          crs = "+proj=longlat +datum=WGS84 +no_defs")
air.temp_2017_14_BFA_clip <- mask(x = air.temp_2017_14_BFA_hiRes, mask = burkina_shape)

air.temp_2017_15_BFA_hiRes <- projectRaster(air.temp_2017_15_BFA,
                                          res = c(0.008333333, 0.008333333),
                                          crs = "+proj=longlat +datum=WGS84 +no_defs")
air.temp_2017_15_BFA_clip <- mask(x = air.temp_2017_15_BFA_hiRes, mask = burkina_shape)






BFA_NDVI_182_hiRes <- projectRaster(BFA_NDVI_182,
                                    res = c(0.008333333, 0.008333333),
                                    crs = "+proj=longlat +datum=WGS84 +no_defs")

BFA_NDVI_213_hiRes <- projectRaster(BFA_NDVI_213,
                                    res = c(0.008333333, 0.008333333),
                                    crs = "+proj=longlat +datum=WGS84 +no_defs")

BFA_NDVI_244_hiRes <- projectRaster(BFA_NDVI_244,
                                    res = c(0.008333333, 0.008333333),
                                    crs = "+proj=longlat +datum=WGS84 +no_defs")

BFA_NDVI_274_hiRes <- projectRaster(BFA_NDVI_274,
                                    res = c(0.008333333, 0.008333333),
                                    crs = "+proj=longlat +datum=WGS84 +no_defs")

BFA_NDVI_305_hiRes <- projectRaster(BFA_NDVI_305,
                                    res = c(0.008333333, 0.008333333),
                                    crs = "+proj=longlat +datum=WGS84 +no_defs")

BFA_NDVI_335_hiRes <- projectRaster(BFA_NDVI_335,
                                    res = c(0.008333333, 0.008333333),
                                    crs = "+proj=longlat +datum=WGS84 +no_defs")



pop_2014_loRes <- projectRaster(pop_2014,
                                res = c(0.008333333, 0.008333333),
                                crs = "+proj=longlat +datum=WGS84 +no_defs")

pop_2017_loRes <- projectRaster(pop_2017,
                                res = c(0.008333333, 0.008333333),
                                crs = "+proj=longlat +datum=WGS84 +no_defs")




BFA_build_up_hiRes <- projectRaster(BFA_build_up,
                                    res = c(0.008333333, 0.008333333),
                                    crs = "+proj=longlat +datum=WGS84 +no_defs")




#####################################################################################################

## Matching covariate layers

precip_2014_06_BFA_clip <- raster::intersect(precip_2014_06_BFA_clip, BFA_public_tt)
precip_2014_07_BFA_clip <- raster::intersect(precip_2014_07_BFA_clip, BFA_public_tt)
precip_2014_08_BFA_clip <- raster::intersect(precip_2014_08_BFA_clip, BFA_public_tt)
precip_2014_09_BFA_clip <- raster::intersect(precip_2014_09_BFA_clip, BFA_public_tt)
precip_2014_10_BFA_clip <- raster::intersect(precip_2014_10_BFA_clip, BFA_public_tt)
precip_2014_11_BFA_clip <- raster::intersect(precip_2014_11_BFA_clip, BFA_public_tt)
precip_2014_12_BFA_clip <- raster::intersect(precip_2014_12_BFA_clip, BFA_public_tt)

precip_2017_08_BFA_clip <- raster::intersect(precip_2017_08_BFA_clip, BFA_public_tt)
precip_2017_09_BFA_clip <- raster::intersect(precip_2017_09_BFA_clip, BFA_public_tt)
precip_2017_10_BFA_clip <- raster::intersect(precip_2017_10_BFA_clip, BFA_public_tt)
precip_2017_11_BFA_clip <- raster::intersect(precip_2017_11_BFA_clip, BFA_public_tt)
precip_2017_12_BFA_clip <- raster::intersect(precip_2017_12_BFA_clip, BFA_public_tt)
precip_2017_13_BFA_clip <- raster::intersect(precip_2017_13_BFA_clip, BFA_public_tt)
precip_2017_14_BFA_clip <- raster::intersect(precip_2017_14_BFA_clip, BFA_public_tt)
precip_2017_15_BFA_clip <- raster::intersect(precip_2017_15_BFA_clip, BFA_public_tt)



air.temp_2014_06_BFA_clip <- raster::intersect(air.temp_2014_06_BFA_clip, BFA_public_tt)
air.temp_2014_07_BFA_clip <- raster::intersect(air.temp_2014_07_BFA_clip, BFA_public_tt)
air.temp_2014_08_BFA_clip <- raster::intersect(air.temp_2014_08_BFA_clip, BFA_public_tt)
air.temp_2014_09_BFA_clip <- raster::intersect(air.temp_2014_09_BFA_clip, BFA_public_tt)
air.temp_2014_10_BFA_clip <- raster::intersect(air.temp_2014_10_BFA_clip, BFA_public_tt)
air.temp_2014_11_BFA_clip <- raster::intersect(air.temp_2014_11_BFA_clip, BFA_public_tt)
air.temp_2014_12_BFA_clip <- raster::intersect(air.temp_2014_12_BFA_clip, BFA_public_tt)

air.temp_2017_08_BFA_clip <- raster::intersect(air.temp_2017_08_BFA_clip, BFA_public_tt)
air.temp_2017_09_BFA_clip <- raster::intersect(air.temp_2017_09_BFA_clip, BFA_public_tt)
air.temp_2017_10_BFA_clip <- raster::intersect(air.temp_2017_10_BFA_clip, BFA_public_tt)
air.temp_2017_11_BFA_clip <- raster::intersect(air.temp_2017_11_BFA_clip, BFA_public_tt)
air.temp_2017_12_BFA_clip <- raster::intersect(air.temp_2017_12_BFA_clip, BFA_public_tt)
air.temp_2017_13_BFA_clip <- raster::intersect(air.temp_2017_13_BFA_clip, BFA_public_tt)
air.temp_2017_14_BFA_clip <- raster::intersect(air.temp_2017_14_BFA_clip, BFA_public_tt)
air.temp_2017_15_BFA_clip <- raster::intersect(air.temp_2017_15_BFA_clip, BFA_public_tt)




BFA_NDVI_hiRes_182_clip <- raster::intersect(BFA_NDVI_182_hiRes, BFA_public_tt)
BFA_NDVI_hiRes_213_clip <- raster::intersect(BFA_NDVI_213_hiRes, BFA_public_tt)
BFA_NDVI_hiRes_244_clip <- raster::intersect(BFA_NDVI_244_hiRes, BFA_public_tt)
BFA_NDVI_hiRes_274_clip <- raster::intersect(BFA_NDVI_274_hiRes, BFA_public_tt)
BFA_NDVI_hiRes_305_clip <- raster::intersect(BFA_NDVI_305_hiRes, BFA_public_tt)
BFA_NDVI_hiRes_335_clip <- raster::intersect(BFA_NDVI_335_hiRes, BFA_public_tt)



pop_2014_loRes_clip <- raster::intersect(pop_2014_loRes, BFA_public_tt)
pop_2017_loRes_clip <- raster::intersect(pop_2017_loRes, BFA_public_tt)



BFA_build_up_hiRes_clip <- raster::intersect(BFA_build_up_hiRes, BFA_public_tt)



#####################################################################################################

## Making IM files

precip_2014_06_BFA_clip <- maptools::as.im.RasterLayer(precip_2014_06_BFA_clip)
precip_2014_07_BFA_clip <- maptools::as.im.RasterLayer(precip_2014_07_BFA_clip)
precip_2014_08_BFA_clip <- maptools::as.im.RasterLayer(precip_2014_08_BFA_clip)
precip_2014_09_BFA_clip <- maptools::as.im.RasterLayer(precip_2014_09_BFA_clip)
precip_2014_10_BFA_clip <- maptools::as.im.RasterLayer(precip_2014_10_BFA_clip)
precip_2014_11_BFA_clip <- maptools::as.im.RasterLayer(precip_2014_11_BFA_clip)
precip_2014_12_BFA_clip <- maptools::as.im.RasterLayer(precip_2014_12_BFA_clip)

precip_2017_08_BFA_clip <- maptools::as.im.RasterLayer(precip_2017_08_BFA_clip)
precip_2017_09_BFA_clip <- maptools::as.im.RasterLayer(precip_2017_09_BFA_clip)
precip_2017_10_BFA_clip <- maptools::as.im.RasterLayer(precip_2017_10_BFA_clip)
precip_2017_11_BFA_clip <- maptools::as.im.RasterLayer(precip_2017_11_BFA_clip)
precip_2017_12_BFA_clip <- maptools::as.im.RasterLayer(precip_2017_12_BFA_clip)
precip_2017_13_BFA_clip <- maptools::as.im.RasterLayer(precip_2017_13_BFA_clip)
precip_2017_14_BFA_clip <- maptools::as.im.RasterLayer(precip_2017_14_BFA_clip)
precip_2017_15_BFA_clip <- maptools::as.im.RasterLayer(precip_2017_15_BFA_clip)


air.temp_2014_06_BFA_clip <- maptools::as.im.RasterLayer(air.temp_2014_06_BFA_clip)
air.temp_2014_07_BFA_clip <- maptools::as.im.RasterLayer(air.temp_2014_07_BFA_clip)
air.temp_2014_08_BFA_clip <- maptools::as.im.RasterLayer(air.temp_2014_08_BFA_clip)
air.temp_2014_09_BFA_clip <- maptools::as.im.RasterLayer(air.temp_2014_09_BFA_clip)
air.temp_2014_10_BFA_clip <- maptools::as.im.RasterLayer(air.temp_2014_10_BFA_clip)
air.temp_2014_11_BFA_clip <- maptools::as.im.RasterLayer(air.temp_2014_11_BFA_clip)
air.temp_2014_12_BFA_clip <- maptools::as.im.RasterLayer(air.temp_2014_12_BFA_clip)

air.temp_2017_08_BFA_clip <- maptools::as.im.RasterLayer(air.temp_2017_08_BFA_clip)
air.temp_2017_09_BFA_clip <- maptools::as.im.RasterLayer(air.temp_2017_09_BFA_clip)
air.temp_2017_10_BFA_clip <- maptools::as.im.RasterLayer(air.temp_2017_10_BFA_clip)
air.temp_2017_11_BFA_clip <- maptools::as.im.RasterLayer(air.temp_2017_11_BFA_clip)
air.temp_2017_12_BFA_clip <- maptools::as.im.RasterLayer(air.temp_2017_12_BFA_clip)
air.temp_2017_13_BFA_clip <- maptools::as.im.RasterLayer(air.temp_2017_13_BFA_clip)
air.temp_2017_14_BFA_clip <- maptools::as.im.RasterLayer(air.temp_2017_14_BFA_clip)
air.temp_2017_15_BFA_clip <- maptools::as.im.RasterLayer(air.temp_2017_15_BFA_clip)



BFA_NDVI_182 <- maptools::as.im.RasterLayer(BFA_NDVI_hiRes_182_clip)
BFA_NDVI_213 <- maptools::as.im.RasterLayer(BFA_NDVI_hiRes_213_clip)
BFA_NDVI_244 <- maptools::as.im.RasterLayer(BFA_NDVI_hiRes_244_clip)
BFA_NDVI_274 <- maptools::as.im.RasterLayer(BFA_NDVI_hiRes_274_clip)
BFA_NDVI_305 <- maptools::as.im.RasterLayer(BFA_NDVI_hiRes_305_clip)
BFA_NDVI_335 <- maptools::as.im.RasterLayer(BFA_NDVI_hiRes_335_clip)


BFA_public_tt <- maptools::as.im.RasterLayer(BFA_public_tt)


pop_2014 <- maptools::as.im.RasterLayer(pop_2014_loRes_clip)
pop_2017 <- maptools::as.im.RasterLayer(pop_2017_loRes_clip)



BFA_build_up <- maptools::as.im.RasterLayer(BFA_build_up_hiRes_clip)



BFA_NDVI_182[which(BFA_NDVI_182$v < 0)] <- NA
BFA_NDVI_182$v <- BFA_NDVI_182$v / 10000000

BFA_NDVI_213[which(BFA_NDVI_213$v < 0)] <- NA
BFA_NDVI_213$v <- BFA_NDVI_213$v / 10000000

BFA_NDVI_244[which(BFA_NDVI_244$v < 0)] <- NA
BFA_NDVI_244$v <- BFA_NDVI_244$v / 10000000

BFA_NDVI_274[which(BFA_NDVI_274$v < 0)] <- NA
BFA_NDVI_274$v <- BFA_NDVI_274$v / 10000000

BFA_NDVI_305[which(BFA_NDVI_305$v < 0)] <- NA
BFA_NDVI_305$v <- BFA_NDVI_305$v / 10000000

BFA_NDVI_335[which(BFA_NDVI_335$v < 0)] <- NA
BFA_NDVI_335$v <- BFA_NDVI_335$v / 10000000




cov_IM_list <- list("precip_2014-06" = precip_2014_06_BFA_clip,
                    "precip_2014-07" = precip_2014_07_BFA_clip,
                    "precip_2014-08" = precip_2014_08_BFA_clip,
                    "precip_2014-09" = precip_2014_09_BFA_clip,
                    "precip_2014-10" = precip_2014_10_BFA_clip,
                    "precip_2014-11" = precip_2014_11_BFA_clip,
                    "precip_2014-12" = precip_2014_12_BFA_clip,
                    "precip_2017-08" = precip_2017_08_BFA_clip,
                    "precip_2017-09" = precip_2017_09_BFA_clip,
                    "precip_2017-10" = precip_2017_10_BFA_clip,
                    "precip_2017-11" = precip_2017_11_BFA_clip,
                    "precip_2017-12" = precip_2017_12_BFA_clip,
                    "precip_2017-13" = precip_2017_13_BFA_clip,
                    "precip_2017-14" = precip_2017_14_BFA_clip,
                    "precip_2017-15" = precip_2017_15_BFA_clip,
                    "air.temp_2014-06" = air.temp_2014_06_BFA_clip,
                    "air.temp_2014-07" = air.temp_2014_07_BFA_clip,
                    "air.temp_2014-08" = air.temp_2014_08_BFA_clip,
                    "air.temp_2014-09" = air.temp_2014_09_BFA_clip,
                    "air.temp_2014-10" = air.temp_2014_10_BFA_clip,
                    "air.temp_2014-11" = air.temp_2014_11_BFA_clip,
                    "air.temp_2014-12" = air.temp_2014_12_BFA_clip,
                    "air.temp_2017-08" = air.temp_2017_08_BFA_clip,
                    "air.temp_2017-09" = air.temp_2017_09_BFA_clip,
                    "air.temp_2017-10" = air.temp_2017_10_BFA_clip,
                    "air.temp_2017-11" = air.temp_2017_11_BFA_clip,
                    "air.temp_2017-12" = air.temp_2017_12_BFA_clip,
                    "air.temp_2017-13" = air.temp_2017_13_BFA_clip,
                    "air.temp_2017-14" = air.temp_2017_14_BFA_clip,
                    "air.temp_2017-15" = air.temp_2017_15_BFA_clip,
                    "NDVI_2014-07" = BFA_NDVI_182,
                    "NDVI_2014-08" = BFA_NDVI_213,
                    "NDVI_2014-09" = BFA_NDVI_244,
                    "NDVI_2014-10" = BFA_NDVI_274,
                    "NDVI_2014-11" = BFA_NDVI_305,
                    "NDVI_2014-12" = BFA_NDVI_335,
                    public_tt = BFA_public_tt,
                    pop_2014 = pop_2014,
                    pop_2017 = pop_2017,
                    build_up = BFA_build_up)


#####################################################################################################


## Writing stacked IM file


## Added air.temp and pop (WorldPop)
save(cov_IM_list, file = "~/OneDrive/Desktop/SAE project/scaled_raster_layers/cov_IM_list.RData")


#####################################################################################################

# 
# 
# cov_IM_list[["public_tt"]]$v <- log(1 + cov_IM_list[["public_tt"]]$v)
# 
# cov_IM_list[["precip_2014_08"]]$v <- log(1 + cov_IM_list[["precip_2014_08"]]$v)
# cov_IM_list[["precip_2014_09"]]$v <- log(1 + cov_IM_list[["precip_2014_09"]]$v)
# cov_IM_list[["precip_2014_10"]]$v <- log(1 + cov_IM_list[["precip_2014_10"]]$v)
# cov_IM_list[["precip_2014_11"]]$v <- log(1 + cov_IM_list[["precip_2014_11"]]$v)
# 
# 
# cov_IM_list[["air.temp_2014_08"]]$v <- log(1 + cov_IM_list[["air.temp_2014_08"]]$v)
# cov_IM_list[["air.temp_2014_09"]]$v <- log(1 + cov_IM_list[["air.temp_2014_09"]]$v)
# cov_IM_list[["air.temp_2014_10"]]$v <- log(1 + cov_IM_list[["air.temp_2014_10"]]$v)
# cov_IM_list[["air.temp_2014_11"]]$v <- log(1 + cov_IM_list[["air.temp_2014_11"]]$v)
# 
# 
# get_norm <- function(X)
# {
#     X$v <- (X$v - mean(X$v, na.rm = T)) / sd(X$v, na.rm = T)
#     
#     return(X)
# }
# 
# 
# cov_IM_list_norm <- lapply(cov_IM_list, get_norm)
# 
# 
# #####################################################################################################
# 
# 
# ## Writing stacked IM file with normalized coefficients
# 
# ## with air.temp and pop (WorldPop)
# save(cov_IM_list_norm, file = "~/OneDrive/Desktop/SAE project/scaled_raster_layers/cov_IM_list_norm.RData")
# 
# 
# 



