#####################################################
##  Active/Inactive health facility determination  ##
#####################################################
#
# Description:
#
#   Making health facilities active/inactive
#
#
#  Sebastian Rodriguez (sebastian@rodriguez.cr)
#  Last edited Mar 09, 2021
#



rm(list = ls(all = TRUE))

require("plyr")
require("dplyr")
require("zoo")

library("imputeTS")
library("maditr")




####################################################################

# Definite reading and writing directory

read_dir <- "~/Box/NU-malaria-team/projects/smc_impact/data/outputs/U5_HF_cases_smc_coords_imputed_rdts_and_allout_MA.csv"

write_dir <- "~/Box/NU-malaria-team/projects/smc_impact/data/outputs/U5_HF_cases_smc_coords_imputed_rdts_and_allout_MA_activeHFs.csv"


####################################################################


# Loading health facility dataset

HF_cases <- read.csv(read_dir)
HF_cases$Date <- as.Date(as.yearmon(HF_cases$Date))
HF_cases <- HF_cases[order(HF_cases$UID, HF_cases$Date),]



####################################################################


rle.try_allout <- ddply(HF_cases, .(UID), summarize,
                        is_NA = rle(is.na(allout_u5))[2],
                        consec = rle(is.na(allout_u5))[1])


####################################################################


consec_NA_list_allout_u5 <- split(rle.try_allout[,2:3], rle.try_allout$UID)
consec_NA_list_allout_u5_reshaped <- lapply(consec_NA_list_allout_u5, function(x) {
    x <- do.call(cbind.data.frame, x);
    names(x) <- c("is.NA", "consec vals");
    return(x) })


HF_active_list_allout <- lapply(consec_NA_list_allout_u5_reshaped, function(x) {
    
    HF_status <- rep("active", 48);
    
    if(x[1, "is.NA"])
    {
        HF_status[1:x[1, "consec vals"]] <- "inactive";
    }
    
    if(x[nrow(x), "is.NA"] & (x[nrow(x), "consec vals"] >= 6))
    {
        HF_status[(48 - x[nrow(x), "consec vals"] + 1):48] <- "inactive";
    }
    
    return(HF_status)
})



####################################################################

rle.try_conf_u5 <- ddply(HF_cases, .(UID), summarize,
                         is_NA = rle(is.na(conf_rdt_u5))[2],
                         consec_NAs = rle(is.na(conf_rdt_u5))[1])

####################################################################


consec_NA_list_conf_u5 <- split(rle.try_conf_u5[,2:3], rle.try_conf_u5$UID)
consec_NA_list_conf_u5_reshaped <- lapply(consec_NA_list_conf_u5, function(x) {
    x <- do.call(cbind.data.frame, x);
    names(x) <- c("is.NA", "consec vals");
    return(x) })


HF_active_list_conf <- lapply(consec_NA_list_conf_u5_reshaped, function(x) {
    
    HF_status <- rep("active", 48);
    
    if(x[1, "is.NA"])
    {
        HF_status[1:x[1, "consec vals"]] <- "inactive";
    }
    
    if(x[nrow(x), "is.NA"] & (x[nrow(x), "consec vals"] >= 6))
    {
        HF_status[(48 - x[nrow(x), "consec vals"] + 1):48] <- "inactive";
    }
    
    return(HF_status)
})



#################################################################################################


HF_active_list <- lapply(names(consec_NA_list_conf_u5_reshaped), function(N) {
    
    x <- consec_NA_list_conf_u5_reshaped[[N]];
    y <- consec_NA_list_allout_u5_reshaped[[N]];
    
    HF_status <- rep("active", 48);
    
    if(x[1, "is.NA"] & y[1, "is.NA"])
    {
        min_consec <- min(x[1, "consec vals"], y[1, "consec vals"]);
        
        HF_status[1:min_consec] <- rep("inactive", min_consec);
    }
    
    if( (x[nrow(x), "is.NA"] & y[nrow(y), "is.NA"]) & (x[nrow(x), "consec vals"] >= 6 & y[nrow(y), "consec vals"] >= 6) )
    {
        min_consec <- min(x[nrow(x), "consec vals"], y[nrow(y), "consec vals"]);
        
        HF_status[(48 - min_consec + 1):48] <- "inactive";
    }
    
    
    HF_status_DF <- data.frame("UID" = rep(N, 48),
                               "Date" = unique(HF_cases$Date),
                               HF_status = HF_status);
    
    return(HF_status_DF)
})

HF_active <- do.call(rbind.data.frame, HF_active_list)


#################################################################################################

HF_cases_active <- left_join(HF_cases, HF_active, by = c("UID", "Date"))



write.csv(HF_cases_active, write_dir, row.names = FALSE)













#################################################################################################


#### Checking which HFs became active / became inactive

rle.try_active <- ddply(HF_cases_active, .(UID), summarize,
                        is_inactive = rle(HF_status == "inactive")[2],
                        consec_inactive = rle(HF_status == "inactive")[1])


flipping_HFs <- rle.try_active[which(sapply(rle.try_active$is_inactive, function(x) (FALSE %in% x & TRUE %in% x))),]

# inactive to active
flipping_HFs[which(sapply(flipping_HFs $is_inactive, function(x) (x[1]) ) ),] %>% View


# active to inactive
flipping_HFs[which(sapply(flipping_HFs $is_inactive, function(x) (x[length(x)]) ) ),] %>% View
