####################################################
##  Correcting outliers and spikes in conf cases  ##
####################################################
#
# Description:
#   Correcting for outliers and spikes which will cause issues
#   with trends analysis downstream from this
#
#  Sebastian Rodriguez (sebastian@rodriguez.cr)
#  Last edited Dec 11, 2021
#


rm(list = ls(all = TRUE))


library("plyr")
library("dplyr")


####################################################################

# Definite reading and writing directory

read_dir <- "~/Box/NU-malaria-team/projects/smc_impact/data/outputs/BFA_Routine_case_data_HF_aggregate_Seb_sum_columns.csv"

# write_dir <- "~/Box/NU-malaria-team/projects/smc_impact/data/outputs/BFA_Routine_case_data_HF_aggregate_Seb_sum_columns_cleaned_outliers.csv"


####################################################################


## Pre-processing the names to make them easier to work with
# Creating UID (unique ID) to have most unique HFs

HF_cases <- read.csv(read_dir, header = TRUE, strip.white = TRUE, stringsAsFactors = FALSE)
names(HF_cases)[c(4,5)] <- c("Commune", "HF.name")
HF_cases$UID <- paste(HF_cases$District, HF_cases$Commune, HF_cases$HF.name)
HF_cases$UID <- tolower(HF_cases$UID)
HF_cases[1:5] <- apply(HF_cases[,1:5], 2, tolower)



####################################################################


# Fixing columns with outliers by changing them to NA.
# The dates for fixing were determined by viewing individual HFs
# time-series for ones which seemed extremely suspicious
#
# Only doing for columns which are very problematic
# & which I use in later trends analysis
#
# I suggest checking these UID (unique ID) names with your dataset to make sure they match

HF_cases[which(HF_cases$UID == "zorgho zorgho csps tuire" & HF_cases$Date == "2018-09-01"), "allout_u5"] <- NA
HF_cases[which(HF_cases$UID == "nanoro kindi csps zerkoum" & HF_cases$Date == "2016-09-01"), "allout_u5"] <- NA
HF_cases[which(HF_cases$UID == "tenkodogo tenkodogo csps urbain ii" & HF_cases$Date == "2018-11-01"), "allout_u5"] <- NA
HF_cases[which(HF_cases$UID == "toma toma cma toma" & HF_cases$Date == "2018-08-01"), "allout_u5"] <- NA
HF_cases[which(HF_cases$UID == "sapone ipelce csps ipelce" & HF_cases$Date == "2016-09-01"), "allout_u5"] <- NA


HF_cases[which(HF_cases$UID == "kampti kampti csps latara" & HF_cases$Date == "2017-03-01"), "conf_rdt_ov5"] <- NA
HF_cases[which(HF_cases$UID == "nouna djibasso csps ba" & HF_cases$Date == "2016-05-01"), "conf_rdt_ov5"] <- NA



####################################################################


# To correct huge spikes in conf malaria cases which lead to conf_rdt_mic_u5 > allout_u5
# I look at which districts have this issue if I didnt do this correction
# and see which HFs in those districts show this behavior.
# I then make conf_rdt_u5 and conf_mic_u5 NA and rely on the imputation procedure
# downstream from this to impute these NAs
#
# I suggest checking these District names with your dataset to make sure they match


## Making NA Oct, Nov, and Dec 2016 for districts in which the prop of conf/allout > 1 for those months

Oct_districts <- c("ouargaye", "boussouma", "leo", "reo", "lena", "toma", "tougan", "garango",
                   "pouytenga", "nanoro", "sabou", "gourcy", "dano", "diebougou", "dedougou", "gorom",
                   "nouna", "sindou", "bitou", "kongoussi", "tougouri", "manga", "kombissiri", "dori",
                   "po", "sapone", "diapaga", "fada", "seguenega", "thiou", "yako", "ziniare", "zorgho")
Nov_districts <- c("ouargaye", "sapouy", "banfora", "mangodara", "koupela", "tenkodogo", "kongoussi", "tougouri",
                   "leo", "nanoro", "reo", "manga", "kombissiri", "po", "bogande", "fada", "ziniare", "sindou")
Sep_districts <- c("fada", "gorom")
Dec_districts <- c("solenzo", "tougan", "banfora", "reo", "tenado", "pama", "bousse", "ziniare", "kampti")
Jul_districts <- "po"


HF_cases[which(HF_cases$District %in% Oct_districts & HF_cases$Date == "2016-10-01" &
                   (HF_cases$conf_rdt_mic_u5 / HF_cases$allout_u5) > 1), c("conf_rdt_u5", "conf_mic_u5")] <- NA

HF_cases[which(HF_cases$District %in% Nov_districts & HF_cases$Date == "2016-11-01" &
                   (HF_cases$conf_rdt_mic_u5 / HF_cases$allout_u5) > 1), c("conf_rdt_u5", "conf_mic_u5")] <- NA

HF_cases[which(HF_cases$District %in% Sep_districts & HF_cases$Date == "2016-09-01" &
                   (HF_cases$conf_rdt_mic_u5 / HF_cases$allout_u5) > 1), c("conf_rdt_u5", "conf_mic_u5")] <- NA

HF_cases[which(HF_cases$District %in% Dec_districts & HF_cases$Date == "2016-12-01" &
                   (HF_cases$conf_rdt_mic_u5 / HF_cases$allout_u5) > 1), c("conf_rdt_u5", "conf_mic_u5")] <- NA

HF_cases[which(HF_cases$District %in% Jul_districts & HF_cases$Date == "2016-07-01" &
                   (HF_cases$conf_rdt_mic_u5 / HF_cases$allout_u5) > 1), c("conf_rdt_u5", "conf_mic_u5")] <- NA



####################################################################


# saving
write.csv(HF_cases, write_dir, row.names = FALSE)




