library(ggplot2)
library(RColorBrewer)
library(plyr)

### setwd()
wd <- setwd("C:/Users/iotte/Desktop/III_isoIntensive")


### read data
iso <- read.csv("iso_test_traject.csv", sep = ";")

met13 <- read.csv("isoIntensiveDec13.csv", sep = ";")
met14 <- read.csv("isoIntensiveApr14.csv", sep = ";")


### aggregate metdate to isodate
## met13
met13.Ta <- aggregate(met13$Ta_200,
                      by = list(met13$date_sample, met13$time, 
                                met13$plotID, met13$type),
                      na.rm = TRUE,
                      FUN = "mean")


met13.rH <- aggregate(met13$rH_200,
                      by = list(met13$date_sample, met13$time, 
                                met13$plotID, met13$type),
                      na.rm = TRUE,
                      FUN = "mean")

met13.agg <- cbind(met13.Ta, met13.rH)
met13.agg <- met13.agg[, -(6:9)]
colnames(met13.agg) <- c("date_sample", "time", "plot_id_sp1", 
                         "type", "Ta_200", "rH_200")

met13.agg$date_sample <- as.Date(met13.agg$date_sample)
met13.agg.sort <- met13.agg[order(met13.agg$plot_id_sp1, met13.agg$type, 
                                met13.agg$date_sample, met13.agg$time), ]
write.csv2(met13.agg.sort, file = "met13_agg_sort.csv", sep = ";")


## met14
met14.Ta <- aggregate(met14$Ta_200,
                      by = list(met14$date_sample, met14$time, 
                                met14$plotID, met14$type),
                      na.rm = TRUE,
                      FUN = "mean")


met14.rH <- aggregate(met14$rH_200,
                      by = list(met14$date_sample, met14$time, 
                                met14$plotID, met14$type),
                      na.rm = TRUE,
                      FUN = "mean")

met14.agg <- cbind(met14.Ta, met14.rH)
met14.agg <- met14.agg[, -(6:9)]
colnames(met14.agg) <- c("date_sample", "time", "plot_id_sp1", 
                         "type", "Ta_200", "rH_200")

met14.agg$date_sample <- as.Date(met14.agg$date_sample)
met14.agg.sort <- met14.agg[order(met14.agg$plot_id_sp1, met14.agg$type, 
                                  met14.agg$date_sample, met14.agg$time), ]
write.csv2(met14.agg.sort, file = "met14_agg_sort.csv", sep = ";")


