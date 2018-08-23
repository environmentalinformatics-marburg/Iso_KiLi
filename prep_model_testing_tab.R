library(ggplot2)
library(RColorBrewer)
library(plyr)

### setwd()
wd <- setwd("C:/Users/iotte/Desktop/III_isoIntensive")

iso <- read.csv2("iso_test_traject.csv", sep = ";")

dex.humid <- subset(iso, iso$d.excess <= 10)
dex.humid.pos <- subset(dex.humid, dex.humid$d.excess >= 0)

dex.humid.pos.O18 <- aggregate(dex.humid.pos$d18_16,
                               by = list(dex.humid.pos$plot_id_sp1), 
                               na.rm = TRUE,
                               FUN = "mean")

colnames(dex.humid.pos.O18) <- c("plot_id_sp1", "O18")


dex.humid.pos.H2 <- aggregate(dex.humid.pos$dD_H,
                               by = list(dex.humid.pos$plot_id_sp1), 
                               na.rm = TRUE,
                               FUN = "mean")

colnames(dex.humid.pos.H2) <- c("plot_id_sp1", "H2")


dex.humid.pos.dex <- aggregate(dex.humid.pos$d.excess,
                               by = list(dex.humid.pos$plot_id_sp1), 
                               na.rm = TRUE,
                               FUN = "mean")

colnames(dex.humid.pos.dex) <- c("plot_id_sp1", "d.excess")

dex.humid.iso <- cbind(dex.humid.pos.O18, dex.humid.pos.H2, dex.humid.pos.dex)
dex.humid.iso <- dex.humid.iso[, -(3)]
dex.humid.iso <- dex.humid.iso[, -(4)]



################
################
###### iso long term
###############
iso <- read.csv2("iso_calc.csv", sep = ";")

dex.humid <- subset(iso, iso$d.excess <= 10)
dex.humid.pos <- subset(dex.humid, dex.humid$d.excess >= 0)

dex.humid.pos.O18 <- aggregate(dex.humid.pos$d18_16,
                               by = list(dex.humid.pos$plot_id_sp1, dex.humid.pos$type), 
                               na.rm = TRUE,
                               FUN = "mean")

colnames(dex.humid.pos.O18) <- c("plot_id_sp1", "type", "O18")


dex.humid.pos.H2 <- aggregate(dex.humid.pos$dD_H,
                              by = list(dex.humid.pos$plot_id_sp1, dex.humid.pos$type), 
                              na.rm = TRUE,
                              FUN = "mean")

colnames(dex.humid.pos.H2) <- c("plot_id_sp1", "type", "H2")


dex.humid.pos.dex <- aggregate(dex.humid.pos$d.excess,
                               by = list(dex.humid.pos$plot_id_sp1, dex.humid.pos$type), 
                               na.rm = TRUE,
                               FUN = "mean")

colnames(dex.humid.pos.dex) <- c("plot_id_sp1", "type", "d.excess")

dex.humid.iso.lgt.type <- cbind(dex.humid.pos.O18, dex.humid.pos.H2, dex.humid.pos.dex)
dex.humid.iso.lgt.type <- dex.humid.iso.lgt.type[, -(4:5)]
dex.humid.iso.lgt.type <- dex.humid.iso.lgt.type[, -(5:6)]


#### now do it

shit <- read.csv2("iso_test_traject_hmd_KiLiType.csv", sep = ";")
shit$mnth <- substr(shit$date_sample, 6, 7)

shit.dec <- subset(shit, shit$mnth == "12")
shit.apr <- subset(shit, shit$mnth == "04")

temp <- aggregate(shit.apr$FcKili,
                  by = list(shit.apr$plot_id_sp1, shit.apr$type), 
                  na.rm = TRUE,
                  FUN = "mean")


