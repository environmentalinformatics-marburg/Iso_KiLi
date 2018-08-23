library(ggplot2)
library(RColorBrewer)
library(plyr)

### setwd()
wd <- setwd("C:/Users/iotte/Desktop/modelling/")

iso <- read.csv2("source/iso_intensive_R.csv", sep = ";")
iso.lgtm <- read.csv2("source/iso_calc.csv", sep = ";")

# subsetting according to precipitation type
iso.rn <- subset(iso, iso$type == "rain")
iso.lgtm.rn <- subset(iso.lgtm, iso.lgtm$type =="rain")
iso.rn.foc <- subset(iso.rn, iso.rn$plot_id_sp1 == "foc0")
iso.lgtm.rn.foc <- subset(iso.lgtm.rn, iso.lgtm.rn$plot_id_sp1 == "foc0")

# build station longterm mean
# still preparation
iso.lgtm.rn.foc <- subset(iso.lgtm.rn, iso.lgtm.rn$plot_id_sp1 =="foc0")
iso.lgtm.mn.foc.O <- mean(iso.lgtm.rn.foc$d18_16, na.rm = TRUE)
iso.lgtm.mn.foc.H <- mean(iso.lgtm.rn.foc$dD_H, na.rm = TRUE)

# separate according to campaigns i.e. Dec13/Apr14!
iso.rn.lgtm.sav <- subset(iso.lgtm.rn, iso.lgtm.rn$plot_id_sp1 == "sav5")
iso.rn.sav.vl <- mean(iso.rn.lgtm.sav$d.excess, na.rm = TRUE)


####
####
# calculate deltaVAP for oxygen and hydrogen 
# calculate deuterium excessVAP 
iso.rn.foc$Ovap <- (iso.lgtm.mn.foc.O / iso.lgtm.mn.foc$X18a - 
                      iso.lgtm.mn.foc$rH * iso.lgtm.mn.foc$d18_16) / 
  ((1 - iso.lgtm.mn.foc$rH) * iso.lgtm.mn.foc$X18ak)
iso.rn.foc$Hvap <- (iso.lgtm.mn.foc.H / iso.lgtm.mn.foc$X2a - 
                      iso.lgtm.mn.foc$rH * iso.lgtm.mn.foc$dD_H) / 
  ((1 - iso.lgtm.mn.foc$rH) * iso.lgtm.mn.foc$X2ak)
iso.rn.foc$Dvap <- iso.lgtm.mn.foc$Hvap - 8 * iso.lgtm.mn.foc$Ovap



####
####
# calculate the moisture-recycling fraction
# was kommt in spalte tst? was ist gesucht? verdunstung relativ zu focanne?
# evtl time lagged?
# abbildung mit iso-signatur aller plots erstellen!
iso.rn.foc$tst <- (iso.rn.foc$d.excess - iso.rn.sav.vl) / (iso.rn.foc$Dvap - iso.rn.sav.vl)
