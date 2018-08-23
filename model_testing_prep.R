library(ggplot2)
library(RColorBrewer)
library(plyr)

### setwd()
wd <- setwd("C:/Users/iotte/Desktop/III_isoIntensive")


### read data
iso <- read.csv("DAR ES SALAAM.csv")

iso$mnth <- substr(iso$Date, 6, 7)

iso.mnth <- aggregate(iso$O18,
                      by = list(iso$Date, iso$mnth), 
                      na.rm = TRUE,
                      FUN = "mean")

colnames(iso.mnth) <- c("Date", "mnth", "O18")


iso.mnth2 <- aggregate(iso$H2,
                      by = list(iso$Date, iso$mnth), 
                      na.rm = TRUE,
                      FUN = "mean")

colnames(iso.mnth2) <- c("Date", "mnth", "H2")

iso.mnth <- cbind(iso.mnth, iso.mnth2)
iso.mnth <- iso.mnth[, -(4:5)] 
iso.mnth$d.excess <- iso.mnth$H2 - 8*iso.mnth$O18

iso.lngrn <- subset(iso.mnth, iso.mnth$mnth == "03" |
                      iso.mnth$mnth == "04" |
                      iso.mnth$mnth == "05")

iso.lngrn.hmd <- subset(iso.lngrn, iso.lngrn$d.excess <= 10)
Dis.lngrn <- mean(iso.lngrn.hmd$d.excess)
O18.lngrn <- mean(iso.lngrn.hmd$O18)
H2.lngrn <- mean(iso.lngrn.hmd$H2)


iso.hmd <- subset(iso.mnth, iso.mnth$d.excess <= 10)
Dis.hmd <- mean(iso.hmd$d.excess)
O18.hmd <- mean(iso.hmd$O18) 
H2.hmd <- mean(iso.hmd$H2) 
