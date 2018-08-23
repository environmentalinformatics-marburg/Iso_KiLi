library(ggplot2)
library(RColorBrewer)
library(plyr)

### setwd()
wd <- setwd("C:/Users/iotte/Desktop/III_isoIntensive")

# Preprocessing - do not run again
### read data
#iso <- read.csv2("fer0_rH_downscaled.csv", sep = ";")

#iso.rH <- aggregate(iso$rh_dwsc,
#                      by = list(iso$date_sample, iso$time, 
#                                iso$plot_id_sp1, iso$type),
#                      na.rm = TRUE,
#                      FUN = "mean")

#colnames(iso.rH) <- c("date_sample", "time", "plot_id_sp1", "type", "rH_dwsc")

#iso.rH$date_sample <- as.Date(iso.rH$date_sample)
#iso.rH.sort <- iso.rH[order(iso.rH$plot_id_sp1, iso.rH$type, 
#                                  iso.rH$date_sample, iso.rH$time), ]
#write.csv2(iso.rH.sort, file = "iso_rH_sort.csv", sep = ";")

###
###
### implement model

#load data
iso <- read.csv2("iso_test_traject.csv", sep = ";")
iso.lt <- read.csv2("iso_calc.csv", sep = ";", header = TRUE)

# manipulate data
# test run sav5 rain
#iso.ltm.18 <- aggregate(iso.lt$d18_16,
#                        by = list(iso.lt$plot_id_sp1, iso.lt$type),
#                        na.rm = TRUE,
#                        FUN = "mean")

plot <- subset(iso.lt, iso.lt$plot_id_sp1 == "hom4")
typ <- subset(plot, plot$type == "rain")
#typ <- plot
is.cmp <- subset(typ, typ$dD_H != "NA")

is.cmp.sm.o <- sum(is.cmp$dD_H.amount_mm)
is.cmp.sm <- sum(is.cmp$amount_mm)
# vgmm
vgmm <- is.cmp.sm.o/is.cmp.sm

##################
##################

iso.rh90 <- subset(iso, iso$rH_200 >= 90)



summary(lm(d.excess ~ rH_200, data = iso.rh90))


lm_labels <- function(iso.rh90) {
  mod <- lm(rH_200 ~ d.excess, data = iso.rh90)
  formula <- sprintf("italic(y) == %.2f %+.4f * italic(x)",
                     round(coef(mod)[1], 4), round(coef(mod)[2], digits = 4))
  
  r <- cor(iso.rh90$rH_200, iso.rh90$d.excess, use = "pairwise.complete.obs")
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r2 = r2, stringsAsFactors = FALSE)
}

labels <- ddply(iso.rh90, c("plot_id_sp1", "type"), lm_labels)
labels





iso.ltm.2 <- aggregate(iso.lt$dD_H,
                        by = list(iso.lt$plot_id_sp1, iso.lt$type),
                        na.rm = TRUE,
                        FUN = "mean")

iso.ltm <- cbind(iso.ltm.18, iso.ltm.2)
iso.ltm <- iso.ltm[, -(4:5)]
colnames(iso.ltm) <- c("plot_id_sp1", "type", "d18_16", "dD_H")


sav5 <- subset(iso, iso$plot_id_sp1 == "sav5")
sav5.rn <- subset(sav5, sav5$type == "rain")

Rw <- subset(iso.ltm, iso.ltm$plot_id_sp1 == "sav5" &
             iso.ltm$type == "rain")

Rw18 <- -1.363053
Rw18w <- -2.66
Rw2 <- -1.055579




