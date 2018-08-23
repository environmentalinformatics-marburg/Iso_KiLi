library(ggplot2)


# set working directory
wd <- setwd("C:/Users/IOtte/Desktop/training/")



### load data
iso <- read.csv2("iso_calc_copy.csv", header = T)

## subset to remove mnp1+2
iso.tr <- subset(iso, iso$plot_id_sp1 != "mnp1" &
                   iso$plot_id_sp1 != "mnp2")



### Plotting layout stuff
col.id <- c("orange", "blue", "purple", "black", "yellow", "red", "white", 
            "green", "brown")

leg <- c("fer0", "fpd0", "fpo0", "foc0", "foc6", "flm1", "nkw1", 
         "hom4", "sav5")


## calc linear model for d18_16 based on rwa data
iso.sm <- summary(lm(d18_16 ~ amount_mm, data = iso.tr))


fg <- subset(iso, iso$type == "fog")
iso.sm.rev.fg <- summary(lm(amount_mm ~ d18_16, data = fg))

tf <- subset(iso, iso$type == "tf")
iso.sm.rev.fg <- summary(lm(amount_mm ~ d18_16, data = tf))

rain <- subset(iso, iso$type == "rain")
iso.sm.rev.fg <- summary(lm(amount_mm ~ d18_16, data = rain))


## plot d18_16 based on raw data 

iso.amnt.d1816 <- qplot(d18_16, amount_mm, data = iso.tr, color = plot_id_sp1, 
                           shape = type, 
                           xlab = "amount_mm", 
                           ylab = expression(delta^{18}*O ~ "\u2030")) + 
  facet_wrap( ~ type) +
  scale_color_manual(values = col.id, limits = leg, name = "Plot ID SP1") + 
  geom_abline(intercept = 10.63, slope = -6.99) 

iso.amnt.d1816.all <- qplot(amount_mm, d18_16, data = iso.tr, color = plot_id_sp1, 
                            shape = type, 
                            xlab = "amount_mm", 
                            ylab = expression(delta^{18}*O ~ "\u2030")) + 
  facet_wrap( ~ plot_id_sp1) +
  scale_color_manual(values = col.id, limits = leg, name = "Plot ID SP1") 


###

## calc linear model for dD_H based on raw data 
iso.sm <- summary(lm(dD_H ~ amount_mm, data = iso.tr))
iso.sm.rev <- summary(lm(amount_mm ~ dD_H, data = iso.tr))


fg <- subset(iso.tr, iso.tr$type == "fog")
iso.sm.rev.fg <- summary(lm(amount_mm ~ dD_H, data = fg))

tf <- subset(iso.tr, iso.tr$type == "tf")
iso.sm.rev.tf <- summary(lm(amount_mm ~ dD_H, data = tf))

rain <- subset(iso.tr, iso.tr$type == "rain")
iso.sm.rev.rn <- summary(lm(amount_mm ~ dD_H, data = rain))


## plot dD_H based on raw data 
iso.amnt.d2h <- qplot(dD_H, amount_mm, data = iso, color = plot_id_sp1, 
                      shape = type, 
                      xlab = "amount_mm", 
                      ylab = expression(delta^{2}*H ~ "\u2030")) + 
  facet_wrap( ~ type) +
  scale_color_manual(values = col.id, limits = leg, name = "Plot ID SP1") + 
  geom_abline(intercept = 10.63, slope = -6.99) 


iso.amnt.d2h.all <- qplot(amount_mm, dD_H, data = iso, color = plot_id_sp1, 
                          shape = type, 
                          xlab = "amount_mm", 
                          ylab = expression(delta^{2}*H ~ "\u2030")) + 
  facet_wrap( ~ plot_id_sp1) +
  scale_color_manual(values = col.id, limits = leg, name = "Plot ID SP1") 





### recalc on monthly values
## Aggregate plot data to monthly mean values

iso.mns <- aggregate(cbind(iso.tr$d18_16, iso.tr$dD_H, iso.tr$d.excess, 
                           iso.tr$amount_mm), 
                     by = list(substr(iso.tr$date_sample, 1, 7), 
                               iso.tr[, 4], iso.tr[, 5], iso.tr[, 6]),
                     FUN = "mean", na.rm = TRUE)

colnames(iso.mns) <- c("date", "plotID", "type", "elevation","d18_16", "dD_H", 
                       "dexcess", "amount")



## calc linear model for d18_16 monthly means
iso.sm <- summary(lm(d18_16 ~ amount, data = iso.mns))


fg <- subset(iso.mns, iso.mns$type == "fog")
iso.fg <- summary(lm(d18_16 ~ amount, data = fg))

tf <- subset(iso.mns, iso.mns$type == "tf")
iso.tf <- summary(lm(d18_16 ~ amount, data = tf))

rain <- subset(iso.mns, iso.mns$type == "rain")
iso.rn <- summary(lm(d18_16 ~ amount, data = rain))


## Plot d18_16 monthly means

iso.mns.d1816 <- qplot(amount, d18_16, data = iso.mns, color = plotID, 
                       shape = type, 
                       xlab = "amount [mm]", 
                       ylab = expression(delta^{18}*O ~ "\u2030")) + 
  facet_wrap( ~ type) +
  scale_color_manual(values = col.id, limits = leg, name = "Plot ID SP1") 



###
## calc linear model for dD_H monthy means
iso.sm <- summary(lm(dD_H ~ amount, data = iso.mns))


fg <- subset(iso.mns, iso.mns$type == "fog")
iso.fg <- summary(lm(dD_H ~ amount, data = fg))

tf <- subset(iso.mns, iso.mns$type == "tf")
iso.tf <- summary(lm(dD_H ~ amount, data = tf))

rain <- subset(iso.mns, iso.mns$type == "rain")
iso.rn <- summary(lm(dD_H ~ amount, data = rain))


## Plot dD_H monthly means
iso.amnt.d2h <- qplot(amount, dD_H,data = iso.mns, color = plotID, shape = type, 
                      xlab = "amount [mm]", 
                      ylab = expression(delta^{2}*H ~ "\u2030")) + 
  facet_wrap( ~ type) +
  scale_color_manual(values = col.id, limits = leg, name = "Plot ID SP1") 
