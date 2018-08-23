library(ggplot2)
library(reshape)
library(gridExtra)

# library(plyr) 
# save_png <- FALSE

# Check your working directory
wd <- setwd("C:/Users/IOtte/Documents/Desktop/training/")

## Read data
iso <- read.csv2("iso_calc_copy.csv", header = T)

ta.200 <- read.csv("C:/Users/IOtte/Documents/Desktop/plot_air_temperatur/iso_ta200_monthly.csv", header = TRUE)

## Sort temperature data 
ta.200 <- melt(ta.200)
colnames(ta.200) <- c("plotID", "date", "ta.200")
ta.200$year <- substr(ta.200$date, 26,29)
ta.200$mon <- substr(ta.200$date, 31,32)
ta.200 <- ta.200[, -2]
ta.200$date <- paste(ta.200$year, ta.200$mon, sep = "-")
ta.200 <- ta.200[, -3]
ta.200 <- ta.200[, -3]


## Aggregate plot data to monthly mean values

iso.mns <- aggregate(cbind(iso$d18_16, iso$dD_H, iso$d.excess, iso$amount_mm), 
                     by = list(substr(iso$date_sample, 1, 7), 
                               iso[, 4], iso[, 5], iso[, 6]),
                     FUN = "mean", na.rm = TRUE)

colnames(iso.mns) <- c("date", "plotID", "type", "elevation","d18_16", "dD_H", "dexcess", "amount")


## Build forest mean sub and merge later on
iso.for <- subset(iso.mns, iso.mns$plotID %in% c("flm1", "foc6", "foc0", "fpo0"))

iso.for.mean <- aggregate(iso.for$plotID, 
                          by = list(iso.for$date, iso.for$type, iso.for$d18_16, 
                                    iso.for$dD_H, iso.for$dexcess, iso.for$amount),
                          FUN = "mean")
colnames(iso.for.mean) <- c("date", "type", "d18_16", "dD_H", "dexcess", "amount", "plotID")
write.csv2(iso.for.mean, file = "iso_for_mean.csv")


iso.res <- subset(iso.mns, iso.mns$plotID %in% c("sav5", "hom4", "fpd0", "nkw1", "fer0"))
write.csv2(iso.res, file = "iso_res.csv")

iso.rest <- read.csv2("iso_res_wrk.csv")


## gleiches f??r temperatur
ta200.for <- subset(ta.200, ta.200[, 1] %in% c("flm1", "foc6", "foc0", "fpo0"))

ta200.for <- aggregate(ta.200$plotID,
                       by = list(ta.200$ta.200, ta.200$date),
                       FUN = "mean")
colnames(ta200.for) <- c("ta.200", "date", "plotID")
write.csv2(ta.200, file = "ta200_for.csv")

ta.200.res <- ta.200
write.csv2(ta.200.res, file = "ta200_res.csv")

ta200.for <- read.csv2("ta200_for.csv", header = TRUE)
ta200.for.agg <- aggregate(ta200.for$ta.200,
                           by = list(ta200.for$plotID, ta200.for$date),
                           FUN = "mean")
colnames(ta200.for.agg) <- c("plotID", "date", "ta200")
write.csv2(ta200.for.agg, file = "ta200_for_agg.csv")

ta200.nw <- read.csv2("ta200_res.csv", header = TRUE)



## subset fog, rain, tf --> d18o16, d-excess, (box)plot gegen temp

iso.ta200 <- merge(iso.rest, ta200.nw)



## calculate d-excess
#iso.ta200$dex <- 0
#iso.ta200 <- as.data.frame(iso.ta200, na.rm = TRUE)
#iso.ta200$dex <- lapply(iso.ta200$dex, function(i){
#  iso.ta200$dD_H(i) - 8*(iso.ta200$d18_16(i))
#})
  
  
## plotting
col.id.3 <- c("orange", "blue", "black", "darkgray", "green", "brown")

leg.ord <- c("fer0", "fpd0", "for0", "nkw1", "hom4", "sav5")

iso.mns.mnth.ta200.18O <- ggplot(iso.ta200, 
                                 aes(x = ta.200, y = d18_16, group = plotID, 
                                     colour = plotID)) + 
  facet_grid(type ~ ., scales = "free") +
  geom_boxplot() + 
  scale_color_manual(values = col.id.3, limits = leg.ord, name = "Plot ID SP1") + 
  ylab( expression(delta^{18}*O ~ "\u2030")) +
  xlab("air temperature [??C]") +
  #scale_x_continuous(breaks = iso.ta.200ld$ta.200) +
  theme(
    panel.grid.major = element_line(color = "lightgray", size = 0.01),
    panel.background = element_rect(fill = NA),
    panel.border = element_rect(color = "gray", fill = NA))

# --> fpo0, foc0,foc6&flm1 mitteln
#--> doubleyscale + dexcess(ordentlich formatieren!)

iso.mns.mnth.ta200.dex <- ggplot(iso.ta200, 
                                 aes(x = ta.200, y = dexcess, group = plotID, 
                                     colour = plotID)) + 
  facet_grid(. ~ type, scales = "free") +
  geom_boxplot() + 
  scale_color_manual(values = col.id.3, limits = leg.ord, name = "Plot ID SP1") + 
  ylab( expression(delta^{18}*O ~ "\u2030")) +
  xlab("air temperature [??C]") +
  #scale_x_continuous(breaks = iso.ta.200ld$ta.200) +
  theme(
    panel.grid.major = element_line(color = "lightgray", size = 0.01),
    panel.background = element_rect(fill = NA),
    panel.border = element_rect(color = "gray", fill = NA))



### try fancy stuff

iso.mns.mnth.d.18 <- ggplot(iso.ta200, 
                                 aes(x = date, y = d18_16, group = plotID, 
                                     colour = plotID)) + 
  facet_grid(type ~ ., scales = "free") +
  geom_line() + 
  scale_color_manual(values = col.id.3, limits = leg.ord, name = "Plot ID SP1") + 
  ylab( expression(delta^{18}*O ~ "\u2030")) +
  xlab("") +
  scale_x_discrete(labels = c("12-11", "12-12", "13-01", "13-02", "13-03", "13-04",
                              "13-05", "13-06", "13-07", "13-08", "13-09", "13-10", 
                              "13-11", "13-12", "14-01", "14-02", "14-03", "14-04",
                              "14-05", "14-06", "14-07", "14-08", "14-09", "14-10",
                              "14-11")) +
  theme(
    panel.grid.major = element_line(color = "lightgray", size = 0.01),
    panel.background = element_rect(fill = NA),
    panel.border = element_rect(color = "gray", fill = NA))

#dexcess
iso.mns.mnth.dex <- ggplot(iso.ta200, 
                            aes(x = date, y = dexcess, group = plotID, 
                                colour = plotID)) + 
  facet_grid(type ~ ., scales = "free") +
  geom_line() + 
  scale_color_manual(values = col.id.3, limits = leg.ord, name = "Plot ID SP1") + 
  ylab( "d.excess") +
  xlab("") +
  scale_x_discrete(labels = c("12-11", "12-12", "13-01", "13-02", "13-03", "13-04",
                              "13-05", "13-06", "13-07", "13-08", "13-09", "13-10", 
                              "13-11", "13-12", "14-01", "14-02", "14-03", "14-04",
                              "14-05", "14-06", "14-07", "14-08", "14-09", "14-10",
                              "14-11")) +
  theme(
    panel.grid.major = element_line(color = "lightgray", size = 0.01),
    panel.background = element_rect(fill = NA),
    panel.border = element_rect(color = "gray", fill = NA))



d18.dex <- grid.arrange(iso.mns.mnth.d.18,iso.mns.mnth.dex) 









# print "iso.mns.mnth.elvtn.18O"
png("out/iso.mns.mnth.elvtn.18O.png", width = 30, height = 20, units = "cm", 
    res = 300, pointsize = 15)
print(iso.mns.mnth.elvtn.18O)
dev.off()

  
  
  
  