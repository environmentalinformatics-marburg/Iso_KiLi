library(ggplot2)
library(lubridate)
library(reshape)
library(gridExtra)
library(RColorBrewer)


col <- rev(brewer.pal(8, "Spectral"))
col.2 <- rev(brewer.pal(8, "YlGnBu"))


# set working directory
wd <- setwd("C:/Users/IOtte/Documents/Desktop/training/")


### load data

iso <- read.csv2("iso_calc.csv", header = T)
  
ta200 <- read.csv("C:/Users/IOtte/Documents/Desktop/plot_air_temperatur/iso_ta200_monthly.csv", header = TRUE)

#wd.sv <- read.csv2("C:/Users/IOtte/Desktop/unpack/wd/sav0_week.csv", header = TRUE)

wd.sv <- read.csv("C:/Users/IOtte/Desktop/sav0_wd.csv", header = TRUE)



# format wd data
#wd.sv.d <- wd.sv.d[, -2]
#wd.sv.d$datetime <- substr(wd.sv.d$datetime, 1, 10)
#wd.sv.d$datetime <- as.Date(wd.sv.d$datetime)

wd.sv$datetime <- substr(wd.sv$datetime, 1, 10)
wd.sv$datetime <- as.Date(wd.sv$datetime)

## Sort temperature data 
ta200 <- melt(ta200)
colnames(ta200) <- c("plotID", "date", "ta200")
ta200$year <- substr(ta200$date, 26,29)
ta200$mon <- substr(ta200$date, 31,32)
ta200 <- ta200[, -2]
ta200$date <- paste(ta200$year, ta200$mon, sep = "-")
ta200 <- ta200[, -3]
ta200 <- ta200[, -3]


## Aggregate iso plot data to monthly mean values
# build monthly mean values of d18-16, dD_H & dexcess

iso.mns <- aggregate(cbind(iso$d18_16, iso$dD_H, iso$d.excess), 
                          by = list(substr(iso$date_sample, 1, 7), 
                                    iso[, 4], iso[, 5], iso[, 6]),
                          FUN = "mean", na.rm = TRUE)

colnames(iso.mns) <- c("date", "plotID", "type", "elevation","d18_16", "dD_H", "dexcess")

# build monthly sums of amount_mm
amnt.smm <- aggregate(iso$amount_mm, 
                      by = list(substr(iso$date_sample, 1, 7), 
                                iso[, 4], iso[, 5], iso[, 6]), 
                      FUN = "sum", na.rm = TRUE)

colnames(amnt.smm) <- c("date", "plotID", "type", "elevation", "amount")


# merge monthly mean of d18-16 & dD_H and monthly sums of amount_mm
iso.mnth <- merge(iso.mns, amnt.smm)


## Merge iso.mns and ta200 to iso.ta200

iso.ta200 <- merge(iso.mnth, ta200)


## subsetting for better facility of instruction

rain <- subset(iso.ta200, iso.ta200$type == "rain")
tf <- subset(iso, iso$type == "tf")
fog <- subset(iso, iso$type == "fog")

sav5 <- subset(rain, rain$plotID == "sav5")
hom4 <- subset(rain, rain$plotID == "hom4")
nkw1 <- subset(rain, rain$plotID == "nkw1")
flm1 <- subset(rain, rain$plotID == "flm1")
foc6 <- subset(rain, rain$plotID == "foc6")
foc0 <- subset(rain, rain$plotID == "foc0")
fpo0 <- subset(rain, rain$plotID == "fpo0")
fpd0 <- subset(rain, rain$plotID == "fpd0")
fer0 <- subset(rain, rain$plotID == "fer0")

nkw1.cor <- cor(nkw1$d18_16, nkw1$amount_mm, method = "pearson", use = "complete.obs")

fer0.cor <- cor(fer0$d18_16, fer0$dexcess, method = "spearman", use = "complete.obs")
fpd0.cor <- cor(fpd0$d18_16, fpd0$dexcess, method = "spearman", use = "complete.obs")
fpo0.cor <- cor(fpo0$d18_16, fpo0$dexcess, method = "spearman", use = "complete.obs")
foc0.cor <- cor(foc0$d18_16, foc0$dexcess, method = "spearman", use = "complete.obs")
foc6.cor <- cor(foc6$d18_16, foc6$dexcess, method = "spearman", use = "complete.obs")
flm1.cor <- cor(flm1$d18_16, flm1$dexcess, method = "spearman", use = "complete.obs")
hom4.cor <- cor(hom4$d18_16, hom4$dexcess, method = "spearman", use = "complete.obs")
sav5.cor <- cor(sav5$d18_16, sav5$dexcess, method = "spearman", use = "complete.obs")

fer0.cor <- cor(fer0$d18_16, fer0$amount, method = "spearman", use = "complete.obs")
fpd0.cor <- cor(fpd0$d18_16, fpd0$amount, method = "spearman", use = "complete.obs")
fpo0.cor <- cor(fpo0$d18_16, fpo0$amount, method = "spearman", use = "complete.obs")
foc0.cor <- cor(foc0$d18_16, foc0$amount, method = "spearman", use = "complete.obs")
foc6.cor <- cor(foc6$d18_16, foc6$amount, method = "spearman", use = "complete.obs")
flm1.cor <- cor(flm1$d18_16, flm1$amount, method = "spearman", use = "complete.obs")
hom4.cor <- cor(hom4$d18_16, hom4$amount, method = "spearman", use = "complete.obs")
sav5.cor <- cor(sav5$d18_16, sav5$amount, method = "spearman", use = "complete.obs")

fer0.cor <- cor(fer0$d18_16, fer0$ta200, method = "spearman", use = "complete.obs")
fpd0.cor <- cor(fpd0$d18_16, fpd0$ta200, method = "spearman", use = "complete.obs")
fpo0.cor <- cor(fpo0$d18_16, fpo0$ta200, method = "spearman", use = "complete.obs")
foc0.cor <- cor(foc0$d18_16, foc0$ta200, method = "spearman", use = "complete.obs")
foc6.cor <- cor(foc6$d18_16, foc6$ta200, method = "spearman", use = "complete.obs")
flm1.cor <- cor(flm1$d18_16, flm1$ta200, method = "spearman", use = "complete.obs")
hom4.cor <- cor(hom4$d18_16, hom4$ta200, method = "spearman", use = "complete.obs")
sav5.cor <- cor(sav5$d18_16, sav5$ta200, method = "spearman", use = "complete.obs")






iso.all.lmwl.gmwl <- qplot(d18_16, dexcess, data = sav5) +
  scale_x_reverse()

iso.sm <- summary(lm(rev(dexcess ~ d18_16), data = sav5))


#type <- lapply(types, function(i){
#  sub <- subset(iso, iso$type == i)
#})


### build plot for presentation
### each plot seperately

col.id.rn <- c("#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#fee08b", "#fdae61", 
               "#f46d43", "#d53e4f")

leg.rn <- c("fer0", "fpd0", "fpo0", "foc0", "foc6", "flm1", "hom4", "sav5")


col.id.fg <- c("#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#fee08b", "#fdae61", 
                "#f46d43")

leg.fg <- c("fer0", "fpd0", "fpo0", "foc0", "foc6", "flm1", "nkw1")


## d18O

iso.d18 <- ggplot(subset(iso.ta200, iso.ta200[, 3] == "fog"), 
                  aes(x = date, y = d18_16, group = plotID, 
                      colour = plotID)) + 
  geom_line() + 
  scale_color_manual(values = col, limits = leg.fg, name = "Plot ID SP1") + 
  ylab( expression(fog  ~ delta^{18}*O ~ "\u2030")) +
  xlab("") +
  scale_x_discrete(labels = c("11", "12", "01", "02", "03", "04", "05", "06", 
                              "07", "08", "09", "10", "11", "12", "01", "02", 
                              "03", "04", "05", "06", "07", "08", "09", "10",
                              "11")) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "gray", fill = NA),
    legend.position = "none")


## dexcess

iso.dexcess <- ggplot(subset(iso.ta200, iso.ta200[, 3] == "fog"), 
                      aes(x = date, y = dexcess, group = plotID, 
                      colour = plotID)) + 
  geom_line() + 
  scale_color_manual(values = col, limits = leg.fg, name = "Plot ID SP1") + 
  ylab( expression(fog  ~ dexcess ~ "\u2030")) +
  xlab("") +
  scale_y_reverse() +
  scale_x_discrete(labels = c("11", "12", "01", "02", "03", "04", "05", "06", 
                              "07", "08", "09", "10", "11", "12", "01", "02", 
                              "03", "04", "05", "06", "07", "08", "09", "10",
                              "11")) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "gray", fill = NA),
    legend.position = "none")


## temperature

iso.ta.200 <- ggplot(subset(iso.ta200, iso.ta200[, 3] == "rain"), 
                    aes(x = date, y = ta200, group = plotID, 
                    colour = plotID)) + 
  geom_line() + 
  scale_color_manual(values = col, limits = leg.rn, name = "Plot ID SP1") + 
  ylab("ta200 [??C]") +
  xlab("") +
  scale_x_discrete(labels = c("11", "12", "01", "02", "03", "04", "05", "06", 
                              "07", "08", "09", "10", "11", "12", "01", "02", 
                              "03", "04", "05", "06", "07", "08", "09", "10",
                              "11")) +
  theme(
    panel.grid.major = element_line(color = "lightgray", size = 0.01),
    panel.background = element_rect(fill = NA),
    panel.border = element_rect(color = "gray", fill = NA),
    legend.position = "none")


## amount

amount.pl <- ggplot(subset(iso.ta200, iso.ta200[, 3] == "fog"), 
                 aes(x = date, y = amount, group = plotID, 
                 colour = plotID)) + 
  geom_line() + 
  scale_color_manual(values = col, limits = leg.fg, name = "Plot ID SP1") + 
  ylab("fog [mm]") +
  xlab("") +
  scale_x_discrete(labels = c("11", "12", "01", "02", "03", "04", "05", "06", 
                              "07", "08", "09", "10", "11", "12", "01", "02", 
                              "03", "04", "05", "06", "07", "08", "09", "10",
                              "11")) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "gray", fill = NA),
    legend.position = "bottom")


## wind direction

wd.pl <- ggplot(wd.sv.d, aes(x = datetime, y = WD)) +
                    # , group = plotID, 
                    # colour = plotID)) + 
  geom_point() + 
  scale_color_manual(values = col) + #, limits = leg.fg, name = "Plot ID SP1") + 
  ylab("wind direction [degree]") +
  xlab("") +
  scale_y_discrete(breaks = seq(0, 360, 90)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "gray", fill = NA))
   # legend.position = "bottom")
  

## afterwards merging

d18.dex.amn.wd.fg <- arrangeGrob(iso.d18, iso.dexcess, amount.pl, wd.pl, 
                                 heights = c(2/9, 2/9, 3/9, 2/9),
                                 ncol = 1)

# print "iso.mns.mnth.amnt.18O"
png("out/iso_d18_dex_amn_wd_fg.png", width = 30, height = 22, units = "cm", 
    res = 300, pointsize = 15)
print(d18.dex.amn.wd.fg)
dev.off()
