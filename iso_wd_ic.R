library(ggplot2)
library(lubridate)
library(reshape)
library(gridExtra)
library(RColorBrewer)
library(scales)


# set working directory
wd <- setwd("C:/Users/IOtte/Documents/Desktop/training/")


col <- rev(brewer.pal(6, "Spectral"))
col.fg <- rev(brewer.pal(4, "Spectral"))
leg.rn <- c("fer0", "fpd0", "fpo0", "foc0", "hom4", "sav5")
leg.fg <- c("fer0", "fpd0", "fpo0", "foc0")


### load data

iso <- read.csv2("iso_calc.csv", header = T)

wd.sv <- read.csv("C:/Users/IOtte/Desktop/sav0_wd.csv", header = TRUE)


# format wd data
#wd.sv <- wd.sv[, -2]
wd.sv$datetime <- substr(wd.sv$datetime, 1, 10)
wd.sv$datetime <- as.Date(wd.sv$datetime)

rain <- subset(iso, iso$type == "rain")
tf <- subset(iso, iso$type == "tf")
fog <- subset(iso, iso$type == "fog")

sav5 <- subset(rain, rain$plot_id_sp1 == "sav5")
hom4 <- subset(rain, rain$plot_id_sp1 == "hom4")
flm1 <- subset(rain, rain$plot_id_sp1 == "flm1")
foc6 <- subset(rain, rain$plot_id_sp1 == "foc6")
foc0 <- subset(rain, rain$plot_id_sp1 == "foc0")
fpo0 <- subset(rain, rain$plot_id_sp1 == "fpo0")
fpd0 <- subset(rain, rain$plot_id_sp1 == "fpd0")
fer0 <- subset(rain, rain$plot_id_sp1 == "fer0")

fer0.cor <- cor(fer0$d18_16, fer0$d.excess, method = "spearman", use = "complete.obs")
fpd0.cor <- cor(fpd0$d18_16, fpd0$d.excess, method = "spearman", use = "complete.obs")
fpo0.cor <- cor(fpo0$d18_16, fpo0$d.excess, method = "spearman", use = "complete.obs")
foc0.cor <- cor(foc0$d18_16, foc0$d.excess, method = "spearman", use = "complete.obs")
foc6.cor <- cor(foc6$d18_16, foc6$d.excess, method = "spearman", use = "complete.obs")
flm1.cor <- cor(flm1$d18_16, flm1$d.excess, method = "spearman", use = "complete.obs")
hom4.cor <- cor(hom4$d18_16, hom4$d.excess, method = "spearman", use = "complete.obs")
sav5.cor <- cor(sav5$d18_16, sav5$d.excess, method = "spearman", use = "complete.obs")


# format iso data

iso$datetime <- paste(iso$date_sample, iso$time, sep = " ")
iso$datetime <- as.POSIXct(iso$datetime)


## d18O

iso.d18 <- ggplot(subset(iso, iso[, 5] == "fog"), 
                  aes(x = datetime, y = d18_16, group = plot_id_sp1, 
                      colour = plot_id_sp1)) + 
  geom_line() + 
  scale_color_manual(values = col, limits = leg.rn, name = "Plot ID SP1") + 
  ylab( expression(rain  ~ delta^{18}*O ~ "\u2030")) +
  xlab("") +
  scale_x_datetime(breaks = pretty_breaks()) +                     
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "gray", fill = NA),
    legend.position = "none")


## dexcess

iso.dexcess <- ggplot(subset(iso, iso[, 5] == "fog"), 
                      aes(x = datetime, y = d.excess, group = plot_id_sp1, 
                          colour = plot_id_sp1)) + 
  geom_line() + 
  scale_color_manual(values = col, limits = leg.rn, name = "Plot ID SP1") + 
  ylab( expression(rain  ~ dexcess ~ "\u2030")) +
  xlab("") +
  scale_y_reverse() +
  scale_x_datetime(breaks = pretty_breaks()) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "gray", fill = NA),
    legend.position = "none")


## amount

amount.pl <- ggplot(subset(iso, iso[, 5] == "fog"), 
                    aes(x = datetime, y = amount_mm, group = plot_id_sp1, 
                        colour = plot_id_sp1)) + 
  geom_line() + 
  scale_color_manual(values = col, limits = leg.rn, name = "Plot ID SP1") + 
  ylab("rain [mm]") +
  xlab("") +
  scale_x_datetime(breaks = pretty_breaks()) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "gray", fill = NA),
    legend.position = "bottom")


## wind direction

wd.pl <- ggplot(wd.sv, aes(x = datetime, y = WD)) +
  geom_point() + 
  scale_color_manual(values = col) +
  ylab("wind direction [degree]") +
  xlab("") +
  #scale_x_datetime(breaks = pretty_breaks()) +
  scale_y_discrete(breaks = seq(0, 360, 90)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "gray", fill = NA))


## afterwards merging

d18.dex.amn.wd.ic1.tf <- arrangeGrob(iso.d18, iso.dexcess, amount.pl, wd.pl, 
                                 heights = c(2/9, 2/9, 3/9, 2/9),
                                 ncol = 1)

# print "iso.mns.mnth.amnt.18O"
png("out/iso_d18_dex_amn_wd_fog.png", width = 14, height = 22, units = "cm", 
    res = 300, pointsize = 15)
print(d18.dex.amn.wd.ic1.tf)
dev.off()

###

iso.sm <- summary(lm(dD_H ~ d18_16, data = iso))

## Plot all available data pairs and
## add local meteoric water line (lmwl) [Rsq 0.9348] and
## global meteoric water line (gmwl) according to Craig (1961) to plot

iso.all.lmwl.gmwl.ic2 <- qplot(d18_16, dD_H, data = iso, color = plot_id_sp1, shape = type, 
                           xlab = "d18O %o \n
                           black line: LMWL for mean values dD = 7.44d18O + 16.58 (r??: 0.88), 
                           dashed: GMWL dD = 8d18O + 10", 
                           ylab = expression(delta^{2}*H ~ "\u2030")) + 
  scale_color_manual(values = col, limits = leg.rn, name = "Plot ID SP1") + 
  geom_abline(intercept = 16.58, slope = 7.44) + 
  geom_abline(intercept = 10, slope = 8, linetype = 2)

# print "iso.all.lmwl.gmwl"
png("out/iso.all.lmwl.gmwl.ic2.png", width = 22, height = 21, units = "cm", 
    res = 300, pointsize = 15)
print(iso.all.lmwl.gmwl.ic2)
dev.off()
