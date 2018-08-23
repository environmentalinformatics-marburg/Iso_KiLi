library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(grid)

# Check your working directory
wd <- setwd("C:/Users/iotte/Documents/Desktop/training/")

## Read data
iso <- read.csv2("iso_calc_intensivecamp.csv", header = T)


leg <- c("fer0", "fpd0", "fpo0", "foc0", "hom4", "sav5") 
col.id <- c("#6a3d9a", "#a6cee3", "#1f78b4", "#33a02c", "#6a3d9a", "#b15928")

leg.ic <- c("c1", "c2") 
col.ic <- c("red", "blue")

# Iso Date
iso$date_sample <- as.Date(iso$date_sample)

iso1 <- subset(iso, iso$date_sample < "2014-04-01")
iso2 <- subset(iso, iso$date_sample > "2014-04-01")

iso1.sm <- summary(lm(dD_H ~ d18_16, data = iso1))
iso2.sm <- summary(lm(dD_H ~ d18_16, data = iso2))

## Scatterplot 
# iso
#sc.pl <- qplot(d18_16, dD_H, data = iso, color = plot_id_sp1, shape = type, xlab = "d18O%oGMWL",
#                 ylab = "d2H%oGMWL") + scale_color_manual(values = col.id)
#sm.all <- summary(lm(dD_H ~ d18_16, data = iso))
#sm.rain <- summary(lm(dD_H ~ d18_16, data = subset(iso, iso$type == "rain")))
#sm.fog <- summary(lm(dD_H ~ d18_16, data = subset(iso, iso$type == "fog"))) 
#sm.tf <- summary(lm(dD_H ~ d18_16, data = subset(iso, iso$type == "tf")))

iso.sm <- summary(lm(dD_H ~ d18_16, data = iso))


iso$campaign <- as.factor(iso$campaign)

iso$plot_id_sp1 <- factor(iso$plot_id_sp1, levels = c("fer0", "fpd0", "fpo0", "foc0",
                                                      "hom4", "sav5"))

iso.ic.lmwl.gmwl <- qplot(d18_16, dD_H, data = iso, color = campaign, shape = type, 
                           xlab = expression(delta^{18}*O ~ "\u2030"),
                           ylab = expression(delta^{2}*H ~ "\u2030")) + 
  facet_wrap( ~ plot_id_sp1) +
  geom_abline(intercept = 12.80, slope = 7.19, linetype = 2, color = "blue") + 
  geom_abline(intercept = 16.61, slope = 7.44, linetype = 2, color = "red") +
  geom_abline(intercept = 10, slope = 8) +
  scale_color_manual(values = col.ic, name = "campaign") +
  theme(strip.text = element_text(face = "bold", size = rel(1.0))) +
  theme(axis.text = element_text(size = rel(0.9))) +
  theme(axis.title = element_text(face = "bold", size = rel(1.0))) +
  theme(legend.text = element_text(size = rel(1.0))) +
  theme(legend.title = element_text(face = "bold", size = rel(1.0))) +
  theme(
    panel.grid.major = element_line(color = "lightgray", size = 0.01),
    panel.background = element_rect(fill = NA),
    panel.border = element_rect(color = "gray", fill = NA))
  #theme(legend.key.size = unit(1.0, "cm")) +
  

# print "iso.ic.lmwl.gmwl"
png("C:/Users/iotte/Desktop/out/03_iso.ic.lmwl.gmwl.png", width = 30, height = 24, 
    units = "cm", res = 300, pointsize = 15)
print(iso.ic.lmwl.gmwl)
dev.off()



## Find regression line for all types of precipitation of the machame transect
## for all data pairs available

iso.machame <- subset(iso, iso$plot_id_sp1 != "hom4" &
                        iso$plot_id_sp1 != "sav5" &
                        iso$plot_id_sp1 != "fpd0" &
                        iso$plot_id_sp1 != "nkw1" &
                        iso$plot_id_sp1 != "mnp1" &
                        iso$plot_id_sp1 != "mnp2")

# plot for checking reasons

col.id.machame <- c("orange", "red", "black", "yellow", "purple")
sc.pl.machame <- qplot(d18_16, dD_H, data = iso.machame, color = plot_id_sp1, shape = type, xlab = "d18O%oGMWL",
               ylab = "d2H%oGMWL") + scale_color_manual(values = col.id.machame)

## regression machame transect only

sm.all.machame <- summary(lm(dD_H ~ d18_16, data = iso.machame))

# machame rain
rain.machame <- subset(iso.machame, iso.machame$type == "rain")
sm.rain.machame <- summary(lm(dD_H ~ d18_16, data = subset(iso.machame, iso.machame$type == "rain")))

# machame fog
machame.fog <- subset(iso.machame, iso.machame$type == "fog")
sm.fog.machame <- summary(lm(dD_H ~ d18_16, data = subset(iso.machame, iso.machame$type == "fog")))  
  
# machame throughfall 
machame.tf <- subset(iso.machame, iso.machame$type == "tf")
sm.tf.machame <- summary(lm(dD_H ~ d18_16, data = subset(iso.machame, iso.machame$type == "tf"))) 
  
  
### regression fer0 only

fer0 <- subset(iso.machame, iso.machame$plot_id_sp1 == "fer0")
sm.fer0 <- summary(lm(dD_H ~ d18_16, data = subset(iso.machame, iso.machame$plot_id_sp1 == "fer0")))  

# fer0 rain
fer0.rain <- subset(fer0, fer0$type == "rain")
sm.fer0.rain <- summary(lm(dD_H ~ d18_16, data = subset(fer0, fer0$type == "rain")))

# fer0 fog
fer0.fog <- subset(fer0, fer0$type == "fog")
sm.fer0.fog <- summary(lm(dD_H ~ d18_16, data = subset(fer0, fer0$type == "fog")))

# fer0 tf
fer0.tf <- subset(fer0, fer0$type == "tf")
sm.fer0.tf <- summary(lm(dD_H ~ d18_16, data = subset(fer0, fer0$type == "tf")))


### regression flm1 only

flm1 <- subset(iso.machame, iso.machame$plot_id_sp1 == "flm1")
sm.flm1 <- summary(lm(dD_H ~ d18_16, data = subset(iso.machame, iso.machame$plot_id_sp1 == "flm1")))  

# flm1 rain
flm1.rain <- subset(flm1, flm1$type == "rain")
sm.flm1.rain <- summary(lm(dD_H ~ d18_16, data = subset(flm1, flm1$type == "rain")))

# flm1 fog
flm1.fog <- subset(flm1, flm1$type == "fog")
sm.flm1.fog <- summary(lm(dD_H ~ d18_16, data = subset(flm1, flm1$type == "fog")))

# flm1 tf
flm1.tf <- subset(flm1, flm1$type == "tf")
sm.flm1.tf <- summary(lm(dD_H ~ d18_16, data = subset(flm1, flm1$type == "tf")))







## Having a closer look on the second intensive campaign: iso2 (April 2013)
sc.pl.2 <- qplot(d18_16, dD_H, data = iso2, color = plot_id_sp1, shape = type, xlab = "d18O%oGMWL",
               ylab = "d2H%oGMWL") + scale_color_manual(values = col.id)
summary(lm(dD_H ~ d18_16, data = iso2))
sc.pl.lmwl.2 <- qplot(d18_16, dD_H, data = iso2, geom = c("point", "smooth"), method = "lm", se = FALSE, 
                    formula = y ~ x)
sc.pl.lmwl.gmwl.2 <- sc.pl.lmwl + geom_abline(intercept = 10, slope = 8)

#iso1
sc.pl.1 <- qplot(d18_16, dD_H, data = iso1, color = plot_id_sp1, shape = type, xlab = "d18O%oGMWL",
               ylab = "d2H%oGMWL") + scale_color_manual(values = col.id)
summary(lm(dD_H ~ d18_16, data = iso1))
sc.pl.lmwl.1 <- qplot(d18_16, dD_H, data = iso1, geom = c("point", "smooth"), method = "lm", se = FALSE, 
                    formula = y ~ x)
sc.pl.lmwl.gmwl.1 <- sc.pl.lmwl + geom_abline(intercept = 10, slope = 8)
