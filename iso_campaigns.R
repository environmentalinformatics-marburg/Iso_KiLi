library(ggplot2)
library(lubridate)

# Check your working directory
wd <- setwd("C:/Users/IOtte/Desktop/training/")

## Read data
iso <- read.csv2("iso_calc.csv", header = T)
iso1 <- read.csv2("iso_campaign1.csv", header = T)

# Iso Date
iso$date_sample <- as.Date(iso$date_sample)
iso2 <- subset(iso, iso$date_sample > "2014-04-04" &
                 iso$date_sample < "2014-04-15" &
                 iso$plot_id_tf != "red" &
                 iso$plot_id_tf != "yellow" &
                 iso$plot_id_tf != "white" &
                 iso$plot_id_sp1 != "mnp1" &
                 iso$plot_id_sp1 != "mnp2")



# col.id
col.id <- c("orange", "red", "black", "yellow", "blue", "purple", "green", 
            "pink", "cornflowerblue", "white", "brown")
col.id.1 <- c("orange", "black", "blue", "purple", "green", "brown")




## Scatterplot 
# iso
sc.pl <- qplot(d18_16, dD_H, data = iso, color = plot_id_sp1, shape = type, xlab = "d18O%oGMWL",
                 ylab = "d2H%oGMWL") + scale_color_manual(values = col.id)
sm.all <- summary(lm(dD_H ~ d18_16, data = iso))
sm.rain <- summary(lm(dD_H ~ d18_16, data = subset(iso, iso$type == "rain")))
sm.fog <- summary(lm(dD_H ~ d18_16, data = subset(iso, iso$type == "fog"))) 
sm.tf <- summary(lm(dD_H ~ d18_16, data = subset(iso, iso$type == "tf")))



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
