library(ggplot2)
library(splines)
library(MASS)
library(RColorBrewer)


# Check your working directory
wd <- setwd("C:/Users/IOtte/Desktop/training/")

# Read data
iso <- read.csv2("iso_calc.csv", header = T)
iso.cp1 <- read.csv2("iso_campaign1.csv", header = T)
iso.cp2 <- read.csv("iso2.csv", header = T)

date <- as.Date(iso$date_sample)
iso$year <- substr(date, 1, 4)
iso$mnth <- substr(date, 6, 7)
iso$yrmn <- paste(iso$year, iso$mnth, sep = "-")
  


### Visualizing stuff
# Colorscale
col.id <- c("orange", "red", "black", "yellow", "blue", "purple", "green", 
            "pink", "cornflowerblue", "white", "brown")
col.id.1 <- c("orange", "black", "blue", "purple", "green", "brown")


# Scatterplot 
sc.pl <- qplot(d18_16, dD_H, data = iso, color = plot_id_sp1, shape = type, xlab = "d18O%oGMWL",
               ylab = "d2H%oGMWL") + scale_color_manual(values = col.id)
summary(lm(dD_H ~ d18_16, data = iso))
sc.pl.lmwl <- qplot(d18_16, dD_H, data = iso, geom = c("point", "smooth"), method = "lm", se = FALSE, 
                    formula = y ~ x)
sc.pl.lmwl.gmwl <- sc.pl.lmwl + geom_abline(intercept = 10, slope = 8)
sc.pl.lmwl.gmwl.1 <- sc.pl.lmwl + geom_abline(intercept = 10.35, slope = 8.17)
sc.pl.lmwl.gmwl.2 <- sc.pl.lmwl + geom_abline(intercept = 15.41, slope = 7.55)


sc.pl.1 <- qplot(d18_16, dD_H, data = iso.cp1, color = plot_id_sp1, shape = type, xlab = "d18O%oGMWL",
                 ylab = "d2H%oGMWL") + scale_color_manual(values = col.id.1)
sc.pl.gmwl.1 <- sc.pl.1 + geom_abline(intercept = 10, slope = 8)

sc.pl.2 <- qplot(d18_16, dD_H, data = iso.cp2, color = plot_id_sp1, shape = type, xlab = "d18O%oGMWL",
                 ylab = "d2H%oGMWL") + scale_color_manual(values = col.id.1)
sc.pl.gmwl.2 <- sc.pl.2 + geom_abline(intercept = 10, slope = 8)

sc.pl.2 <- qplot(d18_16, dD_H, data = iso.cp2, color = type, shape = plot_id_sp1, xlab = "d18O%oGMWL",
                 ylab = "d2H%oGMWL")
sc.pl.gmwl.2 <- sc.pl.2 + geom_abline(intercept = 10.35, slope = 8.17)



# Boxplot
bw <- qplot(plot_id_sp1, data = iso, color = plot_id_sp1, xlab = "") + 
  scale_color_manual(values = col.id)
bw.jt <- qplot(plot_id_sp1, amount_mm, data = iso, color = type, position = "jitter", xlab = "")

bw.1 <- qplot(plot_id_sp1, data = iso.cp1, color = plot_id_sp1, xlab = "") + 
  scale_color_manual(values = col.id.1)
bw.jt.1 <- qplot(plot_id_sp1, amount_mm, data = iso.cp1, color = type, position 
                 = "jitter", xlab = "")


# Faceting
sc.type <- qplot(d18_16, dD_H, data = iso, color = plot_id_sp1) + facet_grid(. ~ type) + 
  scale_color_manual(values = col.id)
sc.type.gmwl <- sc.type + geom_abline(intercept = 10, slope = 8)


sc.pl.id <- qplot(d18_16, dD_H, data = iso, color = type) + facet_grid(. ~ plot_id_sp1) 
sc.type.id <- qplot(d18_16, dD_H, data = iso, color = plot_id_sp1) + 
  facet_grid(type ~ plot_id_sp1) + scale_color_manual(values = col.id)

sc.type.1 <- qplot(d18_16, dD_H, data = iso.cp1, color = plot_id_sp1) + facet_grid(. ~ type) + 
  scale_color_manual(values = col.id.1)
sc.pl.id.1 <- qplot(d18_16, dD_H, data = iso.cp1, color = type) + facet_grid(. ~ plot_id_sp1) 
sc.type.id.1 <- qplot(d18_16, dD_H, data = iso.cp1, color = plot_id_sp1) + 
  facet_grid(type ~ plot_id_sp1) + scale_color_manual(values = col.id.1)


# geom
#temp <- ggplot(iso, aes(d18_16, dD_H, amount))
#aes <- temp + layer(geom = "contour")

sc.al <- qplot(d18_16, dD_H, data = iso, geom = c("point", "smooth"), method = "rlm") # rlm like lm but less affected to outliers

cs.pl.al <- qplot(d18_16, dD_H, data = iso, color = plot_id_sp1, geom = c("point", "smooth"), method = "rlm") + 
  facet_grid(type ~ plot_id_sp1) + scale_color_manual(values = col.id)

isoplot.all <- qplot(plot_id_sp1, amount_mm, data = iso, color = plot_id_sp1, geom = "boxplot", xlab = "") + 
  facet_grid(. ~ type) + scale_color_manual(values = col.id)


sc.al.1 <- qplot(d18_16, dD_H, data = iso.cp1, geom = c("point", "smooth"), method = "rlm") # rlm like lm but less affected to outliers

cs.pl.al.1 <- qplot(d18_16, dD_H, data = iso.cp1, color = plot_id_sp1, geom = c("point", "smooth"), method = "rlm") + 
  facet_grid(type ~ plot_id_sp1) + scale_color_manual(values = col.id.1)

isoplot.all.1 <- qplot(plot_id_sp1, amount_mm, data = iso.cp1, color = plot_id_sp1, geom = "boxplot", xlab = "") + 
  facet_grid(. ~ type) + scale_color_manual(values = col.id.1)


## amount over time per plot

am.mn <- qplot(date, amount_mm, data = iso, color = plot_id_sp1, geom = "line") + 
  scale_color_manual(values = col.id) + facet_grid (plot_id_sp1 ~ type)

rain <- subset(iso, iso$type == "rain")
date <- as.Date(rain$date_sample)
am.mn.rain <- qplot(date, amount_mm, data = rain, color = plot_id_sp1, geom = "line") + 
  scale_color_manual(values = col.id) 


fog <- subset(iso, iso$type == "fog")
date <- as.Date(fog$date_sample)
am.mn.fog <- qplot(date, amount_mm, data = fog, color = plot_id_sp1, geom = "line") + 
  scale_color_manual(values = col.id)


tf <- subset(iso, iso$type == "tf")
date <- as.Date(tf$date_sample)
am.mn.tf <- qplot(date, amount_mm, data = tf, color = plot_id_sp1, geom = "line") + 
  scale_color_manual(values = col.id)




## subset ??ber Zeit 2012-11 - 2013-11 und 2013-11 - 2014-11

iso.1213 <- subset(iso, iso$yrmn < "2013-12")
date1213 <- as.Date(iso.1213$date_sample)

iso.1213.pl <- qplot(date1213, amount_mm, data = iso.1213, color = plot_id_sp1, geom = "line", ) + 
  scale_color_manual(values = col.id)


iso.1314 <- subset(iso, iso$yrmn > "2013-11")
date1314 <- as.Date(iso.1314$date_sample)

iso.1314.pl <- qplot(date1314, amount_mm, data = iso.1314, color = plot_id_sp1, geom = "line", ) + 
  scale_color_manual(values = col.id)


# subset fog
#fog <- subset(iso, iso$type == "fog")
isoplot.fg <- qplot(plot_id_sp1, amount_mm, data = subset(iso, iso$type == "fog"), 
                    color = plot_id_sp1, geom = "boxplot", xlab = "") + 
  scale_color_manual(values = col.id)


#fog.1 <- subset(iso.cp1, iso.cp1$type == "fog")
isoplot.fg.1 <- qplot(plot_id_sp1, amount_mm, data = subset(iso.cp1, iso.cp1$type == "fog"), 
                    color = plot_id_sp1, geom = "boxplot", xlab = "") + 
  scale_color_manual(values = col.id.1)


## MACHAME
# subset machame
machame <- subset(iso, iso$plot_id_sp1 %in% c("fer0", "flm1", "foc6", "foc0", "fpo0"))

sc.pl.mch <- qplot(d18_16, dD_H, data = machame, color = plot_id_sp1, shape = type) + 
  scale_color_manual(values = col.id) + facet_grid(.~ type )


machame.bt.tp <- subset(machame, machame$plot_id_sp1 %in% c("fer0", "flm1"))

sc.pl.mch.bt.tp <- qplot(d18_16, dD_H, data = machame.bt.tp, color = plot_id_sp1, shape = type) + 
  scale_color_manual(values = col.id) + facet_grid (. ~ type)

east.west <- subset(iso, iso$plot_id_sp1 %in% c("fer0", "hom4", "sav5"))

sc.pl.east.west <- qplot(d18_16, dD_H, data = east.west, color = plot_id_sp1, shape = type) + 
  scale_color_manual(values = col.id)



## aggregate type mean
d18.16.mns <- aggregate(iso$d18_16, by = list(iso$plot_id_sp1, iso$type), 
                        FUN = "mean", na.rm = TRUE)
colnames(d18.16.mns) <- c("plot_id_sp1", "type", "d18_16_mn")

dD.H.mns <- aggregate(iso$dD_H, by = list(iso$plot_id_sp1, iso$type), 
                      FUN = "mean", na.rm = TRUE)
colnames(dD.H.mns) <- c("plot_id_sp1", "type", "dD_H_mn")


mns.agg <- merge(d18.16.mns, dD.H.mns)
sc.pl <- qplot(d18_16_mn, dD_H_mn, data = mns.agg, color = plot_id_sp1, shape = type, xlab = "d18O%oGMWL",
               ylab = "d2H%oGMWL") + scale_color_manual(values = col.id) 
sc.pl.gmwl <- sc.pl + geom_abline(intercept = 10, slope = 8)


#################################

# Subset 0 --> 99.99
zero <- subset(iso, iso$amount_mm == 0)


# Colour subset
orange <- subset(iso, iso$plot_id_tf == "orange")
orange.zero <- subset(orange, orange$amount_mm == 0)

#if orange$amount_mm == 0 & orange$d18_16 ==NA, 9999 in orange$d18_16
orange.zero.rm <- {
  ifelse (orange$amount_mm == 0, 99.99, 1)


function(n){
  ifelse (orange$amount_mm == 0),
  orange$d18_16 == 99.99
}


# Type subset
fog <- subset(iso, iso$type == "fog")
plot.fog <- plot(fog$d18_16, fog$dD_H, type = "p", main = "fog all plots",
                 xlab = "d(18_16)", ylab = "d(D_H)")

rain <- subset(iso, iso$type == "rain")
plot.rain <- plot(rain$d18_16, rain$dD_H, type = "p", main = "rain all plots",
                xlab = "d(18_16)", ylab = "d(D_H)")

tf <- subset(iso, iso$type == "tf")
plot.tf <- plot(tf$d18_16, tf$dD_H, type = "p", main = "tf all plots",
                 xlab = "d(18_16)", ylab = "d(D_H)")

# plot
plot.iso <- plot(iso$d18_16, iso$dD_H, type = "p", main = "all plots",
                 xlab = "d(18_16)", ylab = "d(D_H)")

lm.iso <- lm(iso$dD_H ~ iso$d18_16)

abline(lm.iso)


