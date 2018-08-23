library(ggplot2)
library(lubridate)

# Check your working directory
wd <- setwd("C:/Users/iotte/Documents/Desktop/training/")

# Read data
iso <- read.csv2("iso_calc.csv", header = T)
ta.200 <- read.csv("temperature/plots.csv", header = T)


## Iso Date
iso$date_sample <- as.Date(iso$date_sample)
iso$year <- substr(iso$date_sample, 1, 4)
iso$mnth <- substr(iso$date_sample, 6, 7)
iso$yrmn <- paste(iso$year, iso$mnth, sep = "-")
iso$week <- week(iso$date_sample)
iso$yrweek <- paste(iso$year, iso$week, sep = "-")


## Subset Type
iso.mnt.rain <- subset(iso, iso$type == "rain")
iso.mnt.tf <- subset(iso, iso$type == "tf")
iso.mnt.fog <- subset(iso, iso$type == "fog")

################
## gute Darstellung, ausbauen! als y-Achse d18O oder d-excess verwenden!!
###############
iso.mnt <- qplot(d18_16, elevation, data = iso, shape = type) + 
  facet_grid(type ~ plot_id_sp1) + geom_abline()

## Korrelationen Regenmenge(per Subset-Type) f??r Deuterium und 18O
## basierend auf Wochenrohdaten
## Outlier checken!!!
iso.mnt <- qplot(d18_16, amount_mm, data = iso, shape = type) + 
  facet_grid(type ~ plot_id_sp1) + geom_abline()

cor(iso$d18_16, iso$amount_m, use = "pairwise.complete.obs",
    method = "spearman")
cor(iso$dD_H, iso$amount_m, use = "pairwise.complete.obs",
    method = "spearman")

cor(iso.mnt.rain$d18_16, iso.mnt.rain$amount_m, use = "pairwise.complete.obs",
    method = "spearman")
cor(iso.mnt.rain$dD_H, iso.mnt.rain$amount_m, use = "pairwise.complete.obs",
    method = "spearman")
cor(iso.mnt.tf$d18_16, iso.mnt.tf$amount_m, use = "pairwise.complete.obs",
    method = "spearman")
cor(iso.mnt.tf$dD_H, iso.mnt.tf$amount_m, use = "pairwise.complete.obs",
    method = "spearman")
cor(iso.mnt.fog$d18_16, iso.mnt.fog$amount_m, use = "pairwise.complete.obs",
    method = "spearman")
cor(iso.mnt.fog$dD_H, iso.mnt.fog$amount_m, use = "pairwise.complete.obs",
    method = "spearman")

## Korrelationen Temperatur (per Subset-Type) f??r Deuterium und 18O

#ta.200.ts <- do.call("c", lapply(iso.year, function(i) { 
#  seq(as.Date(paste(i, "01", "01", sep = "-")), 
#      as.Date(paste(i, "12", "31", sep = "-")), 1)
#}))

#ta.200.iso <- merge(data.frame(ta.200), 
#                     data.frame(iso), 
#                     by = "plot_id_sp1", all.x = T)

# Temperature Date
ta.200$year <- substr(ta.200$datetime, 1, 4)
ta.200$week <- week(ta.200$datetime)
ta.200$yrweek <- paste(ta.200$year, ta.200$week, sep = "-")

# Aggregate Temperature to weekly means
ta.200.yrweek <- aggregate(ta.200$Ta_200, by = list(ta.200$yrweek, ta.200$plotID),
                           FUN = "mean")
colnames(ta.200.yrweek) <- c("yrweek", "plot_id_sp1", "ta_200")

# merge temperature- and iso-table for correlation
iso.ta.200 <- merge(iso, ta.200.yrweek)

iso.ta.200.mnt <- qplot(d18_16, ta_200, data = iso.ta.200) + 
  geom_smooth(method = lm, se = FALSE)

iso.ta.200.mnt.type <- qplot(d18_16, ta_200, data = iso.ta.200, shape = type) + 
  facet_grid(type ~ plot_id_sp1) + geom_smooth(method = lm, se = FALSE)

# Subset Type
iso.ta.200.rain <- subset(iso.ta.200, iso.ta.200$type == "rain")
iso.ta.200.tf <- subset(iso.ta.200, iso.ta.200$type == "tf")
iso.ta.200.fog <- subset(iso.ta.200, iso.ta.200$type == "fog")


## Korrelationen Regenmenge(per Subset-Type) f??r Deuterium und 18O
## basierend auf Wochenrohdaten
## Outlier checken!!!
cor(iso.ta.200$d18_16, iso.ta.200$ta_200, use = "pairwise.complete.obs",
    method = "pearson")
cor(iso.ta.200$dD_H, iso.ta.200$ta_200, use = "pairwise.complete.obs",
    method = "pearson")

cor(iso.ta.200.rain$d18_16, iso.ta.200.rain$ta_200, use = "pairwise.complete.obs",
    method = "pearson")
cor(iso.ta.200.rain$dD_H, iso.ta.200.rain$ta_200, use = "pairwise.complete.obs",
    method = "pearson")
cor(iso.ta.200.tf$d18_16, iso.ta.200.tf$ta_200, use = "pairwise.complete.obs",
    method = "pearson")
cor(iso.ta.200.tf$dD_H, iso.ta.200.tf$ta_200, use = "pairwise.complete.obs",
    method = "pearson")
cor(iso.ta.200.fog$d18_16, iso.ta.200.fog$ta_200, use = "pairwise.complete.obs",
    method = "pearson")
cor(iso.ta.200.fog$dD_H, iso.ta.200.fog$ta_200, use = "pairwise.complete.obs",
    method = "pearson")





#### Amount
# build monthly mean values of d18-16 & dD_H
iso.mns <- aggregate(cbind(iso$d18_16, iso$dD_H), by = list(iso$yrmn, iso$plot_id_sp1, iso$type),
                     FUN = "mean", na.rm = TRUE)
colnames(iso.mns) <- c("date", "plot_id_sp1", "type", "d18_16", "dD_H")


# build monthly sums of amount_mm
amnt.mns <- aggregate(iso$amount_mm, by = list(iso$yrmn, iso$plot_id_sp1, iso$type), 
                      FUN = "sum", na.rm = TRUE)
colnames(amnt.mns) <- c("date", "plot_id_sp1", "type", "amount_mm")
head(amnt.mns)

# merge monthly mean of d18-16 & dD_H and monthly sums of amount_mm
# to create graphik for publication
iso.mns.all <- merge(iso.mns, amnt.mns)




amnt.mn.plt <- qplot(date, amount_mm, data = amnt.mns)

amnt.mns$year <- substr(amnt.mns$date, 1, 4)
amnt.mns$mnth <- substr(amnt.mns$date, 6, 7)

head(amnt.mns)

amnt.mns.yr <- aggregate(amnt.mns$amount_mm, by = list(amnt.mns$mnth, amnt.mns$plot_id_sp1, amnt.mns$type), 
                         FUN = "sum")
colnames(amnt.mns.yr) <- c("date", "plot_id_sp1", "type", "amount_mm_sum")
amnt.mn.yr.plt <- qplot(date, amount_mm_sum, data = amnt.mns.yr)
amnt.mns.yr$amount_mm_mean <- amnt.mns.yr$amount_mm_sum/2
amnt.mn.yr.plt <- qplot(date, amount_mm_mean, data = amnt.mns.yr, geom = "boxplot")

amnt.mn.yr.plt.plt <- qplot(date, amount_mm_mean, data = amnt.mns.yr, color = type)  + 
  facet_wrap( ~ plot_id_sp1) 

test <- ggplot(amnt.mns.yr, aes(date, amount_mm_mean)) 
+ geom_point()
+ facet_grid( ~ plot_id_sp1)


## Temperature Date
ta.200$date_sample <- as.Date(ta.200$datetime)

iso.ta.200 <- merge(data.frame(ta.200), 
                    data.frame(iso), 
                    by = "date_sample", all.x = T)

#iso.ta.2013 <- subset(iso.ta.200(substr(iso.ta.200$date_sample, 1, 4) == "2013"))
iso.ta.200$yearyear <- substr(iso.ta.200$date_sample, 1, 4)                            
iso.ta.2013 <- subset(iso.ta.200, iso.ta.200$yearyear == "2013")




ta.200.mns <- aggregate(ta.200$Ta_200, by = list(ta.200$yrmn, ta.200$plotID), 
                        FUN = "mean", na.rm = TRUE)
colnames(ta.200.mns) <- c("date", "plot_id_sp1", "Ta_200")

head(ta.200.mns)


amnt.mn.plt <- qplot(date, amount_mm, data = amnt.mns)

amnt.mns$year <- substr(amnt.mns$date, 1, 4)
amnt.mns$mnth <- substr(amnt.mns$date, 6, 7)

head(amnt.mns)

amnt.mns.yr <- aggregate(amnt.mns$amount_mm, by = list(amnt.mns$mnth, amnt.mns$plot_id_sp1, amnt.mns$type), 
                      FUN = "sum")
colnames(amnt.mns.yr) <- c("date", "plot_id_sp1", "type", "amount_mm_sum")
amnt.mn.yr.plt <- qplot(date, amount_mm_sum, data = amnt.mns.yr)
amnt.mns.yr$amount_mm_mean <- amnt.mns.yr$amount_mm_sum/2
amnt.mn.yr.plt <- qplot(date, amount_mm_mean, data = amnt.mns.yr, geom = "boxplot")

amnt.mn.yr.plt.plt <- qplot(date, amount_mm_mean, data = amnt.mns.yr, color = type)  + 
  facet_wrap( ~ plot_id_sp1) 

test <- ggplot(amnt.mns.yr, aes(date, amount_mm_mean)) 
+ geom_point()
+ facet_grid( ~ plot_id_sp1)

