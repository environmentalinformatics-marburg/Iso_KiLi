library(ggplot2)
library(splines)
library(MASS)
library(RColorBrewer)

# Check your working directory
wd <- setwd("C:/Users/iotte/Documents/Desktop/training")


# Read data
iso <- read.csv2("iso_calc.csv", header = T)

# Split date
date <- as.Date(iso$date_sample)
iso$year <- substr(date, 1, 4)
iso$mnth <- substr(date, 6, 7)
iso$yrmn <- paste(iso$year, iso$mnth, sep = "-")


# Colorscale for plotting stuff
col.id <- c("orange", "red", "black", "yellow", "blue", "purple", "green", 
            "cornflowerblue", "pink", "white", "brown")


## amount over time per plot
am.mn <- qplot(date, amount_mm, data = iso, color = plot_id_sp1, geom = "line") + 
  scale_color_manual(values = col.id) + facet_grid (plot_id_sp1 ~ type)



am.mn.nw <- qplot(amount_mm, d18_16, data = iso, color = plot_id_sp1, geom = "line") + 
  scale_color_manual(values = col.id) + facet_grid (plot_id_sp1 ~ type)


## subset ??ber Zeit 2012-11 - 2013-11 und 2013-12 - 2014-11
iso.1213 <- subset(iso, iso$yrmn < "2013-12")
date1213 <- as.Date(iso.1213$date_sample)
iso.1213.pl <- qplot(date1213, amount_mm, data = iso.1213, color = plot_id_sp1, geom = "line", ) + 
  scale_color_manual(values = col.id) + facet_grid (type ~ .)

iso.1314 <- subset(iso, iso$yrmn > "2013-11")
date1314 <- as.Date(iso.1314$date_sample)
iso.1314.pl <- qplot(date1314, amount_mm, data = iso.1314, color = plot_id_sp1, geom = "line", ) + 
  scale_color_manual(values = col.id) + facet_grid (type ~ .)
fog.1314 <- subset(iso.1314, type == "fog")


iso.1213.agg <- aggregate(iso.1213$amount_mm, by = list(iso.1213$plot_id_sp1, iso.1213$type), 
                          FUN = "sum", na.rm = TRUE)
iso.1314.agg <- aggregate(iso.1314$amount_mm, by = list(iso.1314$plot_id_sp1, iso.1314$type), 
                          FUN = "sum", na.rm = TRUE)
