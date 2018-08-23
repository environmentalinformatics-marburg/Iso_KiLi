library(ggplot2)
library(RColorBrewer)
library(plyr)

# Check your working directory
wd <- setwd("C:/Users/iotte/Desktop/HydrologicalProcesses/")


## Read data
iso <- read.csv2("C:/Users/iotte/Documents/Desktop/training/iso_calc.csv", header = T, sep = ";")
# sort legends
leg <- c("fer0", "fpd0", "fpo0", "foc0", "foc6", "flm1", "nkw1",
         "hom4", "sav5")

iso <- subset(iso, iso$plot_id_sp1 != "mnp1" &
               iso$plot_id_sp1 != "mnp2")

# build monthly mean values of d18_16 and dD_H
iso$yrmn <- substr(iso$date_sample, 1, 7)

iso.mnth <- aggregate(cbind(iso$d18_16, iso$dD_H, iso$d_excess), 
                     by = list(iso$yrmn, iso$plot_id_sp1, iso$type, iso$elevation),
                     FUN = "mean", na.rm = TRUE)

colnames(iso.mnth) <- c("date", "plot_id_sp1", "type", "elevation",
                       "d18_16", "dD_H", "d_excess")


# build monthly sums of amount_mm
iso.amnt.mnth <- aggregate(iso$amount_mm, by = list(iso$yrmn, iso$plot_id_sp1,
                                                  iso$type, iso$elevation),
                          FUN = "sum", na.rm = TRUE)

colnames(iso.amnt.mnth) <- c("date", "plot_id_sp1", "type", "elevation","amount_mm")

# merge monthly mean of d18-16 & dD_H and monthly sums of amount_mm
# to create graphik
iso.mnth.amnt <- merge(iso.amnt.mnth, iso.mnth)

write.csv2(iso.mnth.amnt, file = "iso_mnth_amnt.csv", sep = ";")


###
## Read data
df <- read.csv("plots_2.csv", header = T)

df$datetime <- as.Date(df$datetime)
df$mnth <- substr(df$datetime, 3, 7)

# build monthly mean values of d18_16 and dD_H
df.mnth <- aggregate(cbind(df$rH_200, df$Ta_200), 
                     by = list(df$mnth, df$plotID),
                     FUN = "mean", na.rm = TRUE)

colnames(df.mnth) <- c("date", "plot_id_sp1", "rH_200", "Ta_200")
levels(df.mnth$plot_id_sp1)[levels(df.mnth$plot_id_sp1) == "fpd2"] <- "fpd0"
levels(df.mnth$plot_id_sp1)[levels(df.mnth$plot_id_sp1) == "mch0"] <- "fpo0"

write.csv2(df.mnth, file = "df_mnth_again2.csv", sep = ";")
str(df.mnth)
