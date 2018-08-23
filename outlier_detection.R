library(ggplot2)


# Check your working directory
wd <- setwd("C:/Users/iotte/Documents/Desktop/training/")
############### plotting

df <- read.csv2("iso_calc.csv", header = T, sep = ";")


### Ziel: outlier-detection pro plot pro typ
## subset type, subset plot
plot <- subset(df, df$plot_id_sp1 == "fer0")
prcp <- subset(plot, plot$type == "tf")

qplot(dD_H, data = prcp)

quantile(prcp$dD_H, probs = 0.025, na.rm = TRUE)
quantile(prcp$dD_H, probs = 0.975, na.rm = TRUE)

fer0.tf <- subset(prcp, prcp$dD_H >= -57.10753 &
                    prcp$dD_H <= 6.9725) 

fer0 <- rbind(fer0.fog, fer0.rain)
fer0 <- rbind(fer0, fer0.tf)

iso <- rbind(iso, fer0)

write.csv2(iso, "iso_calc_rm_outlier_DH_r.csv")
