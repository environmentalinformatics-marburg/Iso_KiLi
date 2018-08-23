# Check your working directory
wd <- setwd("C:/Users/iotte/Documents/Desktop/training/")

df <- read.csv2("iso_lapse_seasonality_dh.csv", header = T, sep = ";")

df <- subset(df, df$Station != "Fpd0" &
               df$Station != "Sav5" &
               df$Station != "Hom4")


#summary(lm(all_d18_16_w ~ Elevation, data = df))
df.rain <- subset(df, df$type == "rain")

summary(lm(dry_jasond ~ Elevation, data = df.rain))





x1213.uw.d1816 <- qplot(Elevation, X1213_d18_16_uw, data = df) +
  geom_abline(intercept = 0.1155418, slope = -0.0016138) +
  facet_grid(.~type)
summary(lm(X1213_d18_16_uw ~ Elevation, data = rain))

x1213.w.d1816 <- qplot(Elevation, X1213_d18_16_w, data = df) +
  geom_abline(intercept = -2.162234, slope = -0.001145) +
  facet_grid(.~type)
summary(lm(X1213_d18_16_w ~ Elevation, data = rain))