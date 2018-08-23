library(ggplot2)
library(RColorBrewer)
library(quantreg)

# Check your working directory
wd <- setwd("C:/Users/iotte/Documents/Desktop/training/")

## Read data
df <- read.csv2("iso_calc_wd.csv", header = T, sep = ";")

df$date_sample <- as.Date(df$date_sample)

df <- subset(df, df$amount_mm != "NA")



jfmamj <- subset(df, substr(df$date_sample, 6, 7) == "01"|
                      substr(df$date_sample, 6, 7) == "02"|
                      substr(df$date_sample, 6, 7) == "03"|
                      substr(df$date_sample, 6, 7) == "04"|
                      substr(df$date_sample, 6, 7) == "05"|
                      substr(df$date_sample, 6, 7) == "06")

jasond <- subset(df, substr(df$date_sample, 6, 7) == "07"|
                    substr(df$date_sample, 6, 7) == "08"|
                    substr(df$date_sample, 6, 7) == "09"|
                    substr(df$date_sample, 6, 7) == "10"|
                    substr(df$date_sample, 6, 7) == "11"|
                    substr(df$date_sample, 6, 7) == "12")

sav5 <- subset(jasond, jasond$plot_id_sp1 == "sav5")
sav5.rn <- subset(sav5, sav5$type == "rain")

#quantile(sav5.rn$amount_mm, na.rm = TRUE, probs = 0.5)
quantile(sav5.rn$amount_mm, na.rm = TRUE, probs = c(0.3, 0.7))

