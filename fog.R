library(ggplot2)
library(lubridate)

# set working directory
wd <- setwd("C:/Users/IOtte/documents/Desktop/fog/")


### load data
prcp <- read.csv("foc0.csv", header = T)



### Test auf Zusammenh??nge zwischen Logger und Buckets
### Time Lag?
rain.prt <- qplot(P_RT_NRT_01, Rainfall, data = prcp, na.rm = TRUE)
rain.prt.lm <- summary(lm(as.numeric(Rainfall) ~ as.numeric(P_RT_NRT_01), 
                          data = prcp))

rain.fog <- qplot(Rainfall, Fog, data = prcp)
rain.fog.lm <- summary(lm(as.numeric(Fog) ~ as.numeric(Rainfall), 
                          data = prcp))

prt.frt <- qplot(P_RT_NRT_01, F_RT_NRT_02, data = prcp)
prt.frt.lm <- summary(lm(as.numeric(F_RT_NRT_02) ~ as.numeric(P_RT_NRT_01), 
                          data = prcp))

fog.frt <- qplot(F_RT_NRT_02, Fog, data = prcp)
fog.frt.lm <- summary(lm(as.numeric(Fog) ~ as.numeric(F_RT_NRT_02), 
                          data = prcp))


### Nur Zusammenhang zwischen P_RT_NRT und F_RT_NRT gefunden 
### (r?? = 0.17 (p < 0.001)),
### P_RT_NRT - F_RT_NRT wenn F_RT_NRT > P_RT_NRT



