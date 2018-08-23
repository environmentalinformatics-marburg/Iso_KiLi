library(ggplot2)

# set working directory
wd <- setwd("C:/Users/IOtte/documents/Desktop/training/")

# read.data
dat.iso <- read.csv2("iso_calc.csv", header = T) 

# delete observations without precipitation
dat.iso.cmpl <- dat.iso[complete.cases(dat.iso), ]

# subset per plot
iso.fer0 <- subset(dat.iso.cmpl, plot_id_sp1 == "fer0")[, c(1, 2, 4, 5, 7, 8)]


# subset data by rain, tf and fog
iso.fer0.rn <- subset(iso.fer0, type == "rain") 
iso.fer0.tf <- subset(iso.fer0, type == "tf")  
iso.fer0.fg <- subset(iso.fer0, type == "fog")  


# merge data
ls.iso.fer0 <- list(iso.fer0.rn, iso.fer0.tf, iso.fer0.fg)
df.iso.fer0.mrg <- Reduce(function(...) merge(..., by = c(1, 2, 3), all = TRUE), 
                          ls.iso.fer0)

# remove incomplete records
df.iso.fer0.mrg.nw <- df.iso.fer0.mrg[complete.cases(df.iso.fer0.mrg), ]

# in-between values
mm.fer0 <- subset(df.iso.fer0.mrg.nw, 
                  (d18_16.y < d18_16) & (d18_16.y > d18_16.x) | 
                  (d18_16.y > d18_16) & (d18_16.y < d18_16.x))

#####################################


# subset per plot
iso.foc0 <- subset(dat.iso.cmpl, plot_id_sp1 == "foc0")[, c(1, 2, 4, 5, 7, 8)]


# subset data by rain, tf and fog
iso.foc0.rn <- subset(iso.foc0, type == "rain") 
iso.foc0.tf <- subset(iso.foc0, type == "tf")  
iso.foc0.fg <- subset(iso.foc0, type == "fog")  


# merge data
ls.iso.foc0 <- list(iso.foc0.rn, iso.foc0.tf, iso.foc0.fg)
df.iso.foc0.mrg <- Reduce(function(...) merge(..., by = c(1, 2, 3), all = TRUE), 
                          ls.iso.foc0)

# remove incomplete records
df.iso.foc0.mrg.nw <- df.iso.foc0.mrg[complete.cases(df.iso.foc0.mrg), ]

# in-between values
mm.foc0 <- subset(df.iso.foc0.mrg.nw, 
                  (d18_16.y < d18_16) & (d18_16.y > d18_16.x) | 
                    (d18_16.y > d18_16) & (d18_16.y < d18_16.x))


###################################


# subset per plot
iso.flm1 <- subset(dat.iso.cmpl, plot_id_sp1 == "flm1")[, c(1, 2, 4, 5, 7, 8)]


# subset data by rain, tf and fog
iso.flm1.rn <- subset(iso.flm1, type == "rain") 
iso.flm1.tf <- subset(iso.flm1, type == "tf")  
iso.flm1.fg <- subset(iso.flm1, type == "fog")  


# merge data
ls.iso.flm1 <- list(iso.flm1.rn, iso.flm1.tf, iso.flm1.fg)
df.iso.flm1.mrg <- Reduce(function(...) merge(..., by = c(1, 2, 3), all = TRUE), 
                          ls.iso.flm1)

# remove incomplete records
df.iso.flm1.mrg.nw <- df.iso.flm1.mrg[complete.cases(df.iso.flm1.mrg), ]

# in-between values
mm.flm1 <- subset(df.iso.flm1.mrg.nw, 
                  (d18_16.y < d18_16) & (d18_16.y > d18_16.x) | 
                    (d18_16.y > d18_16) & (d18_16.y < d18_16.x))


#######################################


# subset per plot
iso.foc6 <- subset(dat.iso.cmpl, plot_id_sp1 == "foc6")[, c(1, 2, 4, 5, 7, 8)]


# subset data by rain, tf and fog
iso.foc6.rn <- subset(iso.foc6, type == "rain") 
iso.foc6.tf <- subset(iso.foc6, type == "tf")  
iso.foc6.fg <- subset(iso.foc6, type == "fog")  


# merge data
ls.iso.foc6 <- list(iso.foc6.rn, iso.foc6.tf, iso.foc6.fg)
df.iso.foc6.mrg <- Reduce(function(...) merge(..., by = c(1, 2, 3), all = TRUE), 
                          ls.iso.foc6)

# remove incomplete records
df.iso.foc6.mrg.nw <- df.iso.foc6.mrg[complete.cases(df.iso.foc6.mrg), ]

# in-between values
mm.foc6 <- subset(df.iso.foc6.mrg.nw, 
                  (d18_16.y < d18_16) & (d18_16.y > d18_16.x) | 
                    (d18_16.y > d18_16) & (d18_16.y < d18_16.x))


#######################################


# subset per plot
iso.fpo0 <- subset(dat.iso.cmpl, plot_id_sp1 == "fpo0")[, c(1, 2, 4, 5, 7, 8)]


# subset data by rain, tf and fog
iso.fpo0.rn <- subset(iso.fpo0, type == "rain") 
iso.fpo0.tf <- subset(iso.fpo0, type == "tf")  
iso.fpo0.fg <- subset(iso.fpo0, type == "fog")  


# merge data
ls.iso.fpo0 <- list(iso.fpo0.rn, iso.fpo0.tf, iso.fpo0.fg)
df.iso.fpo0.mrg <- Reduce(function(...) merge(..., by = c(1, 2, 3), all = TRUE), 
                          ls.iso.fpo0)

# remove incomplete records
df.iso.fpo0.mrg.nw <- df.iso.fpo0.mrg[complete.cases(df.iso.fpo0.mrg), ]

# in-between values
mm.fpo0 <- subset(df.iso.fpo0.mrg.nw, 
                  (d18_16.y < d18_16) & (d18_16.y > d18_16.x) | 
                    (d18_16.y > d18_16) & (d18_16.y < d18_16.x))


#######################################


# subset per plot
iso.fpd0 <- subset(dat.iso.cmpl, plot_id_sp1 == "fpd0")[, c(1, 2, 4, 5, 7, 8)]


# subset data by rain, tf and fog
iso.fpd0.rn <- subset(iso.fpd0, type == "rain") 
iso.fpd0.tf <- subset(iso.fpd0, type == "tf")  
iso.fpd0.fg <- subset(iso.fpd0, type == "fog")  


# merge data
ls.iso.fpd0 <- list(iso.fpd0.rn, iso.fpd0.tf, iso.fpd0.fg)
df.iso.fpd0.mrg <- Reduce(function(...) merge(..., by = c(1, 2, 3), all = TRUE), 
                          ls.iso.fpd0)

# remove incomplete records
df.iso.fpd0.mrg.nw <- df.iso.fpd0.mrg[complete.cases(df.iso.fpd0.mrg), ]

# in-between values
mm.fpd0 <- subset(df.iso.fpd0.mrg.nw, 
                  (d18_16.y < d18_16) & (d18_16.y > d18_16.x) | 
                    (d18_16.y > d18_16) & (d18_16.y < d18_16.x))


### join all subsets to one data.frame

oco <- merge(mm.foc0, mm.foc6, all = TRUE)
podo <- merge(mm.fpd0, mm.fpo0, all = TRUE)
rst <- merge(mm.flm1, mm.fer0, all = TRUE)
oco.podo <- merge(oco, podo, all = TRUE)
mm.iso <- merge(oco.podo, rst, all = TRUE)

write.csv2(mm.iso, file = "mm.iso.csv")


