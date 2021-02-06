### environment -----

## workspace clearance
rm(list = ls(all = TRUE)); closeAllConnections()

## packages
library(opentraj)
library(doParallel)
library(raster)
library(reshape2)
library(Rsenal)
library(grid)
library(latticeExtra)

## parallelization
cl <- makeCluster(3L)
registerDoParallel(cl)


### processing -----

# ## year and elevation
# kYear <- 2013
# kHeight <- 3000

## path to meteorological reanalysis data
rsv <- "/media/sd19006/data/users/iotte/R-Server"
KMetFiles <- paste0(rsv, "/hysplit/reanalysis/")
KOutFiles <- paste0(rsv, "/hysplit/out/")

## hysplit installation path
KHySplitPath <- paste0(rsv, "/apps/hysplit-886/")

## load and reformat point file
#dat.date <- read.csv2("/media/sd19006/data/users/iotte/R-Server/hysplit/data/plots_lgtm_weaklyMean_agg.csv", 
#                      header = TRUE)
dat.date <- read.csv("/media/sd19006/data/users/iotte/R-Server/hysplit/data/plots.csv", header = TRUE)
dat.date$datetime <- as.Date(dat.date$datetime)

dat <- subset(dat.date, dat.date$P_RT_NRT != "NA")
data <- subset(dat, dat$P_RT_NRT != 0.00)

nms <- read.csv2("/media/sd19006/data/users/iotte/R-Server/hysplit/data/recycFrac_allPlots_monthMeanCopy_tst.csv",
                header = TRUE)
           
#nms[grep("Longitude", nms)] <- "Longitude"
#nms[1, 8:ncol(nms)] <- gsub("\\.", "/", nms[1, 8:ncol(nms)])
#names(dat) <- nms

#mlt <- melt(dat, id.vars = 1:7, variable.name = "date", value.name = "prcp")
#mlt$date <- as.Date(mlt$date, "%d/%m/%Y")



## auxilliary plot elements
#rgb <- brick(paste0(rsv, "/data/bale/HYSPLIT/rgb.tif"))

#spt <- dat
#coordinates(spt) <- ~ Longitude + Latitude 
#proj4string(spt) <- "+init=epsg:4326"

## loop over weather stations

#sav5 <- subset(dat.date, dat.date$plotID == "sav5")
nms.sav5 <- subset(nms, nms$plotID == "sav5")

sav5 <- data

hy.traj2013 <- 
  foreach(i = 1:nrow(sav5), .packages = "opentraj", .combine = rbind) %dopar%
  {
     # lat and lon
    lat <- nms.sav5[[7]] [1]
    long <- nms.sav5[[8]] [1]
    
    #dates <- sav5$date[i]
    dates <- sav5$datetime[i]
    ######################
    #output.file.name <- paste("iso", "_", as.character(sav5$date[i]), "_", sep = "")
    output.file.name <- paste("iso", "_", as.character(sav5$date[i]), "_", sep = "")
    
    ProcTraj(lat = lat, lon = long, name = output.file.name,
             hour.interval = 1,
             met = KMetFiles, out = KOutFiles,
             hours = -96, height = 3000L, hy.path = KHySplitPath, ID = i, #24
             dates = dates,
             start.hour = "00:00", end.hour = "00:00") # 24:00
    
  }





iso <- lapply(dat[, "Station Code"], function(h) {
  sbs <- subset(mlt, mlt[, "Station Code"] == h)
  sbs <- sbs[complete.cases(sbs), ]
  
  stp <- sbs$`Date of Rain Collectors Installed`[1]
  stp <- as.Date(stp, "%d/%m/%Y")
  
  msr <- sbs$date
  
  # coordinates
  lat <- sbs$Latitude[1]; lon <- sbs$Longitude[1]
  
  lst <- foreach(i = 1:nrow(sbs), 
                 .packages = c("opentraj", "Rsenal", "latticeExtra")) %dopar% {
                   
                   dts <- if (i == 1) {
                     seq(stp, msr[i], "day")
                   } else {
                     seq(msr[i-1] + 1, msr[i], "day")
                   }
                   lbl <- paste(format(dts[1], "%m/%d/%Y"), "to", 
                                format(dts[length(dts)], "%m/%d/%Y"))
                   
                   ofl <- paste("iso", h, formatC(i-1, width = 2, flag = "0"), "", sep = "_")
                   
                   trj <- ProcTraj(lat = lat, lon = lon, name = ofl,
                                   hour.interval = 1,#96
                                   met = KMetFiles, out = KOutFiles,
                                   hours = -24, height = 3000L, hy.path = KHySplitPath, ID = i, #96
                                   dates = dts,
                                   start.hour = "00:00", end.hour = "00:00") # 24:00
                   
                   sln <- Df2SpLines(trj, crs = "+init=epsg:4326")
                   
                   spplot(sln, scales = list(draw = TRUE, cex = .7), 
                          xlim = c(35, 50), ylim = c(2, 14),
                          colorkey = FALSE, col.regions = "black", lwd = 2, 
                          sp.layout = list(rgb2spLayout(rgb, alpha = .5))) + 
                     layer(sp.points(spt[spt$`Station Code` == h, ], col = "black", pch = 24, 
                                     cex = 1.2, fill = "white"), data = list(spt = spt)) + 
                     layer(sp.text(c(42.5, 2.5), lbl, font = 1, cex = .7), 
                           data = list(lbl = lbl))
                 }
  
  img <- suppressWarnings(latticeCombineGrid(lst, layout = c(1, 3)))
  ofl <- paste0(rsv, "/data/bale/HYSPLIT/results/vis/iso_", h, ".png")
  
  png(ofl, width = 10, height = 15, units = "cm", res = 500)
  grid.newpage()
  print(img, newpage = FALSE)
  dev.off()
})



