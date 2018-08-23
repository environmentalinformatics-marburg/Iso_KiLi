library(rgdal)
library(caret)
library(Rsenal)
library(latticeExtra)
library(dplyr)
library(gridExtra)

path <- "/media/tims_ex/kilimanjaro_precip_sp1_vs_hemp"
setwd(path)

shp <- "/media/tims_ex/kilimanjaro_plots_dem/Plots_DEM_final/PlotPoles_ARC1960_mod_20140807_final.shp"
lyr <- ogrListLayers(shp)
plts <- readOGR(shp, lyr)
proj4string(plts) <- "+init=epsg:21037"
plts_amp <- subset(plts, PoleType == "AMP")

prcp_ann <- read.csv("envin-umr_ki_prcp_all-plots_aggregated-annually_v010.csv",
                     stringsAsFactors = FALSE)

prcp_mnthly <- read.csv("data/P_RT_NRT_0200_monthly_lt_average_sums.dat",
                      stringsAsFactors = FALSE)
prcp_mnthly_lst <- split(prcp_mnthly, prcp_mnthly$PlotId)
prcp_mnthly_lst_clean <- prcp_mnthly_lst[-c(3, 5, 7, 8, 9, 12, 13)]
prcp_mnthly_lst_clean$nkw1 <- prcp_mnthly_lst_clean$nkw1[1:12, ]

mnthly_wgts_all <- lapply(prcp_mnthly_lst_clean, function(i) {
  i$P_RT_NRT / sum(i$P_RT_NRT)
})

plts <- plts_amp@data[plts_amp$PlotID %in% names(mnthly_wgts_all), ]

evaluate <- FALSE
### cross validation for different models =================================
if (evaluate) {
  mthds <- c("rf", "gam", "bstTree", "cubist", "earth")
  
  result <- lapply(mthds, function(k) {
    
    ### method settings
    mthd <- k
    tunegr <- switch(mthd,
                     rf = expand.grid(.mtry = seq(1, 5, 1)),
                     gam = expand.grid(.method = c("GCV.Cp", "GACV.Cp", "REML", 
                                                   "P-REML", "ML", "P-ML"),
                                       .select = c(TRUE, FALSE)),
                     bstTree = expand.grid(.nu = c(0.1, 0.5, 0.8),
                                           .mstop = seq(5, 20, 5),
                                           .maxdepth = 1),
                     cubist = expand.grid(.committees = seq(20, 100, 20),
                                          .neighbors = seq(3, 9, 2)),
                     earth = expand.grid(.nprune = seq(1, 10, 1),
                                         .degree = seq(1, 3, 1)))
    
    ### prediction evaluation
    out_eval <- lapply(seq(mnthly_wgts_all), function(j) {
      
      mnthly_wgts <- mnthly_wgts_all[-j]
      plt <- names(mnthly_wgts_all)[j]
      
      cat("\nMethod: ", mthd, "predicting Plot: ", plt, "\n")
      
      pred <- do.call("rbind", lapply(seq(mnthly_wgts), function(i) {
        tmp <- data.frame(x = plts$X[plts$PlotID == names(mnthly_wgts)[i]],
                          y = plts$Y[plts$PlotID == names(mnthly_wgts)[i]],
                          z = plts$Z_DEM_HMP[plts$PlotID == names(mnthly_wgts)[i]],
                          map = prcp_ann$MAP[prcp_ann$PlotID == names(mnthly_wgts)[i]],
                          month = sprintf("%02.f", 1:12),
                          resp = mnthly_wgts[i])
        names(tmp) <- c("x", "y", "z", "map", "month", "resp")
        return(tmp)
      }))
      
      newdat <- data.frame(x = plts_amp$X[plts_amp$PlotID == plt],
                           y = plts_amp$Y[plts_amp$PlotID == plt],
                           z = plts_amp$Z_DEM_HMP[plts_amp$PlotID == plt],
                           map = prcp_ann$MAP[prcp_ann$PlotID == plt],
                           month = sprintf("%02.f", 1:12))
      names(newdat) <- c("x", "y", "z", "map", "month")
      
      fmla <- as.formula(paste("resp ~ x + y + z + map + month"))
      
      model <- train(fmla, data = pred, method = mthd,
                     tuneGrid = tunegr)
      print(model)
      
      fit <- predict(model, newdata = newdat)
      obs <- mnthly_wgts_all[[j]]
      
      fit <- fit / sum(fit)
      
      out_df <- data.frame(PlotID = plt,
                           pre = fit * prcp_ann$MAP[prcp_ann$PlotID == plt],
                           obs = prcp_mnthly_lst_clean[[j]]$P_RT_NRT[
                             prcp_mnthly_lst_clean[[j]]$PlotId == plt],
                           sum = sum(fit))
      
      return(out_df)
      
    })
    
    p <- lapply(seq(out_eval), function(i) {
      pred_p <- xyplot(pre ~ seq_along(pre), data = out_eval[[i]],
                       type = "l", col = "red2", lwd = 2,
                       ylim = c(-10, 690), main = mthd,
                       panel = function(x, y, ...) {
                         panel.xyplot(x, y, ...)
                         panel.text(x = 1, y = 550, 
                                    labels = names(mnthly_wgts_all)[i])
                       })
      obs_p <- xyplot(obs ~ seq(12), data = out_eval[[i]],
                      type = "l", col = "black", lwd = 2, lty = 3)
      
      out_p <- pred_p + as.layer(obs_p)
    })
    
    p_final <- latticeCombineGrid(p, layout = c(3, 3))
    print(p_final)
    
    df <- do.call("rbind", out_eval)
    df$method <- mthd
    
    stats <- df %>%
      summarise(Method = mthd,
                ME = mean(pre - obs, na.rm = TRUE),
                MAE = mean(abs(pre - obs), na.rm = TRUE),
                RMSE = sqrt(mean((pre - obs)^2, na.rm = TRUE)),
                R = cor(pre, obs, use = "complete.obs"),
                Rsq = R * R)
    
    return(stats)
  })  
  
  stats <- do.call("rbind", result)
  stats
}
### =======================================================================



### final prediction (no subsetting) ======================================
pred <- do.call("rbind", lapply(seq(mnthly_wgts_all), function(i) {
  tmp <- data.frame(x = plts$X[plts$PlotID == names(mnthly_wgts_all)[i]],
                    y = plts$Y[plts$PlotID == names(mnthly_wgts_all)[i]],
                    z = plts$Z_DEM_HMP[plts$PlotID == names(mnthly_wgts_all)[i]],
                    map = prcp_ann$MAP[prcp_ann$PlotID == names(mnthly_wgts_all)[i]],
                    month = sprintf("%02.f", 1:12),
                    resp = mnthly_wgts_all[i])
  names(tmp) <- c("x", "y", "z", "map", "month", "resp")
  return(tmp)
}))

newdat <- do.call("rbind", lapply(seq(nrow(prcp_ann)), function(i) {
  tmp <- data.frame(x = plts_amp$X[plts_amp$PlotID == prcp_ann$PlotID[i]],
                    y = plts_amp$Y[plts_amp$PlotID == prcp_ann$PlotID[i]],
                    z = plts_amp$Z_DEM_HMP[plts_amp$PlotID == prcp_ann$PlotID[i]],
                    map = prcp_ann$MAP[i],
                    month = sprintf("%02.f", 1:12))
  names(tmp) <- c("x", "y", "z", "map", "month")
  return(tmp)
}))

fmla <- as.formula(paste("resp ~ x + y + z + map + month"))

model <- train(fmla, data = pred, method = "cubist")

fit <- predict(model, newdata = newdat)
#fit <- fit / sum(fit)

prcp_mnthly_fit <- data.frame(PlotID = rep(prcp_ann$PlotID, each = 12),
                              Month = sprintf("%02.f", 1:12),
                              wghts = fit,
                              MAP = rep(prcp_ann$MAP, each = 12))
prcp_mnthly_fit$MMP <- prcp_mnthly_fit$MAP * prcp_mnthly_fit$wghts

df_out <- data.frame(PlotID = prcp_mnthly_fit$PlotID,
                     Month = as.numeric(prcp_mnthly_fit$Month),
                     MMP = prcp_mnthly_fit$MMP,
                     Source = "M")

vers <- "v011"

out_nm <- paste("envin-umr_ki_prcp_all-plots_aggregated-monthly_",
                vers, ".csv", sep = "") 
write.csv(df_out, paste("../kilimanjaro_release_data/points/", 
                         out_nm, sep = ""), row.names = FALSE)


### plots
p_all <- lapply(names(mnthly_wgts_all), function(i) {
  pre <- prcp_mnthly_fit$MMP[prcp_mnthly_fit$PlotID == i]
  sum_pre <- round(sum(prcp_mnthly_fit$MMP[prcp_mnthly_fit$PlotID == i]), 0)
  pred_p <- xyplot(pre ~ seq_along(pre), asp = 1,
                   type = "l", col = "#3182bd", lwd = 2,
                   ylim = c(-10, 690), 
                   xlab = "Month", ylab = "Precipitation [mm]",
                   yscale.components = yscale.components.subticks,
                   panel = function(x, y, ...) {
                     panel.xyplot(x, y, ...)
                     panel.text(x = 6, y = 640, 
                                labels = paste(i, ": ", sum_pre, 
                                               " mm", sep = ""))
                   })
  
  obs <- prcp_mnthly_lst_clean[[i]]$P_RT_NRT
  obs_p <- xyplot(obs ~ seq(12), asp = 1,
                  type = "l", col = "black", lwd = 2, lty = 3)
  
  out_p <- pred_p + as.layer(obs_p)
})

p_final_all <- latticeCombineGrid(p_all, layout = c(3, 3))

png("monthly_precip_eval.png", 
    width = 20, height = 20, units = "cm", res = 300)
grid.newpage()
print(p_final_all)

downViewport(trellis.vpname(name = "figure"))
#grid.rect()
vp1 <- viewport(x = 1, y = 0.7, 
                height = 0.05, width = 0.33,
                just = c("right", "bottom"),
                name = "legend.vp")

pushViewport(vp1)

draw.key(key = list(col = c("black", "#3182bd"),
                    lines = list(lty = c(3, 1),
                                 lwd = 2),
                    text = list(c("observed", "predicted"), 
                                col = "black")), 
         draw = TRUE)
upViewport(0)
dev.off()
### =======================================================================


### all plots visualisation ===============================================
the60 <- c(paste(rep("cof", 5), 1:5, sep = ""),
           paste(rep("fer", 5), 0:4, sep = ""),
           paste(rep("flm", 5), c(1:4, 6), sep = ""),
           paste(rep("foc", 5), 1:5, sep = ""),
           paste(rep("fod", 5), 1:5, sep = ""),
           paste(rep("fpd", 5), 1:5, sep = ""),
           paste(rep("fpo", 5), 1:5, sep = ""),
           paste(rep("gra", 5), 1:5, sep = ""),
           paste(rep("hel", 5), 1:5, sep = ""),
           paste(rep("hom", 5), 1:5, sep = ""),
           paste(rep("mai", 5), 1:5, sep = ""),
           paste(rep("sav", 5), 1:5, sep = ""))

the12 <- c("cof", "fer", "flm", "foc", "fod", "fpd", 
           "fpo", "gra", "hel", "hom", "mai", "sav")

ele_habs <- plts_amp@data %>%
  group_by(substr(plts_amp$PlotID, 1, 3)) %>%
  summarise(Elevation = mean(Z_DEM_HMP, na.rm = TRUE))
names(ele_habs) <- c("Habitat", "Elevation")
ele_habs <- ele_habs[order(ele_habs$Elevation), ]
ele_habs <- ele_habs[ele_habs$Habitat %in% the12, ]

official_plots <- prcp_mnthly_fit[prcp_mnthly_fit$PlotID %in% the60, ]
official_plots$Habitat <- substr(official_plots$PlotID, 1, 3)
official_plots$Habitat <- factor(official_plots$Habitat,
                                 levels = rev(ele_habs$Habitat))
official_plots <- official_plots[order(official_plots$Habitat), ]

ta200 <- read.csv("../kilimanjaro_release_data/points/envin-umr_ki_ta_all-plots_aggregated-monthly_v010.csv")
ta200 <- ta200[ta200$PlotID %in% the60, ]
ta200 <- ta200[, -c(2, 4)]
ta200$Habitat <- substr(ta200$PlotID, 1, 3)
ta200$Habitat <- factor(ta200$Habitat,
                                 levels = rev(ele_habs$Habitat))
ta200 <- ta200[order(ta200$Habitat), ]

official_plots$MMT <- ta200$MMT

p_all <- lapply(unique(official_plots$PlotID), function(i) {
  pre <- official_plots$MMP[official_plots$PlotID == i]
  sum_pre <- round(sum(official_plots$MMP[official_plots$PlotID == i]), 0)
  elev <- round(plts_amp$Z_DEM_HMP[plts_amp$PlotID == i], 0)
  pred_p <- xyplot(pre ~ seq_along(pre), asp = 0.65,
                   type = "l", col = "#3182bd", lwd = 3,
                   ylim = c(-10, 100), as.table = TRUE,
                   xlab = "Month", ylab = "Precipitation [mm]",
                   yscale.components = yscale.components.subticks,
                   panel = function(x, y, ...) {
                     panel.xyplot(x, y, ...)
                     panel.text(x = 7, y = 640, 
                                labels = paste(i, ": ", sum_pre, 
                                               " mm", sep = ""))
                     panel.text(x = 7, y = 550, 
                                labels = paste("elevation", ": ", elev, 
                                               " m", sep = ""))
                   })
})
#   obs <- prcp_mnthly_lst_clean[[i]]$P_RT_NRT
#   obs_p <- xyplot(obs ~ seq(12), asp = 1,
#                   type = "l", col = "black", lwd = 2, lty = 3)
#   
#   out_p <- pred_p + as.layer(obs_p)
# })

t_all <- lapply(unique(official_plots$PlotID), function(i) {
  ta <- official_plots$MMT[official_plots$PlotID == i]
  pred_t <- xyplot(ta ~ seq_along(ta), asp = 0.65,
                   type = "l", col = "#3182bd", lwd = 3,
                   ylim = c(-10/2, 100/2), as.table = TRUE,
                   xlab = "Month", ylab = "Temperature [°C]",
                   yscale.components = yscale.components.subticks)
})

p_t_all <- lapply(seq(t_all), function(i) {
  out <- doubleYScale(p_all[[i]], t_all[[i]])

p_final_all <- latticeCombineGrid(t_all, layout = c(5, 12))

png("monthly_precip_60plots.png", 
    width = 40, height = 60, units = "cm", res = 300)
grid.newpage()
print(p_final_all)
dev.off()

# ### test other plots
# plt <- "fod4"
# pre <- prcp_mnthly_fit$MMP[prcp_mnthly_fit$PlotID == plt]
# sum_pre <- round(sum(prcp_mnthly_fit$MMP[prcp_mnthly_fit$PlotID == plt]), 0)
# pred_p <- xyplot(pre ~ seq_along(pre), 
#                  type = "l", col = "red2", lwd = 2,
#                  ylim = c(-10, 690), 
#                  panel = function(x, y, ...) {
#                    panel.xyplot(x, y, ...)
#                    panel.text(x = 1, y = 550, 
#                               labels = paste(plt, ":", sum_pre, sep = " "))
#                  })
# 
# pred_p

# plot(prcp_mnthly_fit$MMP[prcp_mnthly_fit$PlotID == "nkw1"], type = "l")
# lines(prcp_mnthly_lst$nkw1$P_RT_NRT, col = "red")


# prcp_mnthly <- as.data.frame(do.call("rbind", 
#                                      lapply(seq(mnthly_wgts), function(i) {
#   mnthly_wgts[[i]] * prcp_ann$MAP[i]
# })))
# 
# prcp_mnthly$PlotID <- prcp_ann$PlotID
# names(prcp_mnthly) <- c(sprintf("%02.f", 1:12), "PlotID")
# 
# prcp_mnthly_obs <- read.csv("data/P_RT_NRT_0200_monthly_lt_average_sums.dat",
#                             stringsAsFactors = FALSE)
# 
# 
