library(ggplot2)
library(RColorBrewer)
#library(cowplot)
#library(grid)

# Check your working directory
wd <- setwd("C:/Users/iotte/Documents/Desktop/training/")

## Read data
df <- read.csv2("iso_calc.csv", header = T, sep = ";")

df$date_sample <- as.Date(df$date_sample)

sub = subset(df, df$plot_id_sp1 != "mnp1" &
               df$plot_id_sp1 != "mnp2")

summary(lm(dD_H ~ d18_16, data = sub), na.rm = TRUE)


##Preparation for plotting stuff
# color id for plotting
col.id <- c("#6a3d9a", "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#ff7f00",
            "#fdbf6f", "#b15928", "#e31a1c")

# sort legends
leg <- c("fer0", "fpd0", "fpo0", "foc0", "foc6", "flm1", "nkw1", "hom4", "sav5")


######
# plotting of raw values
lmwl.gmwl <- ggplot(sub, aes(x = d18_16, y = dD_H, color = plot_id_sp1, 
                            shape = type)) +
  geom_point(size = 2) +
  geom_abline(intercept = 13.62 , slope = 7.45 , linetype = 5) +
  geom_abline(intercept = 10, slope = 8, color = "#636363") +
  xlim(-10.585, 5.49) +
  ylim(-77.468, 41.615) +
  xlab( expression(delta^{18}*O ~ "(\u2030)")) +
  ylab( expression(delta*D ~ "(\u2030)")) +
  scale_shape_manual(values = c(15, 16, 17)) +
  #scale_shape_manual(values = c(0, 1, 2)) +
  scale_color_manual(values = col.id, name = "Plot ID SP1", limits = leg,
                     labels = c("Fer0", "Fpd0", "Fpo0", "Foc0", "Foc6", "Flm1",
                                "Nkw1", "Hom4", "Sav5")) + 
  theme(
    axis.text.x = element_text(color = "black", size = rel(2)),
    axis.text.y = element_text(color = "black", size = rel(2)),
    axis.title.x = element_text(color = "black", size = rel(2)),
    axis.title.y = element_text(color = "black", size = rel(2)),
    strip.text = element_text(face = "bold", size = rel(2)),
    strip.background = element_rect(color = "black", fill = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    legend.title = element_blank(),
    legend.text = element_text(size = rel(1.3)),
    legend.background = element_rect(color = "black", fill = "white"))

# print "lmwl.gmwl"
tiff("C:/Users/iotte/Desktop/HydrologicalProcesses/figures/LmwlGmwlRaw.tif", 
    width = 24, height = 20, units = "cm", res = 300, pointsize = 15)
print(lmwl.gmwl)
dev.off() 


######
######
## Build mean values for every plot and each precipitation type
## and add GMWL and corresponding LMWL

d18.16.mns <- aggregate(sub$d18_16, by = list(sub$plot_id_sp1, sub$type), 
                        FUN = "mean", na.rm = TRUE)
colnames(d18.16.mns) <- c("plot_id_sp1", "type", "d18_16_mn")

dD.H.mns <- aggregate(sub$dD_H, by = list(sub$plot_id_sp1, sub$type), 
                      FUN = "mean", na.rm = TRUE)
colnames(dD.H.mns) <- c("plot_id_sp1", "type", "dD_H_mn")

iso.mns <- merge(d18.16.mns, dD.H.mns)

iso.mns.sm <- summary(lm(dD_H_mn ~ d18_16_mn, data = iso.mns))

# plotting of annual mean isotope values
lmwl.gmwl.ann <- ggplot(iso.mns, aes(x = d18_16_mn, y = dD_H_mn, 
                                     colour = plot_id_sp1, shape = type)) +
  geom_point(size = 2) +
  geom_abline(intercept = 9.72, slope = 6.25, linetype = 5) +
  geom_abline(intercept = 10, slope = 8, color = "#636363") +
  xlab( expression(delta^{18}*O ~ "(\u2030)")) +
  ylab( expression(delta*D ~ "(\u2030)")) +
    scale_shape_manual(values = c(15, 16, 17)) +
  scale_color_manual(values = col.id, limits = leg, name = "Plot ID SP1",
                     labels = c("Fer0", "Fpd0", "Fpo0", "Foc0", "Foc6", "Flm1",
                                "Nkw1", "Hom4", "Sav5")) + 
  theme(
    axis.text.x = element_text(color = "black", size = rel(1.5)),
    axis.text.y = element_text(color = "black", size = rel(1.5)),
    axis.title.x = element_text(color = "black", size = rel(1.5)),
    axis.title.y = element_text(color = "black", size = rel(1.5)),
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(color = "black", fill = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    legend.title = element_blank(),
    legend.text = element_text(size = rel(1.3)),
    legend.background = element_rect(color = "black", fill = "white"))

ggdraw(switch_axis_position(lmwl.gmwl.ann,
                              axis = 'y'))


# print "lmwl.gmwl"
tiff("C:/Users/iotte/Desktop/LmwlGmwlAnnTEST.tif", 
     width = 11.5, height = 8, units = "cm", res = 300, pointsize = 15)
print(lmwl.gmwl.ann)
dev.off() 

######
######
## RAINFALL

# regressions for rain
rn <- subset(sub, sub$type == "rain")
sm.rain <- summary(lm(dD_H ~ d18_16, data = rn))

fg <- subset(sub, sub$type == "fog")
sm.fog <- summary(lm(dD_H ~ d18_16, data = fg)) 

tf <- subset(sub, sub$type == "tf")
sm.tf <- summary(lm(dD_H ~ d18_16, data = subset(df, df$type == "tf")))


# plotting of annual mean isotope values
lmwl.gmwl.fog <- ggplot(tf, aes(x = d18_16, y = dD_H, color = plot_id_sp1, 
                                     shape = type)) +
  geom_point(size = 2) +
  #geom_abline(intercept = 13.09, slope = 7.38, linetype = 5) + #rain
  #geom_abline(intercept = 14.54, slope = 7.56, linetype = 5) + #fog
  geom_abline(intercept = 13.52, slope = 7.45, linetype = 5) + #tf
  geom_abline(intercept = 10, slope = 8, color = "#636363") +
  xlim(-10.585, 5.49) +
  ylim(-77.468, 41.615) +
  xlab( expression(delta^{18}*O ~ "(\u2030)")) +
  ylab( expression(delta*D ~ "(\u2030)")) +
  scale_shape_manual(values = 17) +
  scale_color_manual(values = col.id, limits = leg, name = "Plot ID SP1",
                     labels = c("Fer0", "Fpd0", "Fpo0", "Foc0", "Foc6", "Flm1",
                                "Nkw1", "Hom4", "Sav5")) + 
  theme(
    axis.text.x = element_text(color = "black", size = rel(2)),
    axis.text.y = element_text(color = "black", size = rel(2)),
    axis.title.x = element_text(color = "black", size = rel(2)),
    axis.title.y = element_text(color = "black", size = rel(2)),
    strip.text = element_text(face = "bold", size = rel(2)),
    strip.background = element_rect(color = "black", fill = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    legend.title = element_blank(),
    legend.text = element_text(size = rel(1.3)),
    legend.background = element_rect(color = "black", fill = "white"))


# print "lmwl.gmwl"
tiff("C:/Users/iotte/Desktop/HydrologicalProcesses/figures/LmwlGmwlTf.tif", 
     width = 24, height = 20, units = "cm", res = 300, pointsize = 15)
print(lmwl.gmwl.fog)
dev.off() 




######
######


