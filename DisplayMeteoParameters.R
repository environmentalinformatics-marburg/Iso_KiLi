library(ggplot2)
library(RColorBrewer)
library(plyr)

# Check your working directory
wd <- setwd("C:/Users/iotte/Desktop/HydrologicalProcesses/")


## Read data
iso <- read.csv2("iso_mnth_amnt.csv", header = T, sep = ";")
iso.rn <- subset(iso, iso$type == "rain")
iso.tf<- subset(iso, iso$type == "tf")
iso.fg <- subset(iso, iso$type =="fog")
###
## plot humidity vs O18
summary(lm(dD_H ~ Ta_200, data = iso.rn), na.rm = TRUE)

iso.rh <- subset(iso, iso$plot_id_sp1 != "foc0" &
                   iso$plot_id_sp1 != "foc6")

### plots umbenennen
levels(iso.rh$plot_id_sp1)[levels(iso.rh$plot_id_sp1) == "fer0"] <- "a)   Fer0   3,880 m a.s.l."
levels(iso.rh$plot_id_sp1)[levels(iso.rh$plot_id_sp1) == "fpd0"] <- "b)   Fpd0   2,990 m a.s.l."
levels(iso.rh$plot_id_sp1)[levels(iso.rh$plot_id_sp1) == "fpo0"] <- "c)   Fpo0   2,850 m a.s.l."
levels(iso.rh$plot_id_sp1)[levels(iso.rh$plot_id_sp1) == "flm1"] <- "d)   Flm1   1,920 m a.s.l."
levels(iso.rh$plot_id_sp1)[levels(iso.rh$plot_id_sp1) == "nkw1"] <- "e)   Nkw1   1,800 m a.s.l."
levels(iso.rh$plot_id_sp1)[levels(iso.rh$plot_id_sp1) == "hom4"] <- "e)   Hom4   1,260 m a.s.l."
levels(iso.rh$plot_id_sp1)[levels(iso.rh$plot_id_sp1) == "sav5"] <- "f)   Sav5   950 m a.s.l."


### sort facets according to elevation (not alphabetic order!)
f <- c("a)   Fer0   3,880 m a.s.l.", 
       "b)   Fpd0   2,990 m a.s.l.", 
       "c)   Fpo0   2,850 m a.s.l.", 
       "d)   Flm1   1,920 m a.s.l.", 
       "e)   Nkw1   1,800 m a.s.l.",
       "e)   Hom4   1,260 m a.s.l.", 
       "f)   Sav5   950 m a.s.l.")

iso.rh <- within(iso.rh, plot_id_sp1 <- factor(plot_id_sp1, levels = f))


### automatische regression
iso.rh.rn <- subset(iso.rh, iso.rh$type == "rain")

lm_labels <- function(iso.rh.rn) {
  mod <- lm(d18_16 ~ rH_200, data = iso.rh.rn)
  formula <- sprintf("italic(y) == %.2f %+.4f * italic(x)",
                     round(coef(mod)[1], 4), round(coef(mod)[2], digits = 4))
  
  r <- cor(iso.rh.rn$rH_200, iso.rh.rn$d18_16)
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r2 = r2, stringsAsFactors = FALSE)
}

labels <- ddply(iso.rh.rn, "plot_id_sp1", lm_labels)

labels$r2[1] <- "italic(R^2) == 0.32"
labels$r2[3] <- "italic(R^2) == 0.27"
labels$r2[4] <- "italic(R^2) == 0.09"
labels$r2[6] <- "italic(R^2) == 0.17"

summary(lm(d18_16 ~ rH_200, data = subset(iso.rh.rn, iso.rh.rn$elevation == 1800)))
labels



# plot
iso.rh.pl <- ggplot(data = iso.rh.rn, 
             aes(x = rH_200, y = d18_16, shape = plot_id_sp1)) +
  geom_point(size = 2) +
  stat_smooth(method = lm, se = FALSE, color = "black") +
  facet_wrap( ~ plot_id_sp1) +
  xlab("relative humidity (%)") +
  ylab( expression(delta^{18}*O ~ "\u2030")) +
  xlim(50, 100) +
  geom_text(x = 48, y = -4.2, aes(label = formula), data = labels, parse = TRUE, 
            hjust = 0, position = "identity", size = 2.5) +
  geom_text(x = 36, y = -5.1, aes(label = r2), data = labels, parse = TRUE, 
            hjust = -.95, size = 2.5) +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(color = "black", fill = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    legend.title = element_blank(),
    legend.text = element_blank(),
    legend.background = element_blank(),
    plot.margin = unit(c(1, 2, 0.5, 0.5), 'cm'))

# print
tiff("figures/rH200_18O_rain.tif", 
     width = 20, height = 10, units = "cm", res = 300, pointsize = 17)
plot(iso.rh.pl) 
dev.off()

###
## plot temperature vs O18

iso.ta <- subset(iso, iso$plot_id_sp1 != "foc0" &
                   iso$plot_id_sp1 != "nkw1")

### plots umbenennen
levels(iso.ta$plot_id_sp1)[levels(iso.ta$plot_id_sp1) == "fer0"] <- "a)   Fer0   3,880 m a.s.l."
levels(iso.ta$plot_id_sp1)[levels(iso.ta$plot_id_sp1) == "fpd0"] <- "b)   Fpd0   2,990 m a.s.l."
levels(iso.ta$plot_id_sp1)[levels(iso.ta$plot_id_sp1) == "fpo0"] <- "c)   Fpo0   2,850 m a.s.l."
levels(iso.ta$plot_id_sp1)[levels(iso.ta$plot_id_sp1) == "foc6"] <- "d)   Foc6   2,120 m a.s.l."
levels(iso.ta$plot_id_sp1)[levels(iso.ta$plot_id_sp1) == "flm1"] <- "e)   Flm1   1,920 m a.s.l."
levels(iso.ta$plot_id_sp1)[levels(iso.ta$plot_id_sp1) == "hom4"] <- "f)   Hom4   1,260 m a.s.l."
levels(iso.ta$plot_id_sp1)[levels(iso.ta$plot_id_sp1) == "sav5"] <- "g)   Sav5   950 m a.s.l."


### sort facets according to elevation (not alphabetic order!)
f <- c("a)   Fer0   3,880 m a.s.l.", 
       "b)   Fpd0   2,990 m a.s.l.", 
       "c)   Fpo0   2,850 m a.s.l.", 
       "d)   Foc6   2,120 m a.s.l.", 
       "e)   Flm1   1,920 m a.s.l.", 
       "f)   Hom4   1,260 m a.s.l.", 
       "g)   Sav5   950 m a.s.l.")

iso.ta <- within(iso.ta, plot_id_sp1 <- factor(plot_id_sp1, levels = f))


### automatische regression
iso.ta.rn <- subset(iso.ta, iso.ta$type == "rain")

lm_labels <- function(iso.ta.rn) {
  mod <- lm(d18_16 ~ Ta_200, data = iso.ta.rn)
  formula <- sprintf("italic(y) == %.2f %+.4f * italic(x)",
                     round(coef(mod)[1], 4), round(coef(mod)[2], digits = 4))
  
  r <- cor(iso.ta.rn$Ta_200, iso.ta.rn$d18_16)
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r2 = r2, stringsAsFactors = FALSE)
}

labels <- ddply(iso.ta.rn, "plot_id_sp1", lm_labels)

labels$r2[1] <- "italic(R^2) == 0.001"
#labels$r2[2] <- "italic(R^2) == 0.19"
labels$r2[3] <- "italic(R^2) == 0.05"
labels$r2[4] <- "italic(R^2) == 0.12"
labels$r2[5] <- "italic(R^2) == 0.03"
labels$r2[7] <- "italic(R^2) == 0.08"

summary(lm(d18_16 ~ Ta_200, data = subset(iso.ta.rn, iso.ta.rn$elevation == 3880)))

labels

lm_labels <- function(iso.rn) {
  mod <- lm(d18_16 ~ Ta_200, data = iso.rn)
  formula <- sprintf("italic(y) == %.2f %+.4f * italic(x)",
                     round(coef(mod)[1], 4), round(coef(mod)[2], digits = 4))
  
  r <- cor(iso.rn$Ta_200, iso.rn$d18_16, use = "pairwise.complete.obs")
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r2 = r2, stringsAsFactors = FALSE)
}

labels <- ddply(iso.rn, "type", lm_labels)

# sort legends
leg <- c("fer0", "fpd0", "fpo0", "foc0", "foc6", "flm1", "hom4", "sav5")

# plot
iso.ta.pl <- ggplot(data = iso.rn, #.ta.rn, 
                 aes(x = Ta_200, y = d18_16)) + #, shape = plot_id_sp1)) +
  geom_point(size = 2) +
  #stat_smooth(method = lm, se = FALSE, color = "black") +
  #facet_wrap( ~ plot_id_sp1, nrow = 2) +
  geom_abline(slope = 0.16, intercept = -5.10) +
  xlab("air temperature (??C)") +
  ylab( expression(delta^{18}*O ~ "(\u2030)")) +
  scale_shape_manual(values = c(16, 17, 15, 3, 13, 8, 11, 5), name = "Plot ID SP1", 
                     limits = leg,
                     labels = c("Fer0", "Fpd0", "Fpo0", "Foc0", "Foc6", "Flm1",
                                "Hom4", "Sav5")) +
  #xlim(4, 27) +
  geom_text(x = 20, y = -6.2, aes(label = formula), data = labels, parse = TRUE,  
            hjust = 0, position = "identity", size = 4) +
  geom_text(x = 20, y = -7.1, aes(label = r2), data = labels, parse = TRUE,  
            hjust = -.95, size = 4) +
  theme(
    axis.text.x = element_text(color = "black", size = rel(1.4)),
    axis.text.y = element_text(color = "black", size = rel(1.4)),
    axis.title.x = element_text(color = "black", size = rel(1.4)),
    axis.title.y = element_text(color = "black", size = rel(1.4)),
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(color = "black", fill = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    legend.title = element_blank(),
    legend.text = element_text(size = rel(1)),
    legend.background = element_rect(color = "black", fill = "white"),
    plot.margin = unit(c(1, 2, 0.5, 0.5), 'cm'))
  
# print
tiff("figures/Ta200_18O_rain_all_lable1.tif", 
     width = 21, height = 10, units = "cm", res = 300, pointsize = 17)
plot(iso.ta.pl) 
dev.off()


###
## plot amountvs O18

iso.am <- iso


### plots umbenennen
levels(iso.am$plot_id_sp1)[levels(iso.am$plot_id_sp1) == "fer0"] <- "a)   Fer0   3,880 m a.s.l."
levels(iso.am$plot_id_sp1)[levels(iso.am$plot_id_sp1) == "fpd0"] <- "b)   Fpd0   2,990 m a.s.l."
levels(iso.am$plot_id_sp1)[levels(iso.am$plot_id_sp1) == "fpo0"] <- "c)   Fpo0   2,850 m a.s.l."
levels(iso.am$plot_id_sp1)[levels(iso.am$plot_id_sp1) == "foc0"] <- "d)   Foc0   2,498 m a.s.l."
levels(iso.am$plot_id_sp1)[levels(iso.am$plot_id_sp1) == "foc6"] <- "e)   Foc6   2,120 m a.s.l."
levels(iso.am$plot_id_sp1)[levels(iso.am$plot_id_sp1) == "flm1"] <- "f)   Flm1   1,920 m a.s.l."
levels(iso.am$plot_id_sp1)[levels(iso.am$plot_id_sp1) == "nkw1"] <- "g)   Nkw1   1,800 m a.s.l."
#levels(iso.am$plot_id_sp1)[levels(iso.am$plot_id_sp1) == "hom4"] <- "g)   Hom4   1,260 m a.s.l."
#levels(iso.am$plot_id_sp1)[levels(iso.am$plot_id_sp1) == "sav5"] <- "h)   Sav5   950 m a.s.l."


### sort facets according to elevation (not alphabetic order!)
f <- c("a)   Fer0   3,880 m a.s.l.", 
       "b)   Fpd0   2,990 m a.s.l.", 
       "c)   Fpo0   2,850 m a.s.l.", 
       "d)   Foc0   2,498 m a.s.l.", 
       "e)   Foc6   2,120 m a.s.l.", 
       "f)   Flm1   1,920 m a.s.l.", 
       "g)   Nkw1   1,800 m a.s.l.") #, 
      # "g)   Hom4   1,260 m a.s.l.", 
      # "h)   Sav5   950 m a.s.l.")

iso.am <- within(iso.am, plot_id_sp1 <- factor(plot_id_sp1, levels = f))


### automatische regression
iso.am.rn <- subset(iso.am, iso.am$type == "fog")

lm_labels <- function(iso.am.rn) {
  mod <- lm(d18_16 ~ amount_mm, data = iso.am.rn)
  formula <- sprintf("italic(y) == %.2f %+.4f * italic(x)",
                     round(coef(mod)[1], 4), round(coef(mod)[2], digits = 4))
  
  r <- cor(iso.am.rn$amount_mm, iso.am.rn$d18_16, use = "pairwise.complete.obs")
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r2 = r2, stringsAsFactors = FALSE)
}

labels <- ddply(iso.am.rn, "plot_id_sp1", lm_labels)
labels


# plot
iso.am.pl <- ggplot(data = iso.am.rn, 
                    aes(x = amount_mm, y = d18_16, shape = plot_id_sp1)) +
  geom_point(size = 2) +
  stat_smooth(method = lm, se = FALSE, color = "black") +
  facet_wrap( ~ plot_id_sp1, nrow = 2) +
  xlab("fog (mm)") +
  ylab( expression(delta^{18}*O ~ "(\u2030)")) +
  scale_shape_manual(values = c(16,17,15,3,13,8,2)) +#11,5)) +
  ylim(-9.9, 1.5) +
  geom_text(x = 14, y = .5, aes(label = formula), data = labels, parse = TRUE,  #fog x= 14, y=.5
            hjust = 0, position = "identity", size = 2.5) +
  geom_text(x = 15, y = -1, aes(label = r2), data = labels, parse = TRUE, #fog x=15, y=-1
            hjust = -.95, size = 2.5) +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(color = "black", fill = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    legend.title = element_blank(),
    legend.text = element_blank(),
    legend.background = element_blank(),
    plot.margin = unit(c(1, 2, 0.5, 0.5), 'cm'))

# print
tiff("figures/Amount_18O_fog_ud.tif", 
     width = 25, height = 10, units = "cm", res = 300, pointsize = 17)
plot(iso.am.pl) 
dev.off()



  