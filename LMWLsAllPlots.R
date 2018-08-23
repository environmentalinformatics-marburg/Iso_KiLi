library(ggplot2)
library(RColorBrewer)
library(plyr)

# Check your working directory
wd <- setwd("C:/Users/iotte/Documents/Desktop/training/")

## Read data
iso <- read.csv2("iso_calc.csv", header = T, sep = ";")

iso <- subset(iso, iso$plot_id_sp1 != "mnp1" &
                iso$plot_id_sp1 != "mnp2")

###
## plot humidity vs O18
summary(lm(dD_H ~ d18_16, data = iso), na.rm = TRUE)


### plots umbenennen
levels(iso$plot_id_sp1)[levels(iso$plot_id_sp1) == "fer0"] <- "a)   Fer0   3,880 m a.s.l."
levels(iso$plot_id_sp1)[levels(iso$plot_id_sp1) == "fpd0"] <- "b)   Fpd0   2,990 m a.s.l."
levels(iso$plot_id_sp1)[levels(iso$plot_id_sp1) == "fpo0"] <- "c)   Fpo0   2,850 m a.s.l."
levels(iso$plot_id_sp1)[levels(iso$plot_id_sp1) == "foc0"] <- "d)   Foc0   2,498 m a.s.l."
levels(iso$plot_id_sp1)[levels(iso$plot_id_sp1) == "foc6"] <- "e)   Foc6   2,120 m a.s.l."
levels(iso$plot_id_sp1)[levels(iso$plot_id_sp1) == "flm1"] <- "f)   Flm1   1,920 m a.s.l."
levels(iso$plot_id_sp1)[levels(iso$plot_id_sp1) == "nkw1"] <- "g)   Nkw1   1,800 m a.s.l."
#levels(iso$plot_id_sp1)[levels(iso$plot_id_sp1) == "hom4"] <- "g)   Hom4   1,260 m a.s.l."
#levels(iso$plot_id_sp1)[levels(iso$plot_id_sp1) == "sav5"] <- "h)   Sav5   950 m a.s.l."


### sort facets according to elevation (not alphabetic order!)
f <- c("a)   Fer0   3,880 m a.s.l.", 
       "b)   Fpd0   2,990 m a.s.l.", 
       "c)   Fpo0   2,850 m a.s.l.", 
       "d)   Foc0   2,498 m a.s.l.",
       "e)   Foc6   2,120 m a.s.l.",
       "f)   Flm1   1,920 m a.s.l.", 
       "g)   Nkw1   1,800 m a.s.l.") #,
       #"g)   Hom4   1,260 m a.s.l.", 
       #"h)   Sav5   950 m a.s.l.")

iso <- within(iso, plot_id_sp1 <- factor(plot_id_sp1, levels = f))


### automatische regression
rn <- subset(iso, iso$type == "fog")

lm_labels <- function(rn) {
  mod <- lm(dD_H ~ d18_16, data = rn)
  formula <- sprintf("italic(y) == %.2f %+.4f * italic(x)",
                     round(coef(mod)[1], 4), round(coef(mod)[2], digits = 4))
  
  r <- cor(rn$d18_16, rn$dD_H, use = "pairwise.complete.obs")
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula = formula, r2 = r2, stringsAsFactors = FALSE)
}

labels <- ddply(rn, c("plot_id_sp1", "type"), lm_labels)
labels



# plot
iso.pl <- ggplot(data = rn, 
                    aes(x = d18_16, y = dD_H)) +
  geom_point(size = 2, shape = 15) +
  stat_smooth(method = lm, se = FALSE, color = "black") +
  facet_wrap( ~ plot_id_sp1) +
  xlab( expression(delta^{18}*O ~ "(\u2030)")) +
  ylab( expression(delta*D ~ "(\u2030)")) +
  #xlim(50, 100) +
  geom_text(x = -9, y = 20, aes(label = formula), data = labels, parse = TRUE, 
            hjust = 0, position = "identity", size = 2.5) +
  geom_text(x = -11, y = 10, aes(label = r2), data = labels, parse = TRUE, 
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
tiff("C:/Users/iotte/Desktop/HydrologicalProcesses/figures/LmwlsPlotsFog.tif", 
     width = 20, height = 18, units = "cm", res = 300, pointsize = 17)
plot(iso.pl) 
dev.off()
