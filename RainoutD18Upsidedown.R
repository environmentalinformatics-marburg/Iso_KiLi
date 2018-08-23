library(ggplot2)
library(gtable)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(plyr)

# Check your working directory
wd <- setwd("C:/Users/iotte/Documents/Desktop/training/")

## Read data
df <- read.csv2("iso_calc.csv", header = T, sep = ";")

df <- subset(df, df$plot_id_sp1 != "mnp1" &
               df$plot_id_sp1 != "mnp2")

# build monthly mean values of d18_16 and dD_H
df$yrmn <- substr(df$date_sample, 1, 7)

df.mnth <- aggregate(cbind(df$d18_16, df$dD_H, df$d_excess), 
                     by = list(df$yrmn, df$plot_id_sp1, df$type, df$elevation),
                     FUN = "mean", na.rm = TRUE)

colnames(df.mnth) <- c("date", "plot_id_sp1", "type", "elevation",
                       "d18_16", "dD_H", "d_excess")


# build monthly sums of amount_mm
df.amnt.mnth <- aggregate(df$amount_mm, by = list(df$yrmn, df$plot_id_sp1,
                                                  df$type, df$elevation),
                          FUN = "sum", na.rm = TRUE)

colnames(df.amnt.mnth) <- c("date", "plot_id_sp1", "type", "elevation","amount_mm")

# merge monthly mean of d18-16 & dD_H and monthly sums of amount_mm
df.mnth.amnt <- merge(df.amnt.mnth, df.mnth)


#rainfall
rn <- subset(df, df$type == "rain")

rn.mnth <- subset(df.mnth.amnt, df.mnth.amnt$type == "rain")

levels(rn.mnth$plot_id_sp1)[levels(rn.mnth$plot_id_sp1) == "flm1"] <- "d)   Foc0   2,498 m a.s.l."

###### plotting of raw values
p1 <- ggplot(data = subset(rn.mnth, rn.mnth$plot_id_sp1 == "d)   Foc0   2,498 m a.s.l."), 
             aes(x = date, y = amount_mm, color = factor(col))) +
  geom_bar(stat = "identity", fill = "lightgray", color = "black") +
  xlab("months") +
  ylab("rainfall (mm)") +
  facet_wrap( ~ plot_id_sp1, ncol = 1) +
  scale_x_discrete(labels = c("11-2012", "02-2013", "05-2013", "08-2013", "11-2013", 
                              "02-2014", "05-2014", "08-2014", "11-2014"),
                     breaks = c("2012-11", "2013-02", "2013-05", "2013-08", 
                                "2013-11", "2014-02", "2014-05", "2014-08", 
                                "2014-11")) +
  scale_y_reverse() +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    strip.text = element_text(face = "bold", size = rel(1.1)),
    strip.background = element_rect(color = "black", fill = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    legend.title = element_blank(),
    legend.text = element_text(size = rel(.8)),
    legend.background = element_rect(color = "black", fill = "white"),
    plot.margin = unit(c(1, 2, 0.5, 0.5), 'cm'))


p2 <- ggplot(data = subset(rn.mnth, rn.mnth$plot_id_sp1 == "d)   Foc0   2,498 m a.s.l."), 
             aes(x = date, y = d18_16, color = factor (col))) +
  geom_point(data = subset(rn.mnth, rn.mnth$plot_id_sp1 == "d)   Foc0   2,498 m a.s.l."), 
             aes(x = date, y = d18_16), 
             color = "black", shape = 15, size = 3) +
  geom_point(data = subset(rn.mnth, rn.mnth$plot_id_sp1 == "d)   Foc0   2,498 m a.s.l."), 
             aes(x = date, y = d_excess), 
             color = "black", shape = 16, size = 3) +
  geom_line(data = subset(rn.mnth, rn.mnth$plot_id_sp1 == "d)   Foc0   2,498 m a.s.l."), 
            aes(x = date, y = d18_16, group = 1), 
            color = "black", size = 1) +
  geom_line(data = subset(rn.mnth, rn.mnth$plot_id_sp1 == "d)   Foc0   2,498 m a.s.l."), 
            aes(x = date, y = d_excess, group = 1), 
            color = "black", size = 1, lty = 2) +
  scale_y_continuous(breaks = c(-5, -4, -3, -2, -1, 0, 1, 2)) +
  xlab(" ") +
  ylab( expression(delta^{18}*O ~ "(\u2030) / d-excess (\u2030)")) +
  facet_wrap( ~ plot_id_sp1, ncol = 1) +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    strip.text = element_text(face = "bold", size = rel(1.1)),
    strip.background = element_rect(color = "black", fill = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    legend.title = element_blank(),
    legend.text = element_text(size = rel(.8)),
    legend.background = element_rect(color = "black", fill = "white"),
    plot.margin = unit(c(1, 2, 0.5, 0.5), 'cm'))

### create double y-scale
# extract gtable
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == "panel-1", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel-1")]], pp$t, 
                     pp$l, pp$b, pp$l)

# axis tweaks
ia <- which(g2$layout$name == "axis_l-1")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

# draw it
grid.draw(g)


# print rainfall
tiff("C:/Users/iotte/Desktop/HydrologicalProcesses/rainout_foc0.tif", 
    width = 20, height = 10, units = "cm", res = 300, pointsize = 17)
plot(g) 
dev.off()
