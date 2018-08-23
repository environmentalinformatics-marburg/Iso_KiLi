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

df$date_sample <- as.Date(df$date_sample)

##Preparation for plotting stuff

# color id for plotting
col.id <- c("#6a3d9a", "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#ff7f00",
            "#fdbf6f", "#b15928", "#e31a1c")

# sort legends
leg <- c("fer0", "fpd0", "fpo0", "foc0", "foc6", "flm1", "nkw1",
         "hom4", "sav5")

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
# to create graphik
df.mnth.amnt <- merge(df.amnt.mnth, df.mnth)


#rainfall
rn <- subset(df, df$type == "rain")
#rn <- subset(rn, rn$d18_16 != "NA")

rn.mnth <- subset(df.mnth.amnt, df.mnth.amnt$type == "rain")
#rn.mnth <- subset(rn.mnth, rn.mnth$d18_16 != "NaN")


#fog
fg <- subset(df, df$type == "fog")
fg <- subset(fg, fg$d18_16 != "NA")

fg.mnth <- subset(df.mnth.amnt, df.mnth.amnt$type == "fog")
fg.mnth <- subset(fg.mnth, fg.mnth$d18_16 != "NaN")


#throughfall
tf <- subset(df, df$type == "tf")
#tf <- subset(tf, tf$d18_16 != "NA")

tf.mnth <- subset(df.mnth.amnt, df.mnth.amnt$type == "tf")
#tf.mnth <- subset(tf.mnth, tf.mnth$d18_16 != "NaN")


##### sort facets according to elevation (not alphabetic order!)
f <- c("fer0", "fpd0", "fpo0", "foc0", "foc6", "flm1", "nkw1",
       "hom4", "sav5")

rn.mnth <- within(rn.mnth, plot_id_sp1 <- factor(plot_id_sp1, levels = f))
rn <- within(rn, plot_id_sp1 <- factor(plot_id_sp1, levels = f))

foc0 <- subset(rn, rn$plot_id_sp1 == "foc0")


tf <- ggplot() +
  geom_point(data = tf.mnth, aes(x = date, y = d_excess), color = "blue") +
  geom_point(data = tf.mnth, aes(x = date, y = d18_16), color = "red") +
  facet_wrap( ~ plot_id_sp1)


# print rainfall
png("out_sia/amount_d18_dex/tf/tf.png", 
    width = 20, height = 15, units = "cm", res = 300, pointsize = 17)
plot(tf) 
dev.off()


###### plotting of raw values
p1 <- ggplot(data = subset(rn.mnth, rn.mnth$plot_id_sp1 == "fer0"), 
             aes(x = date, y = amount_mm, color = factor(col))) +
  geom_bar(stat = "identity", fill = "lightgray", color = "black") +
  xlab("") +
  ylab("rainfall [mm]") +
  ylim(0, 1350) +
  facet_wrap( ~ plot_id_sp1, ncol = 1) +
  scale_x_discrete(labels = c("11", "02", "05", "08", "11", 
                              "02", "05", "08", "11"),
                     breaks = c("2012-11", "2013-02", "2013-05", "2013-08", 
                                "2013-11", "2014-02", "2014-05", "2014-08", 
                                "2014-11")) +
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


p2 <- ggplot(data = subset(rn, rn$plot_id_sp1 == "fer0"), 
             aes(x = yrmn, y = d18_16, color = factor (col))) +
  #geom_boxplot(data = subset(rn, rn$plot_id_sp1 == "sav5"), 
  #             aes(x = yrmn, y = d_excess), color = "blue") +
  geom_boxplot(data = subset(rn, rn$plot_id_sp1 == "fer0"), 
               aes(x = yrmn, y = d18_16), color = "red") +
  xlab("") +
  ylab( expression(delta^{18}*O ~ "\u2030")) +
  ylim(-8, 3) +
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
png("out_sia/amount_d18_dex/rain/iso_amnt_rn_boxplotted_fer0_nw_slm.png", 
    width = 8.95, height = 6.95, units = "cm", res = 300, pointsize = 17)
plot(g) 
dev.off()

## print throughfall
#png("out_sia/amount_d18_dex/tf/iso_amnt_tf_boxplotted_fer0.png", 
#    width = 9.425, height = 6.95, units = "cm", res = 300, pointsize = 17)
#plot(g) 
#dev.off()
