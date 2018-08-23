library(ggplot2)
library(RColorBrewer)
library(plyr)

# Check your working directory
wd <- setwd("C:/Users/iotte/Documents/Desktop/training/")

## Read data
df <- read.csv2("iso_calc.csv", header = T, sep = ";")
df$date_sample <- as.Date(df$date_sample)

df$year <- substr(df$date_sample, 1, 4)
df <- subset(df, df$plot_id_sp1 != "mnp1"&
               df$plot_id_sp1 != "mnp2")

df$ann <- ifelse(df$date_sample < "2013-12-01", 2013, 2014)


## aggregate yearly sums
df.annual <- aggregate(cbind(df$d18_16, df$dD_H), by = list(df$ann, df$plot_id_sp1, 
                                                            df$type, df$elevation,
                                                            df$season),
                     FUN = "mean", na.rm = TRUE)

colnames(df.annual) <- c("date", "plot_id_sp1", "type", "elevation", 
                         "season", "d18_16", "dD_H")

df.rn <- subset(df.annual, df.annual$type == "rain")



df.year <- aggregate(cbind(df$d18_16, df$dD_H), by = list(df$plot_id_sp1, 
                                                          df$type, df$elevation,
                                                          df$season),
                       FUN = "mean", na.rm = TRUE)

colnames(df.year) <- c("plot_id_sp1", "type", "elevation", 
                         "season", "d18_16", "dD_H")

df.year <- arrange(df.year, plot_id_sp1, type)

write.csv2(df.year, file = "df_year.csv")

df.year <- read.csv2("df_year.csv")

#df.rn <- subset(df.annual, df.annual$type == "rain")

# change the name of the factor levels
df.year$type <- as.factor(df.year$type)

levels(df.year$type)[levels(df.year$type) == "rain"] <- "rainfall"
levels(df.year$type)[levels(df.year$type) == "tf"] <- "throughfall"


##### sort facets according to elevation (not alphabetic order!)
f <- c("rainfall", "throughfall", "fog")

df.year <- within(df.year, type <- factor(type, levels = f))
#df.sub <- within(df.sub, type <- factor(type, levels = f))

#shape = points(c(16,17,15,3,7,8,11,13))


### plotting elevation lapse rate d18_16
iso.elvtn.d1816 <- ggplot(df.year, aes(x = elevation, y = d18_16, colour = season, 
                                       group = season, size = .75, shape = season)) +
  geom_point() +
  #geom_line(size = .8) +
  stat_smooth(data = df.year, method = lm, se = FALSE, size = .5) +
  facet_grid(. ~ type) +
  ylab( expression(delta^{18}*O ~ "\u2030")) +
  #ylab( expression(delta*D ~ "\u2030")) +
  xlab("Elevation (m a.s.l.)") +
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5)),
    strip.background = element_rect(color = "black", fill = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    legend.title = element_blank(),
    #legend.text = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = rel(1.3)),
    legend.key.size = unit(.65, "cm"),
    axis.text.x = element_text(color = "black", size = rel(1.5)),
    axis.text.y = element_text(color = "black", size = rel(1.5)),
    axis.title.x = element_text(color = "black", size = rel(1.5)),
    axis.title.y = element_text(color = "black", size = rel(1.5)),
    legend.background = element_rect(color = "black", fill = "white")) +
  labs(fill = "") +
  scale_shape_manual(values = c(17, 3, 15, 16, 12)) +
  scale_color_manual(values = c("black", "#ca0020", "#0571b0", "#f4a582", "#92c5de"),
                     labels = c("mean", #2012-2013 (dry)", 
                                "long dry season" , #"2012-2013 (wet)",
                                "long rains", #"2013-2014 (dry)",
                                "short dry season",
                                "short rains")) #"2013-2014 (wet)",
                              #"2012-2014"))


# print "iso.mns.mnth.elvtn.18O"
png("out_new/iso_elvtn_d1816_linear.png", width = 38, height = 12, units = "cm", 
    res = 800, pointsize = 15)
print(iso.elvtn.d1816)
dev.off()