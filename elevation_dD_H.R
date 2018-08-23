#####################################
library(ggplot2)

# Check your working directory
wd <- setwd("C:/Users/iotte/Documents/Desktop/training/")
############### plotting

df <- read.csv("elevation_dD_H.csv", header = T, sep = ";")

ann.uw.dDH <- qplot(Elevation, all_dD_H_uw, data = df) +
  #stat_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black",
  #            fullrange = TRUE) +
  stat_smooth(method = "lm", formula = y ~ log(x), se = FALSE, color = "black",
              fullrange = TRUE) + 
  # fullrange: extrapolierte Regressionslinie pro facet_grid (aber nach gleicher formel)
  # facet_grid(. ~ type) +
  annotate("text", label = eqn, parse = TRUE, x = 2300, y = -1.5, hjust = -1.1, vjust = -.5)

coef.fog <- coef(lm(all_d18_16_uw ~ log(Elevation), data = df.fog))
summary(lm(all_d18_16_uw ~ log(Elevation), data = df.fog))

# add regression function and r?? to scatterplot
model <- lm(all_d18_16_uw ~ log(Elevation), df.fog)
eqn <- as.character(as.expression(
  #substitute(italic(y) == a + b * log(x) * "," ~~ italic(r)^2 ~ "=" ~ r2,
  substitute(italic(y) == b * log(x) + a *"," ~~ italic(r)^2 ~ "=" ~ r2,
             list(a = format(coef(model)[1], digits = 3),
                  b = format(coef(model)[2], digits = 3),
                  r2 = format(summary(model)$r.squared, digits = 2)
             ))))
parse(text = eqn)

###### tomorrow
# facet_grid f??r rain, tf, fog
# lineare und log Regressionsgerade legen
# jeweils dazugeh??rige Gleichung dazu.

# ungewichtet all
# ungewichtet 1213
# gewichtet 1213 aber:bislang ohne Nebelkorrektur
# ungewichtet 1314
# gewichtet 1314

##### all again for D

summary(lm(all_dD_H_uw ~ Elevation, data = df))
df.fog <- subset(df, df$type == "fog")
summary(lm(all_dD_H_uw ~ Elevation, data = df.fog))

x1213.uw.dDH <- qplot(Elevation, X1213_dD_H_uw, data = df) +
  geom_abline(intercept = 0.1155418, slope = -0.0016138) +
  facet_grid(.~type)
summary(lm(X1213_dD_H_uw ~ Elevation, data = rain))

x1213.w.dDH <- qplot(Elevation, X1213_dD_H_w, data = df) +
  geom_abline(intercept = -2.162234, slope = -0.001145) +
  facet_grid(.~type)
summary(lm(X1213_dD_H_w ~ Elevation, data = rain))

x1314.uw.dDH <- qplot(Elevation, X1314_dD_H_uw, data = df) +
  geom_abline(intercept = -0.4612316, slope = -0.0010992) +
  facet_grid(.~type)
summary(lm(X1314_dD_H_uw ~ Elevation, data = rain))

x1314.w.dDH <- qplot(Elevation, X1314_dD_H_w, data = df) +
  geom_abline(intercept = -1.2060784, slope = -0.0010700) +
  facet_grid(.~type)
summary(lm(X1314_dD_H_w ~ Elevation, data = rain))

################################################################

iso.mns.mnth.elvtn.18O <- ggplot(rain, 
                                 aes(x = Elevation, y = all_d18_16_uw)) + 
  scale_color_manual(values = "Station") + 
  ylab( expression(delta^{18}*O ~ "\u2030")) +
  xlab("elevation m a.s.l.") +
  scale_x_continuous(breaks = rain$Elevation) +
  theme(
    panel.grid.major = element_line(color = "lightgray", size = 0.01),
    panel.background = element_rect(fill = NA),
    panel.border = element_rect(color = "gray", fill = NA))


# print "iso.mns.mnth.elvtn.18O"
png("out/iso.mns.mnth.elvtn.18O.png", width = 30, height = 20, units = "cm", 
    res = 300, pointsize = 15)
print(iso.mns.mnth.elvtn.18O)
dev.off()