# Check your working directory
setwd("C:/Users/iotte/Desktop/EGU")

lib <- c("zoo", "latticeExtra", "grid")
sapply(lib, function(x) require(x, character.only = TRUE))

data <- read.csv("data/amount_1314.csv", sep = ";", header = T)

### Plotting

# Colors
col.ns <- brewer.pal(9, "RdBu")[length(brewer.pal(9, "RdBu"))] # blue
col.tf <- brewer.pal(3, "BuGn")[3]
col.fg <- brewer.pal(9, "Paired")[7]

# Precipitation
y.lim <- c((floor(min(min(data$rainfall), min(data$tf_mean)) / 200)) * 200 - 50,
           (ceiling(max(max(data$rainfall), max(data$tf_mean))/ 200)) * 200 + 50)

y.scl <- seq((y.lim + 50)[1], (y.lim - 50)[2], 400)

xyplot.ns.tf <- 
  xyplot(rainfall ~ elevation, data = data,
         xlab = "Elevation [m]", 
         ylab = "Precipitation / Throughfall [mm]",
         xlim = c(ceiling(range(data$elevation)[2] / 200) * 200 + 50, 
                  floor(range(data$elevation)[1] / 200) * 200 - 50), 
         ylim = y.lim,
         scales = list(x = list(tck = c(1, 0), 
                                at = seq(round(min(data$elevation), digits = -2), 
                                         round(max(data$elevation), digits = -2), 200), rot = 90), 
                       y = list(at = y.scl, rot = 90)),
         panel = function(x, y, ...) {
           panel.abline(v = data$elevation, col = "gray", label = data$elevation)
           panel.xyplot(x, y, col = col.ns, type = c("p", "l"), lwd = 2, pch = 15)
         }) + 
  as.layer(xyplot(tf_mean ~ elevation, data = data, 
                  xlab = "Elevation [m]", ylab = "Throughfall [mm]", 
                  panel = function(x, y, ...) {
                    panel.xyplot(x, y, type = c("p", "l"), col = col.tf, lty = 2, lwd = 2, pch = 15)
                  }), y.same = TRUE)

# Throughfall
xyplot.fg <- xyplot(fog ~ elevation, data = data, 
                    xlab = "Elevation [m]", ylab = "Fog [mm]", 
                    panel = function(x, y, ...) {
                      panel.xyplot(x, y, type = c("p", "l"), col = col.fg, 
                                   lty = 2, lwd = 2, pch = 15)
                    }, 
                    scales = list(y = list(rot = 90)))

# Labels for right y axis
plot.labels <- data[, 1]
## plot.labels[2] <- paste(plot.labels[2], "\n", sep = "") # mai0
#plot.labels[which(plot.labels == "sav5")] <- paste("\n", plot.labels[which(plot.labels == "sav5")], sep = "") # sav5
#plot.labels[which(plot.labels == "fer0")] <- paste("\n", plot.labels[which(plot.labels == "fer0")], sep = "") # fer0

addtxt <- function() {
  trellis.focus("panel", 1, 1, clip.off = TRUE, highlight = FALSE)
  panel.axis(side = "top", outside = TRUE, at = data$elevation, 
             labels = plot.labels, ticks = FALSE, text.col = "black")#
  trellis.unfocus()
}

# Save image
png(filename = "out/height_ns_tf_fg_1314.png", width = 30, height = 15, 
    units = "cm", res = 300, pointsize = 15)
update(doubleYScale(xyplot.ns.tf, xyplot.fg, add.ylab2 = TRUE, use.style = TRUE),
       main = "\n",
       par.settings = simpleTheme(col = c("black", "black")), 
       xlab = textGrob(label = "Elevation [m]", rot = 180)) 
draw.key(list(text=list(c("Precipitation", "Throughfall", "Fog"), cex = 0.8),
              corner = c(0.9, 0.9),
              columns = 1, rows = 3,
              col = c(col.ns, col.tf, col.fg),
              background = "transparent", border = FALSE),
         draw = TRUE, padding.text = 5,
         vp = viewport(x = unit(0.84, "npc"),
                       y = unit(0.7, "npc"),
                       just = "centre", clip = "off", angle = 90))
addtxt()
dev.off()

