library(ggplot2)

# set working directory
wd <- setwd("C:/Users/IOtte/documents/Desktop/training/")

# read.data
iso <- read.csv2("mm.iso.csv", header = T) 

iso$date_sample <- as.Date(iso$date_sample)


# amount effect significant?
# spearman, significance, scatterplot
# sep for each water type
iso.rn <- subset(iso, type == "rain")
iso.tf <- subset(iso, type == "tf")
iso.fg <- subset(iso, type == "fog")

iso.fg.nw <- subset(iso.fg, date_sample > "2013-10-14")

# for all plots
iso.pl.all <- qplot(amount_mm, d18_16, data = iso.fg.nw, 
                             color = plot_id_sp1,
                             xlab = "amount fog [mm]",
                             ylab = "d18O fog",
                             main = "Amount effect of fog") 

  
iso.pl <- qplot(amount_mm, d18_16, data = iso.fg.nw, 
                color = plot_id_sp1,
                xlab = "amount fog [mm]",
                ylab = "d18O fog",
                main = "Amount effect of fog") +
  facet_wrap( ~ plot_id_sp1)


iso.fg.cor <- cor.test(iso.fg.nw$amount_mm, iso.fg.nw$d18_16, method = "spearman")
iso.tf.cor <- cor.test(iso.tf$amount_mm, iso.tf$d18_16, method = "spearman")
iso.rn.cor <- cor.test(iso.rn$amount_mm, iso.rn$d18_16, method = "spearman")


# on plot basis
# for fog
iso.fg.nw.fer0 <- subset(iso.fg.nw, plot_id_sp1 == "fer0")
iso.fg.nw.fpd0 <- subset(iso.fg.nw, plot_id_sp1 == "fpd0")
iso.fg.nw.fpo0 <- subset(iso.fg.nw, plot_id_sp1 == "fpo0")
iso.fg.nw.foc0 <- subset(iso.fg.nw, plot_id_sp1 == "foc0")
iso.fg.nw.foc6 <- subset(iso.fg.nw, plot_id_sp1 == "foc6")
iso.fg.nw.flm1 <- subset(iso.fg.nw, plot_id_sp1 == "flm1")

iso.fg.cor.fer0 <- cor.test(iso.fg.nw.fer0$amount_mm, iso.fg.nw.fer0$d18_16, method = "spearman")
iso.fg.cor.fpd0 <- cor.test(iso.fg.nw.fpd0$amount_mm, iso.fg.nw.fpd0$d18_16, method = "spearman")
iso.fg.cor.fpo0 <- cor.test(iso.fg.nw.fpo0$amount_mm, iso.fg.nw.fpo0$d18_16, method = "spearman")
iso.fg.cor.foc0 <- cor.test(iso.fg.nw.foc0$amount_mm, iso.fg.nw.foc0$d18_16, method = "spearman")
iso.fg.cor.foc6 <- cor.test(iso.fg.nw.foc6$amount_mm, iso.fg.nw.foc6$d18_16, method = "spearman")
iso.fg.cor.flm1 <- cor.test(iso.fg.nw.flm1$amount_mm, iso.fg.nw.flm1$d18_16, method = "pearson")

# for tf
iso.tf.fer0 <- subset(iso.tf, plot_id_sp1 == "fer0")
iso.tf.fpd0 <- subset(iso.tf, plot_id_sp1 == "fpd0")
iso.tf.fpo0 <- subset(iso.tf, plot_id_sp1 == "fpo0")
iso.tf.foc0 <- subset(iso.tf, plot_id_sp1 == "foc0")
iso.tf.foc6 <- subset(iso.tf, plot_id_sp1 == "foc6")
iso.tf.flm1 <- subset(iso.tf, plot_id_sp1 == "flm1")

iso.fg.cor.fer0 <- cor.test(iso.tf.fer0$amount_mm, iso.tf.fer0$d18_16, method = "spearman")
iso.fg.cor.fpd0 <- cor.test(iso.tf.fpd0$amount_mm, iso.tf.fpd0$d18_16, method = "spearman")
iso.fg.cor.fpo0 <- cor.test(iso.tf.fpo0$amount_mm, iso.tf.fpo0$d18_16, method = "spearman")
iso.fg.cor.foc0 <- cor.test(iso.tf.foc0$amount_mm, iso.tf.foc0$d18_16, method = "spearman")
iso.fg.cor.foc6 <- cor.test(iso.tf.foc6$amount_mm, iso.tf.foc6$d18_16, method = "spearman")
iso.fg.cor.flm1 <- cor.test(iso.tf.flm1$amount_mm, iso.tf.flm1$d18_16, method = "pearson")


# for rain

iso.rn.fer0 <- subset(iso.rn, plot_id_sp1 == "fer0")
iso.rn.fpd0 <- subset(iso.rn, plot_id_sp1 == "fpd0")
iso.rn.fpo0 <- subset(iso.rn, plot_id_sp1 == "fpo0")
iso.rn.foc0 <- subset(iso.rn, plot_id_sp1 == "foc0")
iso.rn.foc6 <- subset(iso.rn, plot_id_sp1 == "foc6")
iso.rn.flm1 <- subset(iso.rn, plot_id_sp1 == "flm1")

iso.fg.cor.fer0 <- cor.test(iso.rn.fer0$amount_mm, iso.rn.fer0$d18_16, method = "spearman")
iso.fg.cor.fpd0 <- cor.test(iso.rn.fpd0$amount_mm, iso.rn.fpd0$d18_16, method = "spearman")
iso.fg.cor.fpo0 <- cor.test(iso.rn.fpo0$amount_mm, iso.rn.fpo0$d18_16, method = "spearman")
iso.fg.cor.foc0 <- cor.test(iso.rn.foc0$amount_mm, iso.rn.foc0$d18_16, method = "spearman")
iso.fg.cor.foc6 <- cor.test(iso.rn.foc6$amount_mm, iso.rn.foc6$d18_16, method = "spearman")
iso.fg.cor.flm1 <- cor.test(iso.rn.flm1$amount_mm, iso.rn.flm1$d18_16, method = "spearman")



