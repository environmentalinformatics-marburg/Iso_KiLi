#######           ########
#### LMWLs Kilimanjaro ###
#######           ########

### setwd()
wd <- setwd("C:/Users/iotte/Documents/Desktop/training/")

df <- read.csv2("iso_calc.csv", header = T, sep = ";")

sub <- subset(df, df$plot_id_sp1 != "mnp1" &
              df$plot_id_sp1 != "mnp2")
#####
sub$mnth <- substr(sub$date_sample, 6, 7)

sub.wet <- subset(sub, sub$mnth == "03" |
                   sub$mnth == "04" |
                   sub$mnth == "05" |
                   sub$mnth == "11" |
                   sub$mnth == "12" |
                   sub$mnth == "01")
sub.wt.sm <- summary(lm(dD_H ~ d18_16, data = sub.wet), na.rm = TRUE)


sub.dry <- subset(sub, sub$mnth == "02" |
                   sub$mnth == "06" |
                   sub$mnth == "07" |
                   sub$mnth == "08" |
                   sub$mnth == "09" |
                   sub$mnth == "10")
sub.dy.sm <- summary(lm(dD_H ~ d18_16, data = sub.dry), na.rm = TRUE)


#######
rn <- subset(sub, sub$type == "rain")
rn.sm <- summary(lm(dD_H ~ d18_16, data = rn), na.rm = TRUE)

tf <- subset(sub, sub$type == "tf")
tf.sm <- summary(lm(dD_H ~ d18_16, data = tf), na.rm = TRUE)

fg <- subset(sub, sub$type == "fog")
fg.sm <- summary(lm(dD_H ~ d18_16, data = fg), na.rm = TRUE)

#######
# wet/dry season
# rainfall
rn$mnth <- substr(rn$date_sample, 6, 7)

rn.wet <- subset(rn, rn$mnth == "03" |
                   rn$mnth == "04" |
                   rn$mnth == "05" |
                   rn$mnth == "11" |
                   rn$mnth == "12" |
                   rn$mnth == "01")
rn.wt.sm <- summary(lm(dD_H ~ d18_16, data = rn.wet), na.rm = TRUE)


rn.dry <- subset(rn, rn$mnth == "02" |
                   rn$mnth == "06" |
                   rn$mnth == "07" |
                   rn$mnth == "08" |
                   rn$mnth == "09" |
                   rn$mnth == "10")
rn.dy.sm <- summary(lm(dD_H ~ d18_16, data = rn.dry), na.rm = TRUE)

# short rains
rn.wet.sh <- subset(rn, rn$mnth == "11" |
                      rn$mnth == "12" |
                      rn$mnth == "01")
rn.wt.sm.sh <- summary(lm(dD_H ~ d18_16, data = rn.wet.sh), na.rm = TRUE)

# long rains
rn.wet.lg <- subset(rn, rn$mnth == "03" |
                      rn$mnth == "04" |
                      rn$mnth == "05")
rn.wt.sm.lg <- summary(lm(dD_H ~ d18_16, data = rn.wet.lg), na.rm = TRUE)
x<-subset(rn.wet.sh, rn.wet.sh$d18_16!="NA")



# wet/dry season
# throughfall
tf$mnth <- substr(tf$date_sample, 6, 7)

tf.wet <- subset(tf, tf$mnth == "03" |
                   tf$mnth == "04" |
                   tf$mnth == "05" |
                   tf$mnth == "11" |
                   tf$mnth == "12" |
                   tf$mnth == "01")
tf.wt.sm <- summary(lm(dD_H ~ d18_16, data = tf.wet), na.rm = TRUE)


tf.dry <- subset(tf, tf$mnth == "02" |
                   tf$mnth == "06" |
                   tf$mnth == "07" |
                   tf$mnth == "08" |
                   tf$mnth == "09" |
                   tf$mnth == "10")
tf.dy.sm <- summary(lm(dD_H ~ d18_16, data = tf.dry), na.rm = TRUE)

# short rains
tf.wet.sh <- subset(tf, tf$mnth == "11" |
                      tf$mnth == "12" |
                      tf$mnth == "01")
tf.wt.sm.sh <- summary(lm(dD_H ~ d18_16, data = tf.wet.sh), na.rm = TRUE)

# long rains
tf.wet.lg <- subset(tf, tf$mnth == "03" |
                      tf$mnth == "04" |
                      tf$mnth == "05")
tf.wt.sm.lg <- summary(lm(dD_H ~ d18_16, data = tf.wet.lg), na.rm = TRUE)
x<-subset(tf.wet.lg, tf.wet.lg$d18_16!="NA")




# wet/dry season
# fog
fg$mnth <- substr(fg$date_sample, 6, 7)

fg.wet <- subset(fg, fg$mnth == "03" |
                   fg$mnth == "04" |
                   fg$mnth == "05" |
                   fg$mnth == "11" |
                   fg$mnth == "12" |
                   fg$mnth == "01")
fg.wt.sm <- summary(lm(dD_H ~ d18_16, data = fg.wet), na.rm = TRUE)


fg.dry <- subset(fg, fg$mnth == "02" |
                   fg$mnth == "06" |
                   fg$mnth == "07" |
                   fg$mnth == "08" |
                   fg$mnth == "09" |
                   fg$mnth == "10")
fg.dy.sm <- summary(lm(dD_H ~ d18_16, data = fg.dry), na.rm = TRUE)

# short rains
fg.wet.sh <- subset(fg, fg$mnth == "11" |
                   fg$mnth == "12" |
                   fg$mnth == "01")
fg.wt.sm.sh <- summary(lm(dD_H ~ d18_16, data = fg.wet.sh), na.rm = TRUE)

# long rains
fg.wet.lg <- subset(fg, fg$mnth == "03" |
                   fg$mnth == "04" |
                   fg$mnth == "05")
fg.wt.sm.lg <- summary(lm(dD_H ~ d18_16, data = fg.wet.lg), na.rm = TRUE)
x<-subset(fg.wet.sh, fg.wet.sh$d18_16!="NA")




