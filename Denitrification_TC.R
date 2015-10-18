#
#
#
#
#
#

setwd("~/GitHub/DEA/")
rm(list=ls())
se <- function(x, ...){sd(x, na.rm = TRUE)/sqrt(length(na.omit(x)))}

require("png")
require("reshape")

# Import Data
dat <- read.delim("./data/2015_TC_DEArate.txt")
dat$Replicate <- as.factor(dat$Replicate)

# Sediment Denitrification
dat.sed <- dat[dat$Type != "W", ]

dim1 <- length(dat.sed$acetyleneb[dat.sed$acetyleneb == "-"])

sed.eff <- as.data.frame(matrix(NA, dim1, 4))
colnames(sed.eff) <- c("Location", "Type", "Replicate", "Rate")
sed.eff$Location <- dat.sed$Location[dat.sed$acetyleneb == "-"]
sed.eff$Type <- dat.sed$Type[dat.sed$acetyleneb == "-"]
sed.eff$Replicate <- dat.sed$Replicate[dat.sed$acetyleneb == "-"]
sed.eff$Rate <- (dat.sed$Rate[dat.sed$acetyleneb == "+"] -
					   dat.sed$Rate[dat.sed$acetyleneb == "-"] )

sed.eff.m <- melt(sed.eff)
sed.eff.c <- cast(data = sed.eff.m, Location + Type ~ variable, c(mean, se), na.rm=T)

sed.eff.c <- as.data.frame(sed.eff.c)

sed.eff.m <- as.data.frame(sed.eff.m)

# # ANOVA - update
aov.sed=aov(value~Location*Type,data=sed.eff.m)
summary(aov.sed)
posthocD <-TukeyHSD(aov.sed,"Location:Type",conf.level=0.95)
posthocD

#Df Sum Sq Mean Sq F value   Pr(>F)
#Location       2   3182  1591.0   34.63 1.04e-05 ***
#  Type           1   1393  1393.2   30.32 0.000135 ***
#  Location:Type  2    996   498.2   10.84 0.002043 **
#  Residuals     12    551    45.9
#---Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#$`Location:Type`
#diff        lwr        upr     p adj
#C:BD-B:BD  -6.138610 -24.728454  12.451234 0.8685718
#D:BD-B:BD -32.105738 -50.695582 -13.515894 0.0009204
#B:BK-B:BD  -7.323933 -25.913777  11.265911 0.7678248
#C:BK-B:BD -44.776813 -63.366657 -26.186968 0.0000386
#D:BK-B:BD -38.929400 -57.519244 -20.339556 0.0001545
#D:BD-C:BD -25.967128 -44.556972  -7.377284 0.0053439
#B:BK-C:BD  -1.185323 -19.775167  17.404521 0.9999193
#C:BK-C:BD -38.638203 -57.228047 -20.048359 0.0001661
#D:BK-C:BD -32.790790 -51.380634 -14.200946 0.0007631
#B:BK-D:BD  24.781805   6.191961  43.371649 0.0076133
#C:BK-D:BD -12.671074 -31.260919   5.918770 0.2693580
#D:BK-D:BD  -6.823662 -25.413506  11.766182 0.8132769
#C:BK-B:BK -37.452880 -56.042724 -18.863035 0.0002238
#D:BK-B:BK -31.605467 -50.195311 -13.015623 0.0010568
#D:BK-C:BK   5.847413 -12.742431  24.437257 0.8891990


# Plot - Sediment N2O
png(filename="./figures/SedN2Oonly.png",
    width = 1200, height = 800, res = 96*2)

pdf(file = "./figures/SedN2Oonly.pdf", width=6, height=4, paper='special')


par(mar=c(3,6,0.5,0.5), oma=c(1,1,1,1)+0.1, lwd=2)
bp_plot <- barplot(sed.eff.c[,3],
					ylab = "Denitrification Rate\n(ng N2O * g^-1 sediment * hr^-1)",
					lwd=3, col="gray", cex.lab=1.25, cex.names = 1.25,
					ylim = c(-2, 60), yaxt="n",
          space = c(1, 0.25, 1, 0.25, 1, 0.25),
          density=c(-1, 15, -1, 15, -1, 15))
arrows(x0 = bp_plot, y0 = sed.eff.c[,3], y1 = sed.eff.c[,3] - sed.eff.c[,4],
       angle = 90, length=0.1, lwd = 2)
arrows(x0 = bp_plot, y0 = sed.eff.c[,3], y1 = sed.eff.c[,3] + sed.eff.c[,4],
       angle = 90, length=0.1, lwd = 2)
axis(side = 2, labels=T, lwd.ticks=2, las=2, lwd=2)
mtext(c("Downstream\nSeep", "Stream\nMiddle", "Culvert\n"),
      side = 1, at=c(2, 5.5, 9),
      line = 2, cex=0.8, adj=0.5)
abline(h=0, lwd=2, lty=3)
legend("topright", c("stream bed", "stream bank"), fill="gray", bty="n", cex=1,
       density=c(-1, 15))

dev.off() # sthis writes plot to folder

# Water Denitrification
dat.water <- dat[dat$Type == "W", ]

#dat.water[,7:10][dat.water[,7:10] < 0] <- 0

dim1 <- length(dat.water$acetyleneb[dat.water$acetyleneb == "-"])

wtr.eff <- as.data.frame(matrix(NA, dim1, 4))
colnames(wtr.eff) <- c("Location", "Time", "Replicate", "Rate")
wtr.eff$Location <- dat.water$Location[dat.water$acetyleneb == "-"]
wtr.eff$Time <- dat.water$Time[dat.water$acetyleneb == "-"]
wtr.eff$Replicate <- dat.water$Replicate[dat.water$acetyleneb == "-"]
wtr.eff$Rate <- (dat.water$Rate[dat.water$acetyleneb == "+"] -
					   dat.water$Rate[dat.water$acetyleneb == "-"] )

wtr.eff.m <- melt(wtr.eff)
wtr.eff.c <- cast(data = wtr.eff.m, Location + Time ~ variable, c(mean, se), na.rm=T)

wtr.eff.c <- as.data.frame(wtr.eff.c)
wtr.eff.m <- as.data.frame(wtr.eff.m)

# # ANOVA - update
aov.wtr=aov(value~Location*Time,data=wtr.eff.m)
summary(aov.wtr)

# Df Sum Sq Mean Sq F value Pr(>F)
# Location       5 0.7579 0.15157   1.544  0.221
# Time           1 0.1136 0.11360   1.157  0.295
# Location:Time  3 0.5062 0.16872   1.718  0.195
# Residuals     20 1.9638 0.09819
# Plot - Water Denitrification

png(filename="./figures/WaterN2Oonly.png",
    width = 1200, height = 800, res = 96*2)

par(mar=c(3,6,0.5,0.5), oma=c(1,1,1,1)+0.1, lwd=2)
bp_plot <- barplot(wtr.eff.c[,3],
					ylab = "Denitrification Rate\n(ng N2O * mL^-1 * hr^-1)",
					lwd=3, yaxt="n", col="gray", ex.lab=1.5, cex.names = 1.25,
					ylim = c(-2, 2),
                   	space = c(1, 0.25, 1, 0.25, 1, 0.25, 1, 0.25, 1, 1),
                   	density=c(-1, 15, -1, 15, -1, 15, -1, 15, 15, 15))
arrows(x0 = bp_plot, y0 = wtr.eff.c[,3], y1 = wtr.eff.c[,3] - wtr.eff.c[,4], angle = 90,
       length=0.1, lwd = 2)
arrows(x0 = bp_plot, y0 = wtr.eff.c[,3], y1 = wtr.eff.c[,3] + wtr.eff.c[,4], angle = 90,
       length=0.1, lwd = 2)
axis(side = 2, labels=T, lwd.ticks=2, las=2, lwd=2)
mtext(c("Tar River\nOutflow", "Downstream\nSeep", "Stream\nMiddle", "Culvert\n",
        "Upstream\nCulvert", "Upstream\nInflow"),
      side = 1, at=c(2, 5.5, 9, 11.75, 14.5, 17),
      line = 2, cex=0.8, adj=0.5)
abline(h=0, lwd=2, lty=3)
legend("topright", c("baseflow", "stormflow"), fill="gray", bty="n", cex=1,
       density=c(-1, 15))


dev.off() # this writes plot to folder
graphics.off() # shuts down open devices



#Panel of graphs

layout(matrix(c(1,2, 3), 1, 3, byrow = TRUE), widths = c(2, 2, 2))
## show the regions that have been allocated to each plot
layout.show(3)

# Import Data - water quality
chem <- read.delim("./data/2015_TC_WaterQualityData.txt")
chem$Replicate <- as.factor(chem$Replicate)

chem.m <- melt(chem, id.vars = c("Location", "Type", "Storm"), measure.vars = "NH4")
chem.c <- cast(data = chem.m, Location + Storm  ~ variable, c(mean, se), na.rm=T)

chem.c <- as.data.frame(chem.c)




# Plot
png(filename="./figures/WaterQuality.png",
    width = 1200, height = 800, res = 96*2)

par(mar=c(3,6,0.5,0.5), oma=c(1,1,1,1)+0.1, lwd=2)
TCplot <- barplot(chem.c[,3], ylab = "Ammonium (mg/L)",
                   lwd=3, yaxt="n", col="gray",
                   cex.lab=1.5, cex.names = 1.25, space = c(1, 0.25, 1, 0.25, 1, 0.25, 1, 0.25, 1, 1, 1, 1, 1))

points(TCplot, chem.c[,3], pch=22, cex = 2, density=c(-1, 15, -1, 15, -1, 15, -1, 15, 15, 15, 15, 15, 15))
arrows(x0 = TCplot, y0 = chem.c[,3], y1 = chem.c[,3] - chem.c[,4], angle = 90,
       length=0.1, lwd = 2)
arrows(x0 = TCplot, y0 = chem.c[,3], y1 = chem.c[,3] + chem.c[,4], angle = 90,
       length=0.1, lwd = 2)
axis(side = 2, labels=T, lwd.ticks=2, las=2, lwd=2)
mtext(c("A", "B", "C", "D","E", "F", "G", "H", "I"))
      side = 1, at=c(2, 4, 5, 8.5, 12, 14.5, 16.5),
      line = 2, cex=0.8, adj=0.5)
legend("topright", c("Baseline", "Storm"), fill="gray", bty="n", cex=1.25,
       density=c(-1, 15))

dev.off() # this writes plot to folder
graphics.off() # shuts down open devices
