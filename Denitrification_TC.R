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

# Plot - Sediment N2O
png(filename="./figures/SedN2Oonly.png",
    width = 1200, height = 800, res = 96*2)

par(mar=c(3,6,0.5,0.5), oma=c(1,1,1,1)+0.1, lwd=2)
bp_plot <- barplot(sed.eff.c[,3], 
					ylab = "Denitrification Rate\n(ng N2O * g^-1 sediment * hr^-1)",
					lwd=3, yaxt="n", col="gray", ex.lab=1.5, cex.names = 1.25,
					ylim = c(-2, 60), 
                   	space = c(1, 0.25, 1, 0.25, 1, 0.25),
                   	density=c(-1, 15, -1, 15, -1, 15))
arrows(x0 = bp_plot, y0 = sed.eff.c[,3], y1 = sed.eff.c[,3] - sed.eff.c[,4], angle = 90,
       length=0.1, lwd = 2)
arrows(x0 = bp_plot, y0 = sed.eff.c[,3], y1 = sed.eff.c[,3] + sed.eff.c[,4], angle = 90,
       length=0.1, lwd = 2)
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