# Adding Header Here
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
dat <- read.delim("./data/2015_TC_DEA.txt")
dat$Replicate <- as.factor(dat$Replicate)

# # ANOVA - update
# aov.prnd=aov(copies~Rotation+Block,data=data.prnd)
# summary(aov.prnd)
# posthocD <-TukeyHSD(aov.prnd,"Rotation",conf.level=0.95)
# posthocD


# Import Data - water quality
chem <- read.delim("./data/2015_TC_WaterQualityData.txt")
chem$Replicate <- as.factor(chem$Replicate)


# Example Plot: Ammonium

## Define Data
chem.m <- melt(chem, id.vars = c("Location", "Type", "Storm"), measure.vars = "NH4")
chem.c <- cast(data = chem.m, Location + Storm  ~ variable, c(mean, se), na.rm=T)
chem.c <- as.data.frame(chem.c)

#Start Plotting Figure
png(filename="./figures/WaterQualityExample.png",
    width = 1200, height = 800, res = 96*2)

xvars <- c(0.8, 1.2, 1.8, 2.2, 2.8, 3.2, 3.8, 4.2, 5, 5.5, 6, 6.5, 7)

par(mar=c(3,4,0.5,0.5), oma=c(1,1,1,1)+0.1, lwd=2)
TCplot <- plot(x = xvars, y = chem.c[,3],
               ylab = "Ammonium (mg/L)", xlim = c(0.5, 7.25),
               lwd=3, yaxt="n", xaxt = "n", col="black", pch = 22,
                   cex.lab=1.5)
arrows(x0 = xvars, y0 = chem.c[,3], y1 = chem.c[,3] - chem.c[,4], angle = 90,
       length=0.1, lwd = 2)
arrows(x0 = xvars, y0 = chem.c[,3], y1 = chem.c[,3] + chem.c[,4], angle = 90,
       length=0.1, lwd = 2)
points(x = xvars, y = chem.c[,3], pch=22, cex=3,
       bg=c("white", "gray", "white", "gray", "white", "gray", "white", "gray",
            "gray", "gray", "gray", "gray", "gray"))
axis(side = 2, labels=T, lwd.ticks=2, las=2, lwd=2)
axis(side = 1, labels=F, lwd.ticks=2, lwd = 2, at = c(1, 2, 3, 4, 5, 5.5, 6, 6.5, 7))
mtext(c("A", "B", "C", "D","E", "F", "G", "H", "I"),
      side = 1, at=c(1, 2, 3, 4, 5, 5.5, 6, 6.5, 7),
      line = 1, cex=1, adj=0.5)
mtext("Station", side = 1, line = 2.5, cex=1.5)
legend("topright", c("Baseline", "Storm"), fill=c("white", "gray"), bty="n", cex=1.25)

dev.off() # this writes plot to folder
graphics.off() # shuts down open devices



# Water Quality Panel Figure
png(filename="./figures/WaterQuality.png",
    width = 1600, height = 1600, res = 96*2)

# Globals
xvars <- c(0.8, 1.2, 1.8, 2.2, 2.8, 3.2, 3.8, 4.2, 5, 5.5, 6, 6.5, 7)
par(mar=c(5,7,0.5,0.5), oma=c(1,1,1,1)+0.1, lwd=2)

# Layout
layout(matrix(c(1,2, 3, 4), 2, 2, byrow = TRUE),
       widths = c(2, 2, 2, 2), heights = c(2,2, 2,2))

# Ammonium
chem.m <- melt(chem, id.vars = c("Location", "Type", "Storm"), measure.vars = "NH4")
chem.c <- cast(data = chem.m, Location + Storm  ~ variable, c(mean, se), na.rm=T)
chem.c <- as.data.frame(chem.c)

TCplot <- plot(x = xvars, y = chem.c[,3],
               ylab = "Ammonium (mg/L)", xlim = c(0.5, 7.25), xlab = "",
               lwd=3, yaxt="n", xaxt = "n", col="black", pch = 22,
               cex.lab=1.5)
arrows(x0 = xvars, y0 = chem.c[,3], y1 = chem.c[,3] - chem.c[,4], angle = 90,
       length=0.1, lwd = 2)
arrows(x0 = xvars, y0 = chem.c[,3], y1 = chem.c[,3] + chem.c[,4], angle = 90,
       length=0.1, lwd = 2)
points(x = xvars, y = chem.c[,3], pch=22, cex=3,
       bg=c("white", "gray", "white", "gray", "white", "gray", "white", "gray",
            "gray", "gray", "gray", "gray", "gray"))
axis(side = 2, labels=T, lwd.ticks=2, las=2, lwd=2)
axis(side = 1, labels=F, lwd.ticks=2, lwd = 2, at = c(1, 2, 3, 4, 5, 5.5, 6, 6.5, 7))
mtext(c("A", "B", "C", "D","E", "F", "G", "H", "I"),
      side = 1, at=c(1, 2, 3, 4, 5, 5.5, 6, 6.5, 7),
      line = 1, cex=1, adj=0.5)
mtext("Station", side = 1, line = 2.5, cex=1.5)
legend("topright", c("Baseline", "Storm"), fill=c("white", "gray"), bty="n", cex=1.25)

# Nitrate Nitrite
chem.m <- melt(chem, id.vars = c("Location", "Type", "Storm"), measure.vars = "NO3.NO2")
chem.c <- cast(data = chem.m, Location + Storm  ~ variable, c(mean, se), na.rm=T)
chem.c <- as.data.frame(chem.c)

TCplot <- plot(x = xvars, y = chem.c[,3],
               ylab = "Nitrate/Nitrite (mg/L)", xlim = c(0.5, 7.25), xlab = "",
               lwd=3, yaxt="n", xaxt = "n", col="black", pch = 22,
               cex.lab=1.5)
arrows(x0 = xvars, y0 = chem.c[,3], y1 = chem.c[,3] - chem.c[,4], angle = 90,
       length=0.1, lwd = 2)
arrows(x0 = xvars, y0 = chem.c[,3], y1 = chem.c[,3] + chem.c[,4], angle = 90,
       length=0.1, lwd = 2)
points(x = xvars, y = chem.c[,3], pch=22, cex=3,
       bg=c("white", "gray", "white", "gray", "white", "gray", "white", "gray",
            "gray", "gray", "gray", "gray", "gray"))
axis(side = 2, labels=T, lwd.ticks=2, las=2, lwd=2)
axis(side = 1, labels=F, lwd.ticks=2, lwd = 2, at = c(1, 2, 3, 4, 5, 5.5, 6, 6.5, 7))
mtext(c("A", "B", "C", "D","E", "F", "G", "H", "I"),
      side = 1, at=c(1, 2, 3, 4, 5, 5.5, 6, 6.5, 7),
      line = 1, cex=1, adj=0.5)
mtext("Station", side = 1, line = 2.5, cex=1.5)
legend("topright", c("Baseline", "Storm"), fill=c("white", "gray"), bty="n", cex=1.25)



# DOC
chem.m <- melt(chem, id.vars = c("Location", "Type", "Storm"), measure.vars = "DOC")
chem.c <- cast(data = chem.m, Location + Storm  ~ variable, c(mean, se), na.rm=T)
chem.c <- as.data.frame(chem.c)

TCplot <- plot(x = xvars, y = chem.c[,3],
               ylab = "DOC (mg/L)", xlim = c(0.5, 7.25), xlab = "",
               lwd=3, yaxt="n", xaxt = "n", col="black", pch = 22,
               cex.lab=1.5)
arrows(x0 = xvars, y0 = chem.c[,3], y1 = chem.c[,3] - chem.c[,4], angle = 90,
       length=0.1, lwd = 2)
arrows(x0 = xvars, y0 = chem.c[,3], y1 = chem.c[,3] + chem.c[,4], angle = 90,
       length=0.1, lwd = 2)
points(x = xvars, y = chem.c[,3], pch=22, cex=3,
       bg=c("white", "gray", "white", "gray", "white", "gray", "white", "gray",
            "gray", "gray", "gray", "gray", "gray"))
axis(side = 2, labels=T, lwd.ticks=2, las=2, lwd=2)
axis(side = 1, labels=F, lwd.ticks=2, lwd = 2, at = c(1, 2, 3, 4, 5, 5.5, 6, 6.5, 7))
mtext(c("A", "B", "C", "D","E", "F", "G", "H", "I"),
      side = 1, at=c(1, 2, 3, 4, 5, 5.5, 6, 6.5, 7),
      line = 1, cex=1, adj=0.5)
mtext("Station", side = 1, line = 2.5, cex=1.5)
legend("topleft", c("Baseline", "Storm"), fill=c("white", "gray"), bty="n", cex=1.25)


# Total Dissolved Nitrogen
chem.m <- melt(chem, id.vars = c("Location", "Type", "Storm"), measure.vars = "TDN")
chem.c <- cast(data = chem.m, Location + Storm  ~ variable, c(mean, se), na.rm=T)
chem.c <- as.data.frame(chem.c)

TCplot <- plot(x = xvars, y = chem.c[,3],
               ylab = "TDN (mg/L)", xlim = c(0.5, 7.25), xlab = "",
               lwd=3, yaxt="n", xaxt = "n", col="black", pch = 22,
               cex.lab=1.5)
arrows(x0 = xvars, y0 = chem.c[,3], y1 = chem.c[,3] - chem.c[,4], angle = 90,
       length=0.1, lwd = 2)
arrows(x0 = xvars, y0 = chem.c[,3], y1 = chem.c[,3] + chem.c[,4], angle = 90,
       length=0.1, lwd = 2)
points(x = xvars, y = chem.c[,3], pch=22, cex=3,
       bg=c("white", "gray", "white", "gray", "white", "gray", "white", "gray",
            "gray", "gray", "gray", "gray", "gray"))
axis(side = 2, labels=T, lwd.ticks=2, las=2, lwd=2)
axis(side = 1, labels=F, lwd.ticks=2, lwd = 2, at = c(1, 2, 3, 4, 5, 5.5, 6, 6.5, 7))
mtext(c("A", "B", "C", "D","E", "F", "G", "H", "I"),
      side = 1, at=c(1, 2, 3, 4, 5, 5.5, 6, 6.5, 7),
      line = 1, cex=1, adj=0.5)
mtext("Station", side = 1, line = 2.5, cex=1.5)
legend("topright", c("Baseline", "Storm"), fill=c("white", "gray"), bty="n", cex=1.25)

dev.off() # this writes plot to folder
graphics.off() # shuts down open devices


