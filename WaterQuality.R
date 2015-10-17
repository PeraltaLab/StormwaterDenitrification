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

chem.m <- melt(chem, id.vars = c("Location", "Type", "Storm"), measure.vars = "NH4")
chem.c <- cast(data = chem.m, Location + Storm  ~ variable, c(mean, se), na.rm=T)

chem.c <- as.data.frame(chem.c)


# Plot
png(filename="./figures/WaterQuality.png",
    width = 1200, height = 800, res = 96*2)

par(mar=c(3,6,0.5,0.5), oma=c(1,1,1,1)+0.1, lwd=2)
TCplot <- plot(chem.c[,3], ylab = "Ammonium (mg/L)",
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
#Panel of graphs

layout(matrix(c(1,2, 3), 1, 3, byrow = TRUE), widths = c(2, 2, 2))
## show the regions that have been allocated to each plot
layout.show(3)