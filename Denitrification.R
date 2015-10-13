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

#All Samples - Denitrification
dat.all <- dat[dat$acetyleneb == "+", ]

# Calculate Rate
dat.all$Rate <- rep(NA, dim(dat.all)[1])
for (i in 1:dim(dat.all)[1]){
  if (all(is.na(dat.all[i,7:10]))){
    next
  } else {
    model <- lm(as.numeric(dat.all[i,7:10]) ~ c(0:3))
    B <- round(as.numeric(model$coefficients[2]), 3)
    dat.all$Rate[i] <- B
  }}

# Remove Odd Sample
dat.all <- dat.all[-18, ]

dat.all.m <- melt(dat.all, id.vars = c("Location", "Type", "Time"), measure.vars = "Rate")
dat.all.c <- cast(data = dat.all.m[dat.all.m$Type != "W", ], Location + Type  ~ variable, c(mean, se), na.rm=T)

dat.all.c <- as.data.frame(dat.all.c)

# Plot
png(filename="./figures/SedDenitRate.png",
    width = 1200, height = 800, res = 96*2)

par(mar=c(3,6,0.5,0.5), oma=c(1,1,1,1)+0.1, lwd=2)
bp_plot <- barplot(dat.all.c[,3], ylab = "Denitification Rate\n()",
                   ylim = c(0, 1600), lwd=3, yaxt="n", col="gray",
                   cex.lab=1.5, cex.names = 1.25, xlim = c(0.5,9.5),
                   space = c(1, 0.25, 1, 0.25, 1, 0.25),
                   density=c(-1, 15, -1, 15, -1, 15))
arrows(x0 = bp_plot, y0 = dat.all.c[,3], y1 = dat.all.c[,3] - dat.all.c[,4], angle = 90,
       length=0.1, lwd = 2)
arrows(x0 = bp_plot, y0 = dat.all.c[,3], y1 = dat.all.c[,3] + dat.all.c[,4], angle = 90,
       length=0.1, lwd = 2)
axis(side = 2, labels=T, lwd.ticks=2, las=2, lwd=2)
mtext(c("Downstream\nSeep", "Stream\nMiddle", "Culvert\n"), side = 1, at=c(2.125, 5.375, 8.625),
      line = 2, cex=1.5, adj=0.5)
legend("topright", c("Stream Bed", "Stream Bank"), fill="gray", bty="n", cex=1.25,
       density=c(-1, 15))


dev.off() # this writes plot to folder
graphics.off() # shuts down open devices


# Sediment Denitrification
dat.sed <- dat[dat$Type != "W", ]

# Calculate Rate
dat.sed$Rate <- rep(NA, dim(dat.sed)[1])
for (i in 1:dim(dat.sed)[1]){
  if (all(is.na(dat.sed[i,7:10]))){
    next
  } else {
    model <- lm(as.numeric(dat.sed[i,7:10]) ~ c(0:3))
    B <- round(as.numeric(model$coefficients[2]), 3)
    dat.sed$Rate[i] <- B
  }}


dim1 <- length(dat.sed$acetyleneb[dat.sed$acetyleneb == "-"])

sed.eff <- as.data.frame(matrix(NA, dim1, 4))
colnames(sed.eff) <- c("Location", "Time", "Replicate", "Efficiency")
sed.eff$Location <- dat.sed$Location[dat.sed$acetyleneb == "-"]
sed.eff$Time <- dat.sed$Time[dat.sed$acetyleneb == "-"]
sed.eff$Replicate <- dat.sed$Replicate[dat.sed$acetyleneb == "-"]
sed.eff$Efficiency <- (dat.sed$Rate[dat.sed$acetyleneb == "+"] -
                       dat.sed$Rate[dat.sed$acetyleneb == "-"] ) /
                       dat.sed$Rate[dat.sed$acetyleneb == "+"]

sed.eff.m <- melt(sed.eff)
sed.eff.c <- cast(data = sed.eff.m, Location + Time ~ variable, c(mean, se), na.rm=T)

sed.eff.c <- as.data.frame(sed.eff.c)

# Plot
png(filename="./figures/SedimentDenitrification.png",
    width = 1600, height = 1200, res = 96*2)

par(mar=c(3,6,0.5,0.5), oma=c(1,1,1,1)+0.1, lwd=2)
bp_plot <- barplot(sed.eff.c[,3], ylab = "Denitrification Efficiency\n(N2 Production)",
                   ylim = c(0, 1.2), lwd=3, yaxt="n", col="gray",
                   cex.lab=1.5, cex.names = 1.25)
arrows(x0 = bp_plot, y0 = sed.eff.c[,3], y1 = sed.eff.c[,3] - sed.eff.c[,4],
       angle = 90, length=0.1, lwd = 2)
arrows(x0 = bp_plot, y0 = sed.eff.c[,3], y1 = sed.eff.c[,3] + sed.eff.c[,4],
       angle = 90, length=0.1, lwd = 2)
axis(side = 2, labels=T, lwd.ticks=2, las=2, lwd=2)
mtext(c("Downstream\nSeep", "Stream\nMiddle", "Culvert\n"), side = 1, at=bp_plot[c(1, 2, 3)],
      line = 2, cex=1.5, adj=0.5)

dev.off() # this writes plot to folder
graphics.off() # shuts down open devices




# Water Denitrification Figure - NOT WORKING YET
dat.water <- dat[dat$Type == "W", ]

# Calculate Rate
dat.water$Rate <- rep(NA, dim(dat.water)[1])
for (i in 1:dim(dat.water)[1]){
	if (all(is.na(dat.water[i,7:10]))){
		next
	} else {
	model <- lm(as.numeric(dat.water[i,7:10]) ~ c(0:3))
	B <- round(as.numeric(model$coefficients[2]), 3)
	dat.water$Rate[i] <- B
}}

dim1 <- length(dat.water$acetyleneb[dat.water$acetyleneb == "-"])

wtr.eff <- as.data.frame(matrix(NA, dim1, 4))
colnames(wtr.eff) <- c("Location", "Time", "Replicate", "Efficiency")
wtr.eff$Location <- dat.water$Location[dat.water$acetyleneb == "-"]
wtr.eff$Time <- dat.water$Time[dat.water$acetyleneb == "-"]
wtr.eff$Replicate <- dat.water$Replicate[dat.water$acetyleneb == "-"]
wtr.eff$Efficiency <- (dat.water$Rate[dat.water$acetyleneb == "+"] -
					   dat.water$Rate[dat.water$acetyleneb == "-"] ) /
					  dat.water$Rate[dat.water$acetyleneb == "+"]

wtr.eff.m <- melt(wtr.eff)
wtr.eff.c <- cast(data = wtr.eff.m, Location + Time ~ variable, c(mean, se), na.rm=T)

wtr.eff.c <- as.data.frame(wtr.eff.c)

# Plot
png(filename="./figures/WaterDenitrEff.png",
    width = 1200, height = 800, res = 96*2)

par(mar=c(3,6,0.5,0.5), oma=c(1,1,1,1)+0.1, lwd=2)
bp_plot <- barplot(wtr.eff.c[,3], ylab = "Denitification Efficiency\n(Total N2 - N20/Total N2)",
                   lwd=3, yaxt="n", col="gray",
                   cex.lab=1.5, cex.names = 1.25,
                   space = c(1, 0.25, 1, 0.25, 1, 0.25, 1, 0.25, 1, 1),
                   density=c(-1, 15, -1, 15, -1, 15, -1, 15, 15, 15))
arrows(x0 = bp_plot, y0 = wtr.eff.c[,3], y1 = wtr.eff.c[,3] - wtr.eff.c[,4], angle = 90,
       length=0.1, lwd = 2)
arrows(x0 = bp_plot, y0 = wtr.eff.c[,3], y1 = wtr.eff.c[,3] + wtr.eff.c[,4], angle = 90,
       length=0.1, lwd = 2)
axis(side = 2, labels=T, lwd.ticks=2, las=2, lwd=2)
mtext(c("Tar River\nOutflow", "Downstream\nSeep", "Stream\nMiddle", "Culvert\n",
        "Upstream\nCulvert", "Upstream\nInflow"),
      side = 1, at=c(2, 4, 5, 8.5, 12, 14.5, 16.5),
      line = 2, cex=0.8, adj=0.5)
legend("topright", c("Baseline", "Storm"), fill="gray", bty="n", cex=1.25,
       density=c(-1, 15))


dev.off() # this writes plot to folder
graphics.off() # shuts down open devices


# GENERIC Plot
png(filename="./figures/WaterDenitrification.png",
    width = 1600, height = 1200, res = 96*2)

par(mar=c(2,6,0.5,0.5), oma=c(1,1,1,1)+0.1, lwd=2)
bp_plot <- barplot(wtr.eff.c[,3], ylab = "Denitification Efficiency/n(N2)",
                   ylim = c(0, 1.1), lwd=3, yaxt="n", col="gray",
                   cex.lab=1.5, cex.names = 1.25,
                   space = c(1, 0.2, 1, 0.2, 1, 0.2, 1, 0.2, 1, 1))
arrows(x0 = bp_plot, y0 = wtr.eff.c[,3], y1 = wtr.eff.c[,3] - wtr.eff.c[,4], angle = 90,
       length=0.1, lwd = 2)
arrows(x0 = bp_plot, y0 = wtr.eff.c[,3], y1 = wtr.eff.c[,3] + wtr.eff.c[,4], angle = 90,
       length=0.1, lwd = 2)
axis(side = 2, labels=T, lwd.ticks=2, las=2, lwd=2)
mtext(levels(wtr.eff.c$Location), side = 1, at=bp_plot[c(1, 3, 5, 7, 9, 10)],
      line = 1, cex=1.5, adj=0)

dev.off() # this writes plot to folder
graphics.off() # shuts down open devices


