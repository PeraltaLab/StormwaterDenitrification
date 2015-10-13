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


# Sediment Denitrification
dat.sed <- dat[dat$Type != "W", ]










# Water Denitrification Figure
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


