
#Read data files
pm25 <- readRDS("summarySCC_PM25.rds")
classif <- readRDS("Source_Classification_Code.rds")

#first lines
head(pm25)
head(classif)

# Question 1: Sum emssions
#
cypm25 <- tapply(pm25$Emissions, pm25$year, sum)
par(mfrow = c(1,1))
plot(names(cypm25), cypm25, pch = 19, xlab = "Year", ylab = "Total emissions (Tons)")
title( "Total PM2.5 - United States")
# add lines, first create a sequence, and then pass it to the segments, - 1 to account for the last point
s <- seq(length(cypm25) - 1)
segments(as.numeric(names(cypm25[s])), cypm25[s], as.numeric(names(cypm25[s+1])), cypm25[s+1])

