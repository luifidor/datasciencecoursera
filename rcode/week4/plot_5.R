
#library(ggplot2)

#Read data files
pm25 <- readRDS("summarySCC_PM25.rds")
classif <- readRDS("Source_Classification_Code.rds")

#
# Question 5
#
# How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

# explore potential level
as.factor(classif$SCC.Level.Two)

#extract the types that have motor on them
motorFactor <- classif$SCC.Level.Two %in% c("Highway Vehicles - Gasoline", "Highway Vehicles - Diesel", "Off-highway Vehicle Gasoline, 4-Stroke", "CNG")

#subset the pm25 data based on the factor        
motor25 <- subset(pm25, SCC %in% classif[motorFactor, ]$SCC)

#extract Baltimore data
baltMot25 <- subset(motor25, fips == "24510")

baltMot <- with(baltMot25, tapply(Emissions, year, sum))
plot(names(baltMot ), baltMot , pch = 19, xlab = "Year", ylab = "Total emissions (Tons)")
title( "Total PM2.5 Motor Vehicles Baltimore City")
# add lines, first create a sequence, and then pass it to the segments, - 1 to account for the last point
s <- seq(length(baltMot ) - 1)
segments(as.numeric(names(baltMot [s])), baltMot [s], as.numeric(names(baltMot [s+1])), baltMot [s+1])
