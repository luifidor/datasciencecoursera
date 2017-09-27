library(ggplot2)

#Read data files
pm25 <- readRDS("summarySCC_PM25.rds")
classif <- readRDS("Source_Classification_Code.rds")


#
# Question 6
#
# How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

#extract the types that have motor on them
motorFactor <- classif$SCC.Level.Two %in% c("Highway Vehicles - Gasoline", "Highway Vehicles - Diesel", "Off-highway Vehicle Gasoline, 4-Stroke", "CNG")

#subset the pm25 data based on the factor        
motor25 <- subset(pm25, SCC %in% classif[motorFactor, ]$SCC)

#extract Baltimore data
baltMot25 <- subset(motor25, fips == "24510")
baltMot <- with(baltMot25, tapply(Emissions, year, sum))

# emissions for los angeles
angMot25 <- subset(motor25, fips == "06037")
angMot <- with(angMot25, tapply(Emissions, year, sum))

par(mfrow = c(1,2), oma = c(0,0,2,0))
plot(names(angMot  ), angMot  , pch = 19, xlab = "Year", ylab = "Total emissions (Tons)")
title( "Total PM2.5 Los Angeles County")
# add lines, first create a sequence, and then pass it to the segments, - 1 to account for the last point
s <- seq(length(angMot  ) - 1)
segments(as.numeric(names(angMot  [s])), angMot  [s], as.numeric(names(angMot  [s+1])), angMot  [s+1])

plot(names(baltMot ), baltMot , pch = 19, xlab = "Year", ylab = "Total emissions (Tons)")
title( "Total PM2.5 Baltimore City")
# add lines, first create a sequence, and then pass it to the segments, - 1 to account for the last point
s <- seq(length(baltMot ) - 1)
segments(as.numeric(names(baltMot [s])), baltMot [s], as.numeric(names(baltMot [s+1])), baltMot [s+1])
title("Emissions Motor Vehicles", outer = T)

install.packages("XML")
library("XML")
fileUrl <- "http://espn.go.com/nfl/team/_/name/bal/baltimore-ravens"
doc <- htmlTreeParse(fileUrl, useInternal = TRUE)

scores <