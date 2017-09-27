

#Read data files
pm25 <- readRDS("summarySCC_PM25.rds")
classif <- readRDS("Source_Classification_Code.rds")


# Question 2: Sum emssions
balt25 <- with(pm25[pm25$fips == "24510",], tapply(Emissions, year, sum))
plot(names(balt25), balt25, pch = 19, xlab = "Year", ylab = "Total emissions (Tons)")
title( "Total PM2.5 Baltimore City")
# add lines, first create a sequence, and then pass it to the segments, - 1 to account for the last point
s <- seq(length(balt25) - 1)
segments(as.numeric(names(balt25[s])), balt25[s], as.numeric(names(balt25[s+1])), balt25[s+1])
