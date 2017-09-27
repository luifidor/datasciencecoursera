
## 
## 
## Test 4:
## 

#library(ggplot2)
#install.packages("plyr")

#Read data files
pm25 <- readRDS("summarySCC_PM25.rds")
classif <- readRDS("Source_Classification_Code.rds")

#first lines
head(pm25)
head(classif)

# Question 1: Sum emssions
#
cypm25 <- tapply(pm25$Emissions, pm25$year, sum)
plot(names(cypm25), cypm25, pch = 19, xlab = "Year", ylab = "Total emissions (Tons)")
title( "Total PM2.5")
# add lines, first create a sequence, and then pass it to the segments, - 1 to account for the last point
s <- seq(length(cypm25) - 1)
segments(as.numeric(names(cypm25[s])), cypm25[s], as.numeric(names(cypm25[s+1])), cypm25[s+1])


# Question 2: Sum emssions
balt25 <- with(pm25[pm25$fips == "24510",], tapply(Emissions, year, sum))
plot(names(balt25), balt25, pch = 19, xlab = "Year", ylab = "Total emissions (Tons)")
title( "Total PM2.5")
# add lines, first create a sequence, and then pass it to the segments, - 1 to account for the last point
s <- seq(length(balt25) - 1)
segments(as.numeric(names(balt25[s])), balt25[s], as.numeric(names(balt25[s+1])), balt25[s+1])

# Question 3: ggplot 2
#
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
# Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make a plot 
# answer this question.

# Subset the data
balt25 <- subset(pm25, fips == "24510")
# create temporary variable
balt25$year.type <- with(balt25, paste(year, type, sep = "."))
# calcualte the sum for the compound variable
balttype25 <- with(balt25, tapply(Emissions, year.type, sum))

# start building a data frame for the ggplot graph
b <- as.data.frame(balttype25)
b$year.type <- names(balttype25)
b$year <- as.numeric(lapply(strsplit(names(balttype25), "\\."), `[[`, 1))
b$type <- as.character(lapply(strsplit(names(balttype25), "\\."), `[[`, 2))
names(b) <- c("Emissions", "year.type", "year", "type")
# plot the grah
ggplot(data = b, aes(x=year, y=Emissions, group = type, color = type)) + geom_line() + geom_point (size = 4, shape = 21, fill = "white")

#
# Question 4
#
# Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?

# create a factor with sources that include Coal
coalFactor <- ifelse(grepl("Coal", unique(classif$Short.Name)), TRUE, FALSE)

# filter the items that have coal on them
coal25 <- subset(pm25, SCC %in% classif[coalFactor,]$SCC)

# calculate the totals by subgroup
c25 <- tapply(coal25$Emissions, coal25$year, sum)
qplot(data = as.data.frame(c25), y = as.numeric(c25), x = as.factor(names(c25)), geom = "line", group = 1) 

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

#
# Question 6
#
# How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

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