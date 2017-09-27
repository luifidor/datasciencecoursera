
library(ggplot2)

#Read data files
pm25 <- readRDS("summarySCC_PM25.rds")
classif <- readRDS("Source_Classification_Code.rds")

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
ggplot(data = b, aes(x=year, y=Emissions, group = type, color = type)) + geom_line() + geom_point (size = 4, shape = 21, fill = "white") + ggtitle("Total PM25 Emissions by Type")
