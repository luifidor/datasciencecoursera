library(ggplot2)

#Read data files
pm25 <- readRDS("summarySCC_PM25.rds")
classif <- readRDS("Source_Classification_Code.rds")

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
qplot(data = as.data.frame(c25), y = as.numeric(c25), x = as.factor(names(c25)), xlab= "Year", ylab = "Emissions (Tons)",geom = "line", group = 1, main = "Total Emissions Coal Combustion related sources") 
