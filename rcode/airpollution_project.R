
## Project Air Pollution 
## 1999
### read the data
pm0 <- read.table("\\\\wbad.group\\dfs_WB\\10_Home\\UWBBENOELF\\DATA\\Desktop\\pm25_data\\pm25_data\\RD_501_88101_1999-0.txt", comment.char = "#", header = FALSE, sep = "|", na.strings = "")

### check dimensions
dim(pm0)
head(pm0)

### read the column names
cnames <- readLines("\\\\wbad.group\\dfs_WB\\10_Home\\UWBBENOELF\\DATA\\Desktop\\pm25_data\\pm25_data\\RD_501_88101_1999-0.txt",1)
cnames <- strsplit(cnames, "|", fixed = TRUE)

names(pm0) <- make.names(cnames[[1]]) # turns the fields into a valid name for a data frame

### Extract the value of the PM25 metric
x0 <- pm0$Sample.Value
class(x0) #check type
str(x0)
summary(x0)
### Test for missing values proportion
mean(is.na(x0))

## 2012
pm1 <- read.table("\\\\wbad.group\\dfs_WB\\10_Home\\UWBBENOELF\\DATA\\Desktop\\pm25_data\\pm25_data\\RD_501_88101_2012-0.txt", comment.char = "#", header = FALSE, sep = "|", na.strings = "")

### check dimensions
dim(pm1)
head(pm1)
### read the column names
names(pm1) <- make.names(cnames[[1]]) # turns the fields into a valid name for a data frame
### Extract the value of the PM25 metric
x1 <- pm1$Sample.Value
class(x1) #check type
str(x1)
summary(x1)

### Quickly compare the two datasets
summary(x0)
summary(x1)

### check boxplot
boxplot(x0, x1)
boxplot(log10(x0), log10(x1))

### check out negative values because because of the measure system it should not happen
negative <- x1 < 0
sum(negative, na.rm = TRUE) #include only true
mean(negative, na.rm = TRUE) # proportion

## validate that something is not happening a particular date
dates <- pm1$Date
str(dates)
dates <- as.Date(as.character(dates), "%Y%m%d")

## create an histogram for dates by month
hist(dates, "month")
hist(dates[negative], "month")

### extract all the date of the US
site0 <- unique(subset(pm0, State.Code == 36, c(County.Code, Site.ID)))
site1 <- unique(subset(pm1, State.Code == 36, c(County.Code, Site.ID)))
## create a single variable combining county and site id
site0 <- paste(site0[,1], site0[,2], sep = ".")
site1 <- paste(site1[,1], site1[,2], sep = ".")

## identify items that are repeated in both datasets
both <- intersect(site0, site1)

## create a new variable to the vector of the dataframe
pm0$county.site <- with(pm0, paste(County.Code, Site.ID, sep = "."))
pm1$county.site <- with(pm1, paste(County.Code, Site.ID, sep = "."))

## subset the original data frames
cnt0 <- subset(pm0, State.Code == 36 & county.site %in% both) # first time using the %in% operator
cnt1 <- subset(pm1, State.Code == 36 & county.site %in% both)

## Count the number of observations by monitor site
sapply(split(cnt0, cnt0$county.site), nrow)
sapply(split(cnt1, cnt1$county.site), nrow)

## subset the county and site
pm1sub <- subset (pm1, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
pm0sub <- subset (pm0, State.Code == 36 & County.Code == 63 & Site.ID == 2008)

## check the dimensions
dim (pm1sub)
dim (pm0sub)

dates1 <- as.Date(as.character(pm1sub$Date), "%Y%m%d")
dates0 <- as.Date(as.character(pm0sub$Date), "%Y%m%d")
# get the values of the pm points
x1sub <- pm1sub$Sample.Value
x0sub <- pm0sub$Sample.Value

## look at one monitor and check how values changed
par(mfrow = c(1, 2), mar = c(4,4,2,1))
plot(dates0, x0sub, pch = 20, ylim = c(0, 40))
abline(h = median (x0sub, na.rm = TRUE))
plot(dates1, x1sub, pch = 20, ylim = c(0, 40))
abline(h = median(x1sub, na.rm=TRUE))

## create a mean per state
mn0 <- with(pm0, tapply(Sample.Value, State.Code, mean, na.rm = T))
mn1 <- with(pm1, tapply(Sample.Value, State.Code, mean, na.rm = T))
str(mn0)
str(mn1)
summary(mn0)
summary(mn1)

## create a data frame with the results
d0 <- data.frame(state = names(mn0), mean = mn0)
d1 <- data.frame(state = names(mn1), mean = mn1)

## merge the data by state
mrg <- merge(d0, d1, by = "state")
head(mrg)
names(mrg) <- c("state", "1999", "2012")


## plot the values
par(mfrow = c(1,1))
with(mrg, plot(rep(1999, 52), mrg[,2], xlim = c(1998, 2013)))
with(mrg, points(rep(2012, 52), mrg[,3]))
segments(rep(1999, 52), mrg[,2], rep(2012, 52), mrg[,3]) # connect the lines between the points

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