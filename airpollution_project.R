
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
## 
## 
