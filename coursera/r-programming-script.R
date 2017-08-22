
m <- matrix(nrow = 2, ncol = 3)
m

#Matriz dimensions
m <- 1:10
dim(m) <- c(2,5)
m

#Factors

## creates a factor vector
x<-factor(c("YES", "NO", "YES", "YES", "NO"))
x
## checks the count of items within that vector
table(x)

unclass(x)

x<-factor(c("YES", "NO", "YES", "YES", "NO"), levels=c("YES", "NO"))


## missing values

x<-c(1,2,NA,4)
is.na(x)

x<-c(1,2,NA,NaN)
is.na(x)
is.nan(x)

## data frames
x <- data.frame (foo = 1:4, bar = c(T, T, F, F))
x
nrow(x)
ncol(x)

## names - is it also possible to assign names to vectors and list
x<-1:3
names(x) <- c("foo", "bar", "do")
x

## removing na values
x<- c(1, 2, NA, 4, NA, 5)
bad <- is.na(x)
x[!bad]

## remove items that are not matching across vectors
x<-c(1, 2, NA, 4, NA, 5)
y<-c("a","b",NA,"d", NA, "f")
length(x)
length(y)
good <- complete.cases(x,y)
good
x[good]
y[good]

## list
x <- list(2, "a", "b", TRUE)


# test

x <- read.csv("hw1_data.csv")
sum(is.na(x$Ozone))
rle(is.na(x$Ozone))

x[!is.na(x$Ozone),]$Ozone

mean(x[!is.na(x$Ozone),]$Ozone)

x[!is.na(x$Ozone) & !is.na(x$Temp) & x$Ozone > 31 & x$Temp > 90,]$Solar.R

mean(x[!is.na(x$Ozone) & !is.na(x$Temp) & x$Ozone > 31 & x$Temp > 90,]$Solar.R)

max(x[!is.na(x$Ozone) & x$Month ==5,]$Ozone)


## Control 

x <- 5

if(x > 3) {
        y <- 10
        
} else {
        y <- 0
}

y

## for loops vectors

x<-c("a", "b", "c", "d")

for (i in seq_along(x)) {
        print(x[i])
}

## for loops matrix

x<-matrix(1:6, 2, 3)

for (i in seq_len(nrow(x))) {
        for (j in seq_len(ncol(x))) {
                print(x[i,j])
        }
}

# while loop

count <- 0

while (count < 10) {
        print(count)
        count <- count + 1
}


## Column definitions

add2 <- function(x,y) {
        x + y
}

above <- function(x, y) {
        
        use <- x > y
        
        x[use]
        
}

columnmean <- function (y, removeNA = TRUE) {
        nc <- ncol(y)
        means <- numeric(nc)
        for (i in 1:nc) {
                means[i] <- mean(y[, i], na.rm = removeNA)
        }
        means
        
}

# dates

x<- as.Date()


# LAPPLY

x <- list(a= 1:5, b = rnorm(10))
x
lapply(x, mean)

mean (1:5)

#apply

x<- matrix(rnorm(200), 20, 10)
x
apply(x, 1, quantile, probs = c(0.25,0.75))

rnorm (2 * 2 * 3)

#range

range(c(1:5, 6))
x<-c(1:5, 6)
y<-gl(3, 2)
lapply(split(x, y), mean)

####################################################################################################################
####################################################################################################################

#bidlog example
#mean savings per bid type per month
install.packages("matrixStats")
library(matrixStats)
b_reason <- bidlog$reason
b_member <- bidlog$member
interaction(b_member, b_reason)
b_reason_means <- sapply(split(bidlog,list(b_member, b_reason), drop = TRUE), function(x) colMeans(x[, c("award_localdn_var", "localdn_savings_rate", "count_processing_days", "count_analysis_scenarios")], na.rm = TRUE))
write.table(b_reason_means, 'clipboard', sep='\t')

b_reasons_variance <- lapply(b_reason, function(x) colSds(x[, c("award_localdn_var", "localdn_savings_rate", "count_processing_days", "count_analysis_scenarios")]))

print (lapply)
# connect R to SQL Server 
## https://www.red-gate.com/simple-talk/sql/reporting-services/making-data-analytics-simpler-sql-server-and-r/
## https://support.rstudio.com/hc/en-us/articles/214510788-Setting-up-R-to-connect-to-SQL-Server-
install.packages("RODBC")
library(RODBC)
??RODBC
cn <-odbcDriverConnect("driver=SQL Server;server=swbbesqld01.wbad.group;database=minibid; uid=minibid_submit; pwd=Wbad2016")
bidlog <- sqlFetch(cn, 'fact.bidlog', colnames=FALSE,
                   rows_at_time=1000)
hist(as.numeric(bidlog$count_processing_days), xlim=c(0,30),breaks = 7)

## Analyze the distribution of processing days across years
hist(as.numeric(bidlog[(bidlog$count_processing_days < 100 & bidlog$close_date_cy == 2015), ]$count_processing_days), ylim=c(0, 0.05), probability = TRUE)
hist(as.numeric(bidlog[(bidlog$count_processing_days < 100 & bidlog$close_date_cy == 2016), ]$count_processing_days), ylim=c(0, 0.05), probability = TRUE)
hist(as.numeric(bidlog[(bidlog$count_processing_days < 100 & bidlog$close_date_cy == 2017), ]$count_processing_days),  ylim=c(0, 0.05), probability = TRUE)

sapply(bidlog$award_localdn_var, max, na.rm=TRUE)

summary(bidlog$award_localdn_var)

hist(as.numeric(bidlog[(bidlog$award_localdn_var < 4e+06 & bidlog$award_localdn_var > 150000), ]$award_localdn_var), xlim = c(150000, 4e06),probability = TRUE)

which.max(bidlog$award_localdn_var)
bidlog[which.max(bidlog$award_localdn_var),]


install.packages('pastecs')
library (Hmisc)
describe(bidlog$award_localdn_var)


#####################################################################
# Exam week 2
#####################################################################

#https://stackoverflow.com/questions/15897236/whats-the-equivalent-to-excels-left-and-right-in-r

right = function (string, char){
        substr(string,nchar(string)-(char-1),nchar(string))
}

left = function (string,char){
        substr(string,1,char)
}

polluteantmean <- function(directory, pollutant, id = 1:332) {
        
        # directory: character vector of length 1 indicating the location of the CSV files
        
        # pollutant is a character vector indicating hte name of the pollutant
        
        # id is an integer vector indicating the monito id numbers
        
        x <- NULL
        
        for (i in id) {
           filename <- right(paste("000", i, sep=""), 3)
           #print(paste(directory, filename , ".csv",sep="",collapse=""))
           y <- read.csv(file=paste(directory, filename , ".csv",sep="",collapse=""))
           #y <- cbind(y, id=i)
           if (!is.null(x)) {
                x <- rbind(x, y)
           } else {
                x <- y
           }
           
        }
        #mean(x[, pollutant], na.rm = TRUE)
        x
}

#a <- "\\wbad.group\dfs_WB\10_Home\UWBBENOELF\DATA\Desktop\GIT Folder\coursera\specdata\"

a <- "\\\\wbad.group\\dfs_WB\\10_Home\\UWBBENOELF\\DATA\\Desktop\\GIT Folder\\coursera\\specdata\\"
b <- "nitrate"
i <- 70:72

polluteantmean(a, "nitrate", 70:72)
polluteantmean(a, "nitrate", 23)
polluteantmean(a, "sulfate", 1:10)
system.time(polluteantmean(a, "nitrate", 1:332))
        