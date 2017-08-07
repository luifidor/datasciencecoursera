
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