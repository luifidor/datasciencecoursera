######## Exam Week 1

# Question 1
# Download the file American Community Survey
#https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl, destfile = "./data/exam1.csv")
exam1 <- read.csv("./data/exam1.csv")

# count the number of properties that are worth a millon
sum(exam1$VAL == 24, na.rm = TRUE)

unique(exam1$FES)

# Question 3
# Download the spreadsheet natural gas aquisition program 
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(fileUrl, destfile = "./data/exam1_gas.xlsx")
exam1 <- read.xlsx("./data/exam1_gas.xlsx",sheetIndex =1, colIndex = 7:15, rowIndex = 18:23)

# Question 4
# Download the xml of Baltimore restaurants
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
download.file(fileUrl, destfile = "./data/exam1_xml.xml")
doc <- xmlTreeParse("./data/exam1_xml.xml", useInternal = TRUE)
xmlRoot(doc)
#extract the zip codes
zipCodes <- xpathSApply(xmlRoot(doc), "//zipcode", xmlValue)
sum(zipCodes == "21231")

# Question 5
# Download the American Community Survey 
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(fileUrl, destfile = "./data/exam1_community.csv")
library(data.table)
DT <- fread( "./data/exam1_community.csv")

system.time(
        tapply(DT$pwgtp15, DT$SEX,mean)
)

system.time(
        DT[,mean(pwgtp15),by=SEX]
)

######## Exam Week 2
#
#

# Question 1
# register in githug and use oauth to access the repo
# https://github.com/r-lib/httr/blob/master/demo/oauth2-github.r
# https://stackoverflow.com/questions/30819293/authorization-code-for-github-api-used-in-r
# I had the redirect url badly setup

myapp <- oauth_app("github", key="481bb59de51ef576f2d3", secret="55637e83547dc17bdd2439557a62118d8d0ca055")

#get token
library(httr)
library(jsonlite)

github_token <- oauth2.0_token(oauth_endpoints("github"), myapp, use_oob=FALSE)
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
stop_for_status(req)
content(req)
json <- content(req)
#transform into a dataframe
Df = jsonlite::fromJSON(toJSON(content(req)))
#show all the available repositories
Df[1:length(Df)]$html_url
#filter the repository data sharing, extract the whole record
extract <-Df[Df[1:length(Df)]$html_url == "https://github.com/jtleek/datasharing",]
extract$created_at

# Question 2
# Download the American Community Survey data and load it into an R object called
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(fileUrl, destfile = "./data/exam1_community.csv")
acs <- read.csv("./data/exam1_community.csv")
#install.packages("sqldf")
#install.packages("RSQLite")
#install.packages("DBI")
library("sqldf")
#it allows to translate SQL statements into R code
sqldf("select pwgtp1 from acs where AGEP < 50")
# Question 3
sqldf("select distinct AGEP from acs")

# Question 4
# how many characters are in the 10th, 20th, 30th, and 100th lines
# http://biostat.jhsph.edu/~jleek/contact.html
#
#
# create connection
con = url("http://biostat.jhsph.edu/~jleek/contact.html")
# read all lines into a DF
htmlCode = readLines(con)
# close connection
close(con)

nchar(htmlCode[10])
nchar(htmlCode[20])
nchar(htmlCode[30])
nchar(htmlCode[100])

# Question 5
#https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"
download.file(fileUrl, destfile = "./data/exam2_community.for")

file <- read.fwf("./data/exam2_community.for", widths = c(10, 9, 5, 8, 5,8,5,8,5), skip=4)
head(file)
sum(as.numeric(file$V4)) + sum(as.numeric(file$V9))