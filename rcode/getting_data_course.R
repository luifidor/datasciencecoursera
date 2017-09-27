#
# Data Cleaning Course
#

if  (file.exists("./data")) {
  dir.create("./data")
}

fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile = "./data/cameras.csv", method = "curl")
list.files("./data/")

# get the date

dateDownloaded <- date()

#  "Thu Sep 21 17:12:04 2017"
#
#

# Read local tables

cameraData <- read.table("./data/cameras.csv", sep = ",", header = TRUE, quote = "" )

#
# nrows = 10 read only the first 10 lines of the file
# na.strings set the character for the na
# skip skip the first 10 
#
getwd()
# read EXCEL files
install.packages("xlsx")
install.packages("rJava")
install.packages("XLConnect")
library(xlsx)
install.packages("rJava")
install.packages("XLConnect")
library(XLConnect) # XLConnect vignette

# it is possible to read specific parts of the Excel
colIndex <- 2:3
rowIndex <- 1:4

cameraData <- read.xlsx("", sheetIndex =1, colIndex = colIndex, rowIndex = rowIndex)


##### READ XMLS
install.packages("XML")
library("XML")
fileUrl <- "https://www.w3schools.com/xml/simple.xml"
dataFile <- "./data/simple.xml"
download.file(fileUrl, dataFile, method ="wget", quiet = TRUE)
doc <- xmlTreeParse(dataFile, useInternal = TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)

#access the first node
rootNode[[1]]

#access the second item of the first node
rootNode [[1]][[1]]

#retrieve the value
#
# /node -> Top level node
# //node -> Node at any level
# node[@attr-name] Node with an attribute name
# node[@attr-name = "bob"] Node with an attribute name
#
xmlSApply(rootNode, xmlValue)

xpathSApply(rootNode, "//name", xmlValue)
xpathSApply(rootNode, "//price", xmlValue)

### try with a webpage

fileUrl <- "http://www.espn.com/nfl/team/schedule/_/name/bal"
doc <- htmlTreeParse(fileUrl, useInternal = TRUE)

scores <- xpathSApply(doc, "//li[@class='score']", xmlValue)
teams <- xpathSApply(doc, "//li[@class='team-name']", xmlValue)


#### JSON FILES ####
install.packages("jsonlite")
install.packages("curl")
library(jsonlite)

#download json data from gibhub
jsonData <- fromJSON("https://api.github.com/users/luifidor/repos")
names(jsonData)

#dataframe within a dataframe
jsonData$owner$login
names(jsonData$owner)

# turn a dataframe into a json
myjson <- toJSON(bidlog, pretty=TRUE)
cat(myjson)

# back from JSON to dataframe
b <- fromJSON(myjson)
head(b)

## data tables

library(data.table)
DF = data.frame(x=rnorm(9), y=rep(c("a", "b", "c"), each=3), z=rnorm(9))
head(DF,3)

DT = data.table(x=rnorm(9), y=rep(c("a", "b", "c"), each=3), z=rnorm(9))
head(DT,3)

#check all the tables in the database
tables()

DT[2,]
DT[DT$y=="a",]

#if we subset without index it takes the rows
DT[c(2,3)] 

#subsetting doesnt work as in dataframes, columns
DT[,c(2,3)]

# it is possible to pass a list of functions to the data table
DT[,list(mean(x), mean(z), length(x))]
DT[, list(table(y))]
DT[,w:=z^2]

# normal assignment does not create a copy, creates a new reference
DT2 <-copy(DT)

# add multiple steps in one execution
DT[, m:= {tmp <- (x+z); log2(tmp+5)}]

# plyr like operations
DT[, a:=x>0]
DT[, b:= mean(x+w), by=a] #aggregated mean and copied to every row by group

# count the number of items in a data table
set.seed(123);
DT <- data.table(x=sample(letters[1:3], 1E5, TRUE))
DT[,.N,by=x]

# set key in data tables
DT <- data.table(x=rep(c("a", "b", "c"), each=100), y=rnorm(300))
setkey(DT, x)
DT["a"]

#merge can be used if you set up a key for both 
merge(DT1, DT2)

# Connect to the databases
# connect R to SQL Server 
## https://www.red-gate.com/simple-talk/sql/reporting-services/making-data-analytics-simpler-sql-server-and-r/
## https://support.rstudio.com/hc/en-us/articles/214510788-Setting-up-R-to-connect-to-SQL-Server-
install.packages("RODBC")
library(RODBC)
??RODBC

cn <-odbcDriverConnect("driver=SQL Server;server=tcp:swbbesqlp01.wbad.group,1434;database=minibid; uid=wbad_report; pwd=Wbad2016")
bidlog <- sqlFetch(cn, 'fact.bidlog', colnames=FALSE,
                   rows_at_time=1000)
cn.Disconnect()


# HDF5
# Standard library for big data

source("https://bioconductor.org/biocLite.R")
biocLite("rhdf5")

library(rhdf5)
#create a file
created = h5createFile("example.h5")

#create groups
created = h5createGroup ("example.h5", "foo")
created = h5createGroup ("example.h5", "baa")
created = h5createGroup ("example.h5", "foo/foobaa")
h5ls("example.h5")

#writting to groups
A = matrix(1:10, nr=5, nc=2)
h5write(A, "example.h5", "foo/A")
B = array(seq(0.1,2.0,by=0.1), dim=c(5,2,2))

# add attributes to the data frames
attr(B, "scale") <- "liter"
h5write(B, "example.h5", "foo/foobaa/B")

# add dataframes directly
df = data.frame(1L:5L, seq(0,1, length.out = 5), c("ab", "cde", "fgh", "a", "s"))
h5write(df, "example.h5", "df")
h5ls("example.h5")

readA = h5read("example.h5", "foo/A")
readB = h5read("example.h5", "foo/foobaa/B")
readdf = h5read("example.h5", "df")

# it is possible to write and read in chunks
# using indexes from the function

h5write(c(12,13,14), "example.h5", "foo/A", index=list(1:3, 1))
h5read("example.h5", "foo/A")

#https://support.hdfgroup.org/downloads/index.html

###### CONNECT TO WEBPAGES #######
# not so easy to read
con = url("https://www.wgzimmer.ch/de/wgzimmer/search/mate/ch/zurich-stadt/")
htmlCode = readLines(con)
close(con)
htmlCode

# use XML package 
library(XML)
url <- "https://www.wgzimmer.ch/de/wgzimmer/search/mate/ch/zurich-stadt"
html <- htmlTreeParse(htmlCode, useInternalNodes=T)
xpathSApply(html, "//title", xmlValue)

xpathSApply(html, "//ul[@class='list']/li", xmlValue)

#obtain an attribute
#https://stackoverflow.com/questions/25315381/using-xpathsapply-to-scrape-xml-attributes-in-r
#https://www.w3schools.com/xml/xpath_syntax.asp
xpathSApply(html, "//ul[@class='list']/li/a", xmlGetAttr, 'href')


# GET from the HTTR package
#install.packages("httr")
#install.packages("httpuv")
library(httr)
library(httpuv)
html2 = GET(url)
content2 = content(html2, as="text")
parsedHtml = htmlParse(content2, asText = TRUE)
xpathSApply(html, "//ul[@class='list']/li/a", xmlGetAttr, 'href')
unlist(parsedHtml["//ul[@class='list']/li/a/@href"])

# accessing websites with passwords
urlFb <- "https://www.facebook.com/noel.figuera.1/friends?lst=671839524%3A671839524%3A1506412766&source_ref=pb_friends_tl"
pg2 = GET(url, authenticate("luisnoelf@hotmail.com", " "))
pg2

fb <- handle("http://facebook.com")
pg3 <- GET(handle=fb, path="/")
names(pg3)

# we are authenticated now trying to extract content out of it
#install.packages("xml2")
content2 <- content(pg2)
parsedHtml = htmlParse(content2, asText = TRUE)
xpathSApply(html, "//ul[@class='list']/li/a", xmlGetAttr, 'href')

### connecting to Twitter

myapp <- oauth_app("twitter", key="jpkrKT2xlOItzrAwNqBrOkrdT", secret="EG6jGjt9GdBquhtDGdr1NtEZMYnknmbsvY9hD4yvGXLLeCi3aj")
sig = sign_oauth1.0(myapp, token = "118716473-EyTbO0WjWt5luZQ6YRurpILcpWvL9E7J0619B7Jp", token_secret = "5b3hayZ1LDDYywYrTW1sZ3VIV0GdJnyHFCY1XsqLoqD9n")
homeTL = GET("https://api.twitter.com/1.1/statuses/home_timeline.json",sig)
# returns a json file, content can recognizes that
cTwit <- content(homeTL)
names(cTwit)

# transform json to data farme
json2 = jsonlite::fromJSON(toJSON(cTwit))
json2[,1:4]


# search for twits that involves tricentis
myapp <- oauth_app("twitter", key="jpkrKT2xlOItzrAwNqBrOkrdT", secret="EG6jGjt9GdBquhtDGdr1NtEZMYnknmbsvY9hD4yvGXLLeCi3aj")
sig = sign_oauth1.0(myapp, token = "118716473-EyTbO0WjWt5luZQ6YRurpILcpWvL9E7J0619B7Jp", token_secret = "5b3hayZ1LDDYywYrTW1sZ3VIV0GdJnyHFCY1XsqLoqD9n")
homeTL = GET("https://api.twitter.com/1.1/search/tweets.json?q=%23Teneriffa&count=5",sig)
# returns a json file, content can recognizes that
cTwit <- content(homeTL)
names(cTwit)
json2 = jsonlite::fromJSON(toJSON(cTwit))
json2$statuses[1:5]$text
cTwit
