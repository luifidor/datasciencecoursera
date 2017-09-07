
#Exploratory Statistics
#Questions:
# - Is sometimes the allin DNAC lower than the awarded item?
# - 

summary(bidlog$award_localdn_var)
boxplot(bidlog$award_localdn_var)
hist(bidlog$award_localdn_var, col = "red", breaks = 300)
rug(bidlog$award_localdn_var)

summary(bidlog$count_processing_days)
boxplot(bidlog$count_processing_days, col="green")
abline(h = 25)
hist(bidlog$count_processing_days, col = "red", breaks = 100)
rug(bidlog$count_processing_days)
abline(v=median(bidlog$count_processing_days), col ="black", lwd = 4)

barplot(table(bidlog$member), col="wheat", main = "Number of bids per Member")

for (i in levels(factor(bidlog$open_date_cy))) {
        print(summary(bidlog[bidlog$open_date_cy == i,]$count_processing_days))
}

plot(bidlog[bidlog$open_date_cy > 2015,]$count_processing_days)
hist(bidlog[bidlog$open_date_cy == 2016 & bidlog$count_processing_days > 0,]$count_processing_days, breaks = 100)
hist(bidlog[bidlog$open_date_cy == 2017 & bidlog$count_processing_days > 0,]$count_processing_days, breaks = 100)

data <- bidlog[bidlog$open_date_cy == 2017 & bidlog$count_processing_days > 0 & (bidlog$member == "ABC" | bidlog$member == "WAG"),]

## so change the margin on the left (no. 2)


##Lesson 3


intersect(data$member, data$reason)
## default is c(5, 4, 4, 2) + 0.1
## and shrink text size so we don't need a huge margin
par(mar = c(5, 15, 4, 2) + 0.1)
boxplot(data$count_processing_days ~ data$reason, col="wheat", las = 2, horizontal = TRUE,  main="Count Processing Days per Reason")
par(mar = c(5, 15, 4, 2) + 0.1)
barplot(table(data[data$bid_status=="PENDING" || data$bid_status=="CLOSED"]$reason), col="gray", las = 2, horiz = TRUE, cex.axis = 0.7, main="Count Bids per Reason - 2017")
hist(subset(data, member=="ABC")$count_processing_days, col = "green")
hist(subset(data, member=="WAG")$count_processing_days, col = "green")
par(mfrow = c(1,2), mar = c(5, 4, 2, 1))
boxplot(subset(data, bid_owner == "Marc_Friolet")$count_processing_days ~ subset(data, bid_owner == "Marc_Friolet")$member, main = "MF", ylim = c(0, 40))
boxplot(subset(data, bid_owner == "Mike_Osinski")$count_processing_days ~ subset(data, bid_owner == "Mike_Osinski")$member, main = "MO", ylim = c(0, 40))

par(mfrow = c(1,2), mar = c(5, 4, 2, 1))
barplot(table(subset(data, bid_owner == "Marc_Friolet")$member), main = "MF", ylim = c(0,150))
barplot(table(subset(data, bid_owner == "Mike_Osinski")$member), main = "MO", ylim = c(0,150))

par(mfrow = c(1,2), mar = c(5, 4, 2, 1))
barplot(table(subset(data, bid_owner == "Marc_Friolet")$member), main = "MF", horiz = FALSE, ylim = c(0,150))
barplot(table(subset(data, bid_owner == "Mike_Osinski")$reason), main = "MO", horiz = FALSE, ylim = c(0,150))

interaction(data$member,  data$reason)
par()
barplot(table(with(data, interaction(member,  reason), drop = TRUE )), horiz = TRUE, las = 2)

hist(subset(data, bid_owner == "Mike_Osinski")$count_processing_days ~ subset(data, bid_owner == "Mike_Osinski")$member, main = "MO", ylim = c(0, 40))


boxplot(subset(data, bid_owner == c("Mike_Osinski","Marc_Friolet") )$count_processing_days ~ subset(data, bid_owner == c("Mike_Osinski","Marc_Friolet"))$member)

#scatterplot
with(data, plot(count_processing_days, award_localdn_var, col =member))

#boxplot
transform(bidlog, open_date_cy = factor(open_date_cy))
boxplot (localdn_savings_rate ~ open_date_cy, bidlog, xlab = "Calendar Year", ylab = "Savings rate", ylim = c(0,1) )
boxplot (localdn_savings_rate ~ interaction(member, open_date_cy), subset(bidlog, member == "ABC" | member == "WAG"), xlab = "Calendar Year", ylab = "Savings rate", ylim = c(0,1) )
abline(h = 0.1)

#count the number of samples
#https://stackoverflow.com/questions/9809166/count-number-of-rows-within-each-group
#https://stackoverflow.com/questions/30794273/plot-a-data-frame-as-a-table
library(data.table)
DT <- data.table(subset(bidlog, member == "ABC" | member == "WAG"))
x <- DT[,.N, by = list(open_date_cy, member)]
library(gridExtra)
order(x$open_date_cy, x$member)
ss <- tableGrob(x[order(x$open_date_cy, x$member)])
grid.arrange(ss)
#aggregate(interaction(bidlog$member, bidlog$open_date_cy), data = bidlog, FUN = length)

## reset the plotting parameters

summary(data[data$reason=="MINIBID", ]$count_processing_days)
abline(h=14)

with(bidlog, plot(count_processing_days, localdn_savings_rate, ylim=c(0, 2)))
with(subset(bidlog, member =="ABC"), points(count_processing_days, localdn_savings_rate, col = "blue"))


with(bidlog, plot(count_processing_days, localdn_savings_rate, ylim=c(0, 2), xlim = c(0,50), type ="n"))
with(subset(bidlog, reason == "MINIBID" & member =="ABC" & open_date_cy == 2016), points(count_processing_days, localdn_savings_rate, col = "blue", pch = 20))
with(subset(bidlog, reason == "MINIBID" & member =="WAG" & open_date_cy == 2016), points(count_processing_days, localdn_savings_rate, col = "red", pch = 20))
model <- lm(count_processing_days ~ localdn_savings_rate, subset(bidlog, reason == "MINIBID" & member =="WAG" & open_date_cy == 2016))
abline(model, lwd = 2)
legend("topright", pch = 20, col = c("blue", "red"), legend = c("ABC", "WAG"))

par(mfrow = c(1, 3), mar =c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
with(bidlog, {
        plot (open_date_cy, localdn_savings_rate, main = "DNAC Savings per month", ylim = c(0,1))
        plot (open_date_cy, contract_savings_rate, main = "Contract Savings per month", ylim = c(0,1))
        plot (open_date_cy, count_processing_days, main ="Processing Days")
        mtext ("Bidlog analysis", outer = TRUE)
})


## Lattice plot
reset_par()
library(lattice)
xyplot(award_localdn_var  ~ count_processing_days  | member * reason, data = data, layout = c(2, 1))

## ggplot2

library(ggplot2)
qplot(count_processing_days, localdn_savings_rate, data = data, ylim = c(0,1))

# par() parameters
# las: orientation of the axis labels on plots
# bg: background color
# mar: margin size
# oma: outer margin size
# mfrow: number of plots per row
# mfcol: number of plors per column

# basic graphic parameters
# pch: plotting symbol
# lty: line type
# lwd: line width
# col: plotting color
# xlab: character string for x-axis label
# ylab: character string for the y-axis label
#

levels(bidline$offer_type)

append_row <- function(vector, row) {
        
        if (is.null(vector)) {
                vector <- row
        } else {
                rbind(vector, row)
        }
}

bid_ids <- levels(factor(bidline$bid_id))

inc <- subset(bidline, offer_type == "Inc")
offer <- subset(bidline, offer_type == "Offer")
award <- subset(bidline, award >= 0.75)
lines_merged <- merge(offer, inc[, c("ndc", "vendor", "award", "indirect_cost", "contract_cost", "locadn_cost", "allindn_cost", "bid_id", "item_num")], by =c("bid_id", "item_num"))
lines_awarded <- merge(lines_merged, award[, c("ndc", "vendor", "award", "indirect_cost", "contract_cost", "locadn_cost", "allindn_cost", "bid_id", "item_num")], by =c("bid_id", "item_num"))

analysis <- subset(lines_awarded, award.x == 0 & allindn_cost > allindn_cost.x)

diff <-  analysis$allindn_cost -  analysis$allindn_cost.x
value <- analysis$annual_qty * diff

length(diff)
length(value)
length(analysis)
consolidated <- cbind(analysis, as.data.frame(diff),as.data.frame(value))

head(consolidated[order(-value),"bid_id"], 1)

length(consolidated)

sapply (analysis, function(x) {
        
        diff <-  x$allindn_cost -  x$allindn_cost.x #awarded cost - lowest offer cost
        value <- x$annual_qty * diff
        cbind(x, diff, value)
        
})

levels(factor(analysis$bid_id))

consolidated[, c("bid_id", "award.x")]

bid_ids <- levels(factor(inc$bid_id))
inc[, c("ndc", "vendor", "award", "indirect_cost", "contract_cost", "locadn_cost", "allindn_cost")]
for (id in bid_ids) {
        
}

#leeson 1 week 2
library(lattice)
xyplot(localdn_savings_rate )

xyplot(localdn_savings_rate  ~ count_processing_days  | factor(member) * factor(reason), data = data, layout = c(2, 2), ylim = c(0, 1))

xyplot(localdn_savings_rate  ~ count_processing_days  | factor(member) * factor(reason) * factor(open_date_cy)
       , xlabel = "Count of bid processing days"
       , ylabel = "Percentage of discount"
       , data = bidlog[bidlog$reason == "MINIBID" & bidlog$open_date_cy > 2014 & bidlog$count_processing_days <51, ],layout = c(2, 3), ylim = c(0, 1), panel =function(x, y, ...) {
               panel.xyplot (x, y, ...)
               panel.abline (h = median(y), lty =2)
               #panel.abline (h = mean(y), lty =1)
               panel.lmline (x, y, col = 2, lty =1)
               panel.abline (v= median(x), lty =1)
               
       })

xyplot(localdn_savings_rate  ~ count_processing_days  | factor(member) * factor(reason) * factor(open_date_cy)
       , xlabel = "Count of bid processing days"
       , ylabel = "Percentage of discount"
       , data = bidlog[bidlog$reason == "COMPETITIVE" & bidlog$open_date_cy > 2014 & bidlog$count_processing_days <51, ],layout = c(1, 3), ylim = c(0, 1), panel =function(x, y, ...) {
               panel.xyplot (x, y, ...)
               panel.abline (h = median(y), lty =2)
               #panel.abline (h = mean(y), lty =1)
               panel.lmline (x, y, col = 2, lty =1)
               panel.abline (v= median(x), lty =1)
               
       })
       
# GGPLOT2
library(ggplot2)
str(mpg)
qplot (displ, hwy, data = mpg)
qplot (displ, hwy, data = mpg) + geom_smooth()

qplot(count_processing_days, localdn_savings_rate, data = bidlog[bidlog$reason == "MINIBID" & bidlog$count_processing_days <51 & bidlog$localdn_savings_rate < 1 & bidlog$localdn_savings_rate > 0,]
      , color = member)

qplot(count_processing_days, localdn_savings_rate, data = bidlog[bidlog$reason == "MINIBID" & bidlog$count_processing_days <51 & bidlog$localdn_savings_rate < 1 & bidlog$localdn_savings_rate > 0,]
      , geom = c("point", "smooth")
      , color = member)

qplot(localdn_savings_rate, data = bidlog[bidlog$reason == "MINIBID" & bidlog$count_processing_days <51 & bidlog$localdn_savings_rate < 1 & bidlog$localdn_savings_rate > 0,]
      , fill = member)
qplot(localdn_savings_rate, data = bidlog[bidlog$reason == "MINIBID" & bidlog$count_processing_days <51 & bidlog$localdn_savings_rate < 1 & bidlog$localdn_savings_rate > 0,]
      , geom = "density"
      , color = member)

qplot(count_processing_days, data = bidlog[bidlog$reason == "MINIBID" & bidlog$count_processing_days <51 & bidlog$localdn_savings_rate < 1 & bidlog$localdn_savings_rate > 0,]
      , fill = member)

#Pretty cool one
qplot(count_processing_days, localdn_savings_rate, data = bidlog[bidlog$reason == "MINIBID" & bidlog$count_processing_days <51 & bidlog$localdn_savings_rate < 1 & bidlog$localdn_savings_rate > 0,]
      , geom = c("point", "smooth")
      , facets = open_date_cy~ member)

qplot(count_processing_days, localdn_savings_rate, data = bidlog[bidlog$reason == "COMPETITIVE" & bidlog$count_processing_days <51 & bidlog$localdn_savings_rate < 1 & bidlog$localdn_savings_rate > 0,]
      , geom = c("point", "smooth")
      , facets = open_date_cy ~ member)

qplot(count_processing_days, localdn_savings_rate, data = bidlog[bidlog$reason == "MINIBID",])
qplot(count_processing_days, localdn_savings_rate, data = bidlog[bidlog$reason == "MINIBID",], shape = member)
qplot(count_processing_days, localdn_savings_rate, data = bidlog[bidlog$reason == "MINIBID",], color = member)

qplot(count_processing_days, localdn_savings_rate, data = bidlog[bidlog$reason == "MINIBID",]
      , color = member, facets = .~member) + geom_smooth(method = "loess")

# ggplot2

g <- ggplot(bidlog, aes(count_processing_days,localdn_savings_rate))
#g + geom_point(color = "steelblue", size = 4, alpha =1/2) + geom_smooth() + facet_grid(member~.) + coord_cartesian(ylim = c(0, 1), xlim =c(0,50)) 
g + geom_point(aes(color = reason), size = 4, alpha =1/2) + geom_smooth() + labs(title = "Processing days vs savings") + labs(y = "Local DNAC", x = "Count processing days")  + facet_grid(member~.) + coord_cartesian(ylim = c(0, 1), xlim = c(0,60))
      
g <- ggplot(bidlog[bidlog$reason == "MINIBID",], aes(factor(num_players_over_5perc),localdn_savings_rate))
#g + geom_point(color = "steelblue", size = 4, alpha =1/2) + geom_smooth() + facet_grid(member~.) + coord_cartesian(ylim = c(0, 1), xlim =c(0,50)) 
g + geom_point(aes(color = reason), size = 4, alpha =1/2) + geom_smooth() + labs(title = "Processing days vs savings") + labs(y = "Local DNAC", x = "Count processing days")  + facet_grid(member~.) + coord_cartesian(ylim = c(0, 1), xlim = c(0,10))

#change the base theme
g <- ggplot(bidlog[bidlog$reason == "MINIBID",], aes(factor(num_players_over_5perc),localdn_savings_rate))
g + geom_point(aes(color = reason), size = 4, alpha =1/2) + geom_smooth() + labs(title = "Processing days vs savings") + labs(y = "Local DNAC", x = "Count processing days")  + facet_grid(member~.) + coord_cartesian(ylim = c(0, 1), xlim = c(0,10)) + theme_bw(base_family = "Times")


# cutpoints
## calculate the deciles of the data
cutpoint <- quantile(bidlog$localdn_savings_rate, seq(0,1, length = 4), na.rm = TRUE)

## cut the data at the deciles and a create a new factor variable
bidlog$f_savings <- cut(bidlog$localdn_savings_rate, cutpoint)
levels(bidlog$f_savings)
