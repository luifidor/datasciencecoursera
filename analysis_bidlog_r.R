


#retrieve all incumbent lines
inc <- subset(bidline, offer_type == "Inc", select = c(bid_id, snapshot, member, line_type, vendor, award, ndc, indirect_cost, contract_cost, locadn_cost, allindn_cost, scenario, item_num, offerid, annual_qty))
colnames(inc) <- paste("inc", colnames(inc), sep = '_')

#retrieve all offers lines
offers <- subset(bidline, offer_type == "Offer" & award == 0, select = c(bid_id, snapshot, member, line_type, vendor, award, ndc, indirect_cost, contract_cost, locadn_cost, allindn_cost, scenario, item_num, offerid, annual_qty))
colnames(offers) <- paste("offer", colnames(offers), sep = '_')

#retrieve all award lines
awards <- subset(bidline, award >= 0.75, select = c(bid_id, snapshot, member, line_type, vendor, award, ndc, indirect_cost, contract_cost, locadn_cost, allindn_cost, scenario, item_num, offerid, annual_qty))
colnames(awards) <- paste("aw", colnames(awards), sep = '_')


#merge lines incumbent with offer lines
lines_merged <- merge.data.frame (offers, inc[, c("inc_ndc", "inc_vendor", "inc_award", "inc_indirect_cost", "inc_contract_cost", "inc_locadn_cost", "inc_allindn_cost", "inc_bid_id", "inc_item_num")], by.y = c("inc_bid_id", "inc_item_num"), by.x=c("offer_bid_id", "offer_item_num"))
head(lines_merged,5)

#merge consolidated lines with awards
lines_awarded <- merge.data.frame (lines_merged, awards[, c("aw_ndc", "aw_vendor", "aw_award", "aw_indirect_cost", "aw_contract_cost", "aw_locadn_cost", "aw_allindn_cost", "aw_bid_id", "aw_item_num")], by.y = c("aw_bid_id", "aw_item_num"), by.x=c("offer_bid_id", "offer_item_num"))


#1) analyze how many offers are not awarded when their allincost is lower than the awarded item
offer_lowest_allincost <- subset(lines_awarded, offer_award == 0 & offer_allindn_cost < aw_allindn_cost)
head(offer_lowest_allincost)


diff <-  offer_lowest_allincost$aw_allindn_cost -  offer_lowest_allincost$offer_allindn_cost
value <- offer_lowest_allincost$offer_annual_qty * diff

length(diff)
length(value)
length(analysis)
consolidated <- cbind(offer_lowest_allincost, diff = as.data.frame(diff), value = as.data.frame(value))

hist(value, breaks = 2000, xlim = c(0, 1e6))

consolidated[order(-consolidated$value), c("offer_bid_id", "offer_snapshot", "offer_allindn_cost", "aw_allindn_cost", "value", "offer_vendor", "aw_vendor")]

factor(bidlog$member)
plot(factor(bidlog$member), log10(bidlog$localdn_savings_rate))

library(ggplot2)
q <-qplot(factor(bidlog$reason),log10(bidlog$localdn_savings_rate), alpha=I(0.1))
q + theme(axis.text.x = element_text(angle = 90, hjust = 1))


negative <- bidlog$localdn_savings_rate < 0
mean(negative, na.rm = T)


##
## Analysis: Study how per year the median and mean savings have moved around with processing days
##
## - Get median localdn vector per year per reason
## - Get median processing days per year per reason
##

r_to_consider <- c("MINIBID", "COMPETITIVE", "MINIBID_QUIET","NEW_OFFER")

bidlog_reduce <- subset(bidlog, reason %in% r_to_consider & (member == "ABC" | member == "WAG"))

competitive <- subset(bidlog, open_date_cy == 2017 & reason == "COMPETITIVE", localdn_savings_rate)
summary (competitive)
minibid <- subset(bidlog_reduce , open_date_cy == 2017 & reason == "MINIBID", localdn_savings_rate)
summary (minibid)
mean(minibid$localdn_savings_rate)

ml15 <- with(bidlog_reduce[bidlog_reduce$open_date_cy == 2015, ], tapply(localdn_savings_rate, reason, median, na.rm = T))
ml16 <- with(bidlog_reduce[bidlog_reduce$open_date_cy == 2016, ], tapply(localdn_savings_rate, reason, median, na.rm = T))
ml17 <- with(bidlog_reduce[bidlog_reduce$open_date_cy == 2017, ], tapply(localdn_savings_rate, reason, median, na.rm = T))


pd15 <- with(bidlog_reduce[bidlog_reduce$open_date_cy == 2015, ], tapply(count_processing_days, reason, median, na.rm = T))
pd16 <- with(bidlog_reduce[bidlog_reduce$open_date_cy == 2016, ], tapply(count_processing_days, reason, median, na.rm = T))
pd17 <- with(bidlog_reduce[bidlog_reduce$open_date_cy == 2017, ], tapply(count_processing_days, reason, median, na.rm = T))


## it is not showing the data as I want it to be, there is an error somewhere
par(mfrow = c(2,3), mar = c(5, 7.5, 2, 2))
plot(pd15, ml15, pch = 19, xlim = c(0,100),ylim=c(0, 0.4), main = "2015", xlab = "Median Processing Days", ylab = "Median LocalDN Savings Rate")
text(pd15 + 2 , ml15, names(pd15), cex =  0.8, pos = 3)
plot(pd16, ml16, pch = 19, xlim = c(0,100),ylim=c(0, 0.4),main = "2016",  xlab = "Median Processing Days", ylab = "Median LocalDN Savings Rate")
text(pd16 + 2, ml16,names(pd16), cex =  0.8, pos = 3)
plot(pd17, ml17, pch = 19, xlim = c(0,100),ylim=c(0, 0.4), main = "2017", xlab = "Median Processing Days", ylab = "Median LocalDN Savings Rate")
text(pd17+2, ml17,labels = names(ml17), cex =  0.8, pos = 3)

## Count the number of occurances per reason
library(data.table)
bd <- data.table(bidlog_reduce)
x <- bd[,.N, by = list(open_date_cy, reason)]
x$label <- paste(x$open_date_cy, x$reason, sep = ".")

## Extract the data ranges per year
n15 <- subset(x[order(reason, open_date_cy),], open_date_cy == 2015)
n16 <- subset(x[order(reason, open_date_cy),], open_date_cy == 2016)
n17 <- subset(x[order(reason, open_date_cy),], open_date_cy == 2017)
barplot(n15$N, names.arg = n15$reason, horiz=T, las = 2, xlim = c(0, 700), xlab = "Count of bids")
barplot(n16$N, names.arg = n16$reason, horiz=T, las = 2, xlim = c(0, 700), xlab = "Count of bids")
barplot(n17$N, names.arg = n17$reason, horiz=T, las = 2, xlim = c(0, 700), xlab = "Count of bids")

install.packages ("plyr")
library(plyr)