


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