

outcome <- read.csv("\\\\wbad.group\\dfs_WB\\10_Home\\UWBBENOELF\\DATA\\Desktop\\GIT Folder\\coursera\\coursera\\outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
nrow(outcome)

outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11])

best <- function(state, outcome) {
        ## Read outcome data
        
        ## Check that state and outcome are valid
        
        ## Return hospital name is tha tstate with lowest 30 day death
        ## rate
        
        # "heart attack" , "heart failure" and "pneumonia"
        outcome_id = NULL
        
        # check if outcome measure is valid
        if (outcome == "heart attack") {
                outcome_id = 11
        } else if (outcome == "heart failure") {
                outcome_id = 17
        } else if ( outcome == "pneumonia") {
                outcome_id = 23
        } else {
                stop ("Not a valid outcome measure")
        }
        
        # load relevant data
        data <- read.csv("\\\\wbad.group\\dfs_WB\\10_Home\\UWBBENOELF\\DATA\\Desktop\\GIT Folder\\coursera\\coursera\\outcome-of-care-measures.csv", colClasses = "character")
        suppressWarnings(data[,outcome_id] <- as.numeric(data[,outcome_id]))
        
        # check if state is valid
        if (is.na(match(state, data[,7]))) {
                stop ("Not a valid state")
        }
        
        state_data <- data[data$State == state,]
        #state_data[which.min(state_data[,outcome_id]),2]
        ordered_data <- state_data[order(state_data[,outcome_id], state_data[,2]),]
        ordered_data[1,2]

}

rankhospital <- function(state, outcome, num) {
        ## Read outcome data
        
        ## Check that state and outcome are valid
        
        ## Return hospital name is tha tstate with lowest 30 day death
        ## rate
        
        # "heart attack" , "heart failure" and "pneumonia"
        outcome_id = NULL
        
        # check if outcome measure is valid
        if (outcome == "heart attack") {
                outcome_id = 11
        } else if (outcome == "heart failure") {
                outcome_id = 17
        } else if ( outcome == "pneumonia") {
                outcome_id = 23
        } else {
                stop ("Not a valid outcome measure")
        }
        
        # load relevant data
        data <- read.csv("\\\\wbad.group\\dfs_WB\\10_Home\\UWBBENOELF\\DATA\\Desktop\\GIT Folder\\coursera\\coursera\\outcome-of-care-measures.csv", colClasses = "character")
        suppressWarnings(data[,outcome_id] <- as.numeric(data[,outcome_id]))
        
        # check if state is valid
        if (is.na(match(state, data[,7]))) {
                stop ("Not a valid state")
        }
        
        #filter by state and order the data ascending
        state_data <- data[data$State == state & complete.cases(data[,outcome_id]),]
        ordered_data <- state_data[order(state_data[,outcome_id], state_data[,2]),]
        
        return_id = NULL
        
        #return results
        if (num == "best") {
                return_id = 1
        } else if (num == "worst") {
                return_id = nrow(ordered_data)
        } else {
                return_id = as.numeric(num)
        }
        r <- cbind(ordered_data[return_id, c(2, outcome_id)], return_id)
        colnames(r) <- c("Hospital", "Rate", "Rank")
        if (is.na(r[1,1])) {
                NA
        } else {
                r
        }
        
}
rankhospital("MD", "heart attack", "worst")
rankhospital("TX", "heart failure", 4)
rankhospital("MN", "heart attack", 5000)

rankall <- function(outcome, num) {
        ## Read outcome data
        
        ## Check that state and outcome are valid
        
        ## Return hospital name is tha tstate with lowest 30 day death
        ## rate
        
        # "heart attack" , "heart failure" and "pneumonia"
        outcome_id = NULL
        
        # check if outcome measure is valid
        if (outcome == "heart attack") {
                outcome_id = 11
        } else if (outcome == "heart failure") {
                outcome_id = 17
        } else if ( outcome == "pneumonia") {
                outcome_id = 23
        } else {
                stop ("Not a valid outcome measure")
        }
        
        # load relevant data
        data <- read.csv("\\\\wbad.group\\dfs_WB\\10_Home\\UWBBENOELF\\DATA\\Desktop\\GIT Folder\\coursera\\coursera\\outcome-of-care-measures.csv", colClasses = "character")
        suppressWarnings(data[,outcome_id] <- as.numeric(data[,outcome_id]))
        
        # check if state is valid
        if (is.na(match(state, data[,7]))) {
                stop ("Not a valid state")
        }
        
        #filter by state and order the data ascending
        data <- data[complete.cases(data[,outcome_id]),]
        
        rank_v <- sapply(split(data, data$State), function(x) {order(x[,outcome_id], x[,2])})
        
        a <- NULL
        
        for (i in 1:length(rank_v)) {
                a <- c(a,rank_v[[i]])
        }
        rank_v <- a
        ranked_data <- cbind(data, rank=rank_v)
        
        #return results
        if (num == "best") {
                r <- ranked_data[ranked_data$rank == 1,c(7,2, outcome_id)]
        } else if (num == "worst") {
                worst_rank <- lapply(split(ranked_data,ranked_data$State), function(x) {x[match(max(x$rank), x$rank),c(7,2, outcome_id)]})
                a <- NULL
                for (i in 1:length(worst_rank)) {
                        if (is.null(a)) {
                                a <- worst_rank[[i]]
                        } else {
                        a <- rbind(a,worst_rank[[i]])
                        }
                r <- a
                }
        } else {
                r <- ranked_data[ranked_data$rank == as.numeric(num),c(7,2, outcome_id)]
        }
        
        colnames(r) <- c("State", "Hospital", "Rate")
        r
        
}
install.packages("dplyr")
library(dplyr)

r <- sapply(split(outcome, outcome$State), function(x) {order(x$State, x[,11], x[,2])})
as.data.frame(do.call(rbind, r))
r$AK
r[[54]]
length(r)
a<- NULL
for (i in 1:length(r)) {
        a <- c(a,r[[i]])
}

rank_v <- sapply(split(outcome, outcome$State), function(x) {order(x$State, x[,11], x[,2])})
rev_v <- sapply(split(outcome[,11], outcome$State),max, na.rm = TRUE)


head(rankall("heart attack", 20), 10)