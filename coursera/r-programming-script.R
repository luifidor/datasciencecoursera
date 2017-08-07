
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
