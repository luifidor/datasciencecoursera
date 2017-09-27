# Lesson 3: Dimension Reduction (part 1)
set.seed(12345)
par(mar = rep(0.2,4))
dataMatrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(dataMatrix) [, nrow(dataMatrix):1])
heatmap(dataMatrix) #shows no pattern

##Add pattern to the matrix
set.seed(678910)
for(i in 1:40) {
        # flip a coin
        coinFlip <- rbinom(1, size = 1, prob = 0.5)
        # if coin is heads add a common pattern to that row
        if (coinFlip) {
                dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0,3), each = 5)
        }
}

## Patterns in rows and columns
hh <- hclust(dist(dataMatrix)) ## cluster analysis hierar
dataMatrixOrdered <- dataMatrix[hh$order,]
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered), 40:1, , xlab = "Row Mean", ylab = "Row", pch = 19)
plot(colMeans(dataMatrixOrdered), xlab = "Column", ylab = "Column Mean", pch = 19)

## Componenets of the SVD - u and v
sdv1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(sdv1$u[,1], 40:1, , xlab = "Row", ylab = "First left singular vector", pch  = 19)
plot(sdv1$v[,1], xlab = "Column", ylab = "First right singular vector", pch = 19)

## Components of the SVD
par(mfrow = c(1,2))
plot(sdv1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(sdv1$d^2/sum(sdv1$d^2), xlab = "Column", ylab = "Singular value", pch = 19)

## Relationship to principal components
sdv1 <- svd(scale(dataMatrixOrdered))
pca1 <- prcomp(dataMatrixOrdered, scale = TRUE)
plot(pca1$rotation[,1], sdv1$v[,1], pch = 19, xlab = "Principal Component 1", ylab = "Right Singular Vector 1")
abline(c(0,1))

## add some variablity

set.seed(678910)

for (i in 1:40) {
        coinFlip1 <- rbinom(1, size = 1, prob = 0.5)
        coinFlip2 <- rbinom(1, size = 1, prob = 0.5)
        
        if (coinFlip1) {
                dataMatrix[i,] <- dataMatrix[i, ] + rep(c(0,5), each = 5)
        }
        if (coinFlip2) {
                dataMatrix[i,] <- dataMatrix[i, ] + rep(c(0,5), 5)
        }
hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix [hh$order,]
}

## Imputing {inpute} - Get rid of missing values
library(impute) ## Available from http://bioconductor.org
dataMatrix2 <- dataMatrixOrdered
dataMatrix2[sample(1:100, size=40, replace=FALSE)] <- NA
dataMatrix2 <- imput.knn(dataMatrix2$data)$data
svd1 <- svd(scale(dataMatrixOrdered)); svd2 <- svd(scale(dataMatrix2))

scale(bidlog$award_localdn_var)

# coloRamp
# return colors across the two colors, number between 0 and 1
pal <- colorRamp (c("red", "blue"))
pal(0)
pal(0.5)
pal(1)
image(as.matrix(1:10), colors = pal(0.5))
pal(seq(0,1,len  = 10))

pal <- colorRampPalette(c("red", "yellow")) #creates a  number of steps depending on the integer provided to the function
pal(3)


## RColorBrewerPackage
# Sequential is useful for ordered data
# Diverging, items diverging from a mean
# Qualitative, data that is not ordered
# it can be used in conjuctions with colorRamp ColorRampPalet

install.packages("RColorBrewer")
library (RColorBrewer)
cols <- brewer.pal(5, "YlOrRd")
pal <- colorRampPalette(cols)
image(volcano, col = pal(30))

## creates a density cloud using the blue palette
x <- rnorm(1000)
y <- rnorm(1000)
smoothScatter(x,y)

smoothScatter(log(bidlog$count_processing_days), log(bidlog$localdn_savings_rate))
smoothScatter(bidlog$count_processing_days, bidlog$localdn_savings_rate, xlim = c(0, 60), ylim = c(-1, 1),  colramp = pal)
smoothScatter(bidlog$localdn_savings_rate)
smoothScatter(bidlog$count_processing_days)

# RGB it allows to translate decimal to hexadecimal, alpha parameter can be used to add transparency
rgb(255, 0, 0, 0.5)
plot(x, y, col = rgb(0,0,0,0.2), pch = 19) #points become transparent and it is easier to see them, cheap way to get an histogram, density
