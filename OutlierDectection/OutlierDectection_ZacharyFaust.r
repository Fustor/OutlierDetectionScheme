#Zachary Faust
#Data Mining - HW4

library(readr)
Complex8_N15 <- read_csv("D:/Dropbox/Dropbox/DataMining/HW4/Complex8_N15.csv")

#Gets distance between 2 objects
distanceFun <- function(u,v) {
    distance <- sqrt((u$V1 - v$V1)^2 + (u$V2 - v$V2)^2)
    distance
} 

#Finds the 3 closest objects and totals the distance to them
findDist <- function(u,v) {
    distances <- double()
    total <- 0
    for(i in 1:nrow(v)) {
        distances <- c(distances,distanceFun(u,v[i,]))
    }
    distances <- distances[distances != 0]
    total <- total + distances[which.min(distances)]
    distances <- distances[-which.min(distances)]
    total <- total + distances[which.min(distances)]
    distances <- distances[-which.min(distances)]
    total <- total + distances[which.min(distances)]
    distances <- distances[-which.min(distances)]
    total
}

#Runs findDist for all rows
findAllDist <- function(u) {
    distVect <- double()
    for(i in 1:nrow(u)) {
        distVect <- c(distVect,findDist(u[i,],u))
    }
    distVect
}

#Finding all totals for 3 closest distances
distanceVector <- findAllDist(Complex8_N15)

#Adding ols and sorting
ComplexOSL <- Complex8_N15
ComplexOSL$ols <- distanceVector
ComplexOSLSorted <- ComplexOSL[order(-ComplexOSL$ols),]

#Plot 7%
SevenPlotHead <- head(ComplexOSLSorted,.07*nrow(ComplexOSLSorted))
SevenPlotTail <- tail(ComplexOSLSorted,.93*nrow(ComplexOSLSorted))
plot(SevenPlotHead$V1, SevenPlotHead$V2, col=c("red","blue","brown","#00CC00","#003399","#FF33FF","#FF0033","#660066")[ComplexOSLSorted$V3],asp=1,xlim=c(-350, 1100), ylim=c(-200, 700))
plot(SevenPlotTail$V1, SevenPlotTail$V2, col=c("red","blue","brown","#00CC00","#003399","#FF33FF","#FF0033","#660066")[ComplexOSLSorted$V3],asp=1,xlim=c(-200, 1100), ylim=c(-200, 700))

#Plot 14%
FourteenPlotHead <- head(ComplexOSLSorted,.14*nrow(ComplexOSLSorted))
FourteenPlotTail <- tail(ComplexOSLSorted,.86*nrow(ComplexOSLSorted))
plot(FourteenPlotHead$V1, FourteenPlotHead$V2, col=c("red","blue","brown","#00CC00","#003399","#FF33FF","#FF0033","#660066")[ComplexOSLSorted$V3],asp=1,xlim=c(-350, 1100), ylim=c(-200, 700))
plot(FourteenPlotTail$V1, FourteenPlotTail$V2, col=c("red","blue","brown","#00CC00","#003399","#FF33FF","#FF0033","#660066")[ComplexOSLSorted$V3],asp=1,xlim=c(-350, 1100), ylim=c(-200, 700))

#Plot 21%
TwoOnePlotHead <- head(ComplexOSLSorted,.21*nrow(ComplexOSLSorted))
TwoOnePlotTail <- tail(ComplexOSLSorted,.79*nrow(ComplexOSLSorted))
plot(TwoOnePlotHead$V1, TwoOnePlotHead$V2, col=c("red","blue","brown","#00CC00","#003399","#FF33FF","#FF0033","#660066")[ComplexOSLSorted$V3],xlim=c(-350, 1100), ylim=c(-200, 700))
plot(TwoOnePlotTail$V1, TwoOnePlotTail$V2, col=c("red","blue","brown","#00CC00","#003399","#FF33FF","#FF0033","#660066")[ComplexOSLSorted$V3],xlim=c(-350, 1100), ylim=c(-200, 700))

#Histogram
hist(TwoOnePlotHead$ols)