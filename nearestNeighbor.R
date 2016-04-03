# Nearest Neighbor method
# Define a Decision-Boundary
# 2016/4/3
frame()

cat('Please input two coordinate points.\n')
x1 <- readline()
x1 <- as.numeric(unlist(strsplit(x1, ',')))

x2 <- readline()
x2 <- as.numeric(unlist(strsplit(x2, ',')))

cols <- c('red', 'blue', 'black')

X <- cbind(x1, x2)

xRange <- abs(X[,1]-X[,2])[1]
yRange <- abs(X[,1]-X[,2])[2]

if (xRange > yRange) {
  xlims <- c(min(X[1,])-3, max(X[1,])+3)
  ylims <- c(min(X[2,])-((xRange-yRange)/2+3), max(X[2,])+((xRange-yRange)/2+3))
} else {
  xlims <- c(min(X[2,])-((yRange-xRange)/2+3), max(X[2,])+((yRange-xRange)/2+3))
  ylims <- c(min(X[1,])-3, max(X[1,])+3)
}
plot(0,0, cex=0, xlim=xlims, ylim=ylims)
points(X[1,][1], X[2,][1], col=cols[1], pch=16, cex=2)
points(X[1,][2], X[2,][2], col=cols[2], pch=16, cex=2)

if (X[1,1]-X[1,2] == 0) {
  abline(h=(X[2,1]+X[2,2])/2)
} else if (X[2,1]-X[2,2] == 0) {
  abline(v=(X[1,1]+X[1,2])/2)
} else {
  a <- (X[1,1]-X[1,2])/(-X[2,1]+X[2,2])
  b <- (-X[1,1]^2-X[2,1]^2+X[1,2]^2+X[2,2]^2)/(-2*(X[2,1]-X[2,2]))
  abline(b, a)
}

class <- function(x, class) {
  points(x[1], x[2], pch=16, col=cols[class], cex=2)
  cat('Class')
  print(class)
}

repeat {
  x <- readline()
  x <- as.numeric(unlist(strsplit(x, ',')))

  # Judge which regions
  y <- (x[1]-X[1,1])^2+(x[2]-X[2,1])^2-(x[1]-X[1,2])^2-(x[2]-X[2,2])^2
  if (y == 0) {
    class(x,3)
  } else if (y > 0) {
    if (sign(-X[2,1]+X[2,2]) == 1) {
      class(x,1)
    } else {
      class(x,2)
    }
  } else {
    if (sign(-X[2,1]+X[2,2]) == -1) {
      class(x,1)
    } else {
      class(x,2)
    }
  }
}
