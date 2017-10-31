euclideanDistance <- function(u, v) {
  sqrt(sum((u - v) ^ 2))
}

sortObjectsByDist <- function(xl, z, metricFunction = euclideanDistance)
{
  l <- dim(xl)[1] 
  n <- dim(xl)[2] - 1
  
  distances <- matrix(NA, l, 2) 
  for (i in 1:l) {
    distances[i,] <- c(i, metricFunction(xl[i, 1:n], z))
  }
  
  orderedXl <- xl[order(distances[, 2]),] 
  return (orderedXl)
  
}

kNN <- function(xl, z, k) {
  orderedXl <- sortObjectsByDist(xl, z, euclideanDistance) 
  n <- dim(orderedXl)[2] - 1 
  classes <-orderedXl[1:k, n + 1] 
  counts <- table(classes)
  class <- names(which.max(counts)) 
  return (class)
}

plot(
  iris[, 3:4],
  pch = 21,
  bg = colors[iris$Species],
  col = colors[iris$Species],
  asp = 1
)

z <-c(2.7, 1) 
xl <-iris[, 3:5] 
class <-kNN(xl, z, k = 6) 
points(z[1], z[2], pch = 22, bg = colors[class], col = colors[class], asp = 1, lwd = 5)


  i <- 1 
  tmp <- 0 
  sectmp <- 1
  k <-1 
  arr <- c(seq(1,50)) 
  while (k <= 50){ 
    while (i <= 150){ 
      if (iris[i,5] != kNN(iris[-i, 3:5], c(iris[i,3], iris[i,4]), k)){ 
        tmp = tmp + 1 
      } 
      i = i + 1 
    } 
    arr[sectmp] <- tmp/150 
    k = k + 1 
    tmp = 0 
    i = 1
    sectmp = sectmp + 1
  } 
  
  print(arr)
  plot(c(seq(1,5)),arr,type="l",ylab="Error Rate",
       xlab="K",main="Error Rate for Iris", ylim = c(0.4, 0.6))

  arr2 <- (seq(1,50))
  plot(arr2, arr, type = "l")