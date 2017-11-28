parcenWindowFixed <- function(xl,z,h){
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1     
 
# Получаем список классов к которым относятся все объекты xl
  list <- unique(xl[,n+1])
  counts = 0
  for(i in 1:length(list)){
    counts[i] = 0
  }
  for (i in 1:l)
  {
    counts[which(xl[i,n+1]==list)] =
      counts[which(xl[i,n+1]==list)] + kernel(euclideanDistance(z,xl[i,1:n])/h)
  }
  print(counts)
  return (list[which.max(counts)]) 
  
}
parcenWindowFloat <- function(xl,z,k){
  orderedXl <- sortObjectsByDist(xl, z)
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1          
  list <- unique(xl[,n+1])
  h <- euclideanDistance(z,orderedXl[k+1,1:n])
  counts = 0
  for(i in 1:length(list)){
    counts[i] = 0
  }
  
#Вычисляем сумму весов объектов
  for (i in 1:l)
  {
    counts[which(xl[i,n+1]==list)] =
      counts[which(xl[i,n+1]==list)] + kernel(euclideanDistance(z,xl[i,1:n])/h)
  }
  print(counts)
# Класс с максимальным весом  
  return (list[which.max(counts)]) 
  
}

colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue") 
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)  
z <- c(2.7, 1) 
xl <- iris[, 3:5] 
class <- parcenWindowFloat(xl, z, 8)
res <- loo(xl,5,parcenWindowFloat)
print(res)
points(z[1], z[2], pch = 22, bg = colors[class], asp = 1) 
