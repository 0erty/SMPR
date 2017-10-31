v <- iris[, 3:4]

p <- c(3, 1)


avg <- function(x)
  
{
  sum(x) / length(x)
  
}


colors <-
  c("setosa" = "red",
    "versicolor" = "green",
    "virginica" = "blue")
ax <- avg(iris[iris$Species == "setosa", 3])

ay <- avg(iris[iris$Species == "setosa", 4])

bx <- avg(iris[iris$Species == "versicolor", 3])

by <- avg(iris[iris$Species == "versicolor", 4])

cx <- avg(iris[iris$Species == "virginica", 3])

cy <- avg(iris[iris$Species == "virginica", 4])


plot(iris[, 3:4],
     pch = 21,
     bg = colors[iris$Species],
     col = colors[iris$Species])

points(ax, ay, pch = 20, col = "black")

points(bx, by, pch = 20, col = "black")

points(cx, cy, pch = 20, col = "black")

points(p, pch = 20, col = "yellow", lwd = 9)

dist <- function(u, v)
  
{
  sqrt(sum((u - v) ^ 2))
  
}

a <- dist(c(ax, ay), p)

b <- dist(c(bx, by), p)

c <- dist(c(cx, cy), p)

min(c(a, b, c))