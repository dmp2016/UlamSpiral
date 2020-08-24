library(ggplot2)


x = (0:8) %/% 3
y = (0:8) %% 3


num_to_xy <- function(n){
  L <- ceiling((sqrt(n) - 1) / 2)
  print(L)
  d <- L * 2 - 1
  r <- (n - d*d)
  r[L > 0] = r[L > 0] %% (8*L[L > 0])
  print(r)
  part <- r %/% (2*L)
  x <- rep(0, length(part))
  p <- 2*L
  print(part)
  x[part == 0] = L[part == 0]
  x[part == 1] = L[part == 1] - (r[part == 1] - p[part == 1])
  x[part == 2] = -L[part == 2]
  x[part == 3] = -L[part == 3] + (r[part == 3] - 3*p[part == 3])
  y = rep(0, length(part))
  y[part == 0] = -L[part == 0] + r[part == 0]
  y[part == 1] = L[part == 1]
  y[part == 2] = L[part == 2] - (r[part == 2] - 2*p[part == 2])
  y[part == 3] = -L[part == 3]
  return(data.frame(x, y))
}

num_to_xy(c(1, 4, 9, 12, 15, 16, 17, 25, 26))

switch(c(1, 3, 1, 4), 0, 1, 2, 3, 4)

d <- 1
d[d < 0] = 0
d


  
sqrt(5))

mapply(c, replicate(3, 1:3))

ggplot(mapping = aes(x = x, y = y)) + 
  geom_point()

list

ceiling()