library(ggplot2)
install.packages("matlab")
library(matlab)


num_to_xy <- function(n){
  L <- ceiling((sqrt(n) - 1) / 2)
  d <- L * 2 - 1
  r <- (n - d*d)
  r[L > 0] = r[L > 0] %% (8*L[L > 0])
  part <- r %/% (2*L)
  x <- rep(0, length(part))
  p <- 2*L
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

k <- 50000

nums <- 2:k

primes <- nums[as.logical(isprime(nums))]
hist(primes, breaks = 50)

df <- num_to_xy(primes)


ggplot(mapping = aes(x = df$x, y = df$y)) + 
  geom_point(col = "red", size = 0.2)
  

probs = rep(1, k - 1) / log(2:k)
p_sum = sum(probs)
r_nums <- sample(2:k, size = k/log(k), prob = probs/p_sum)
r_nums <- sort(r_nums)
hist(r_nums, breaks = 50)

r_df <- num_to_xy(r_nums)

ggplot(mapping = aes(x = r_df$x, y = r_df$y)) + 
  geom_point(col = "red", size = 0.2)

c(length(r_nums), k/log(k))
