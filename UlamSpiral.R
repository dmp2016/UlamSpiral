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



k <- 10000

!(1:5 == 3)

primes2 <- (1:k)
primes2 <- (1:k) * 3 + 1
primes2 <- (1:k) * 8 + 1
primes2 <- (1:k) * 11 + 1
primes2 <- (1:k) * 12 + 1
primes2 <- (1:k) * 13 + 1
primes2 <- (1:k) * 14 + 1
primes2 <- (1:k) * 15 + 1
primes2 <- (1:k) * 16 + 1
primes2 <- (1:k) * 32 + 1
primes2 <- (1:k) * 64 + 1
primes2 <- (1:k) * 60 + 1
primes2 <- (1:k) * 91 + 1
primes2 <- primes2[as.logical(isprime(primes2))]


primes <- (1:k)[as.logical(isprime(1:k))]
df <- num_to_xy(primes)

primes3 <- (1:k)
primes3 <- primes3[((1:k) %% 23 != 1)]
primes3 <- primes3[as.logical(isprime(primes3))]
df3 <- num_to_xy(primes3)

primes4 <- (1:k)
primes4 <- primes4[((1:k) %% 23 == 1)]
primes4 <- primes4[as.logical(isprime(primes4))]
df4 <- num_to_xy(primes4)

length(primes4)
length(primes3)

ggplot() + 
  geom_point(mapping = aes(x = df3$x, y = df3$y), 
             col = "red", size = 1) + 
  geom_point(mapping = aes(x = df4$x, y = df4$y), 
             col = "blue", size = 1)


ggplot() + 
  geom_point(mapping = aes(x = df3$x, y = df3$y), 
             col = "red", size = 1) +
  geom_point(mapping = aes(x = 1:as.integer(k^0.5) - as.integer(k^0.5) %/% 2, y = -(1:as.integer(k^0.5) - as.integer(k^0.5) %/% 2) + 10), col = "black", size = 1)

  

ggplot() + 
  geom_point(mapping = aes(x = df4$x, y = df4$y), 
             col = "blue", size = 0.2)


length(df$x)
ggplot(mapping = aes(x = df$x, y = df$y)) + 
  geom_point(col = "red", size = 0.2)
  

k1 <- k %/% 10
probs = 1 / log(2:k1)
p_sum = sum(probs)
r_nums <- sample(2:k1, size = k1/log(k1), prob = probs/p_sum)
r_nums <- sort(r_nums)

length(r_nums)


hist(r_nums, breaks = 50)

r_df <- num_to_xy(r_nums)

ggplot(mapping = aes(x = r_df$x, y = r_df$y)) + 
  geom_point(col = "red", size = 0.2)

c(length(r_nums), k/log(k))

k <- 5000


pol2 <- function(x) return(a*x^2 + b*x + c)


a <- 1
b <- 2
c <- 0

pol_v <- pol2(1:k)
pol_v <- pol_v[pol_v <= k]
# pol_v <- pol_v[as.logical(isprime(pol_v))]

pol_df1 <- num_to_xy(pol_v)

a <- 1
b <- 1
c <- 0


pol_v <- pol2(1:k)
pol_v <- pol_v[pol_v <= k]
# pol_v <- pol_v[as.logical(isprime(pol_v))]

pol_df2 <- num_to_xy(pol_v)


ggplot() + 
  geom_point(mapping = aes(x = pol_df1$x, y = pol_df1$y), col = "black", size = 0.2) +
  geom_point(mapping = aes(x = pol_df2$x, y = pol_df2$y), col = "red", size = 0.2)


