library(ggplot2)
library(dplyr)
install.packages("matlab")
library(matlab)

num_to_xy <- function(n){
  # Точки располагаются на сторонах квадратов с нечётными длинами сторон 2*sn + 1,
  # где sn - номер квадрата, начиная с нуля. 
  # Откуда находим номер квадрата, на стороне которого находится n:
  sn <- ceiling((sqrt(n) - 1) / 2)
  # Количество точек внутри квадрата с номером sn равно количеству точек в квадрате
  # с предыдущим номером или его площади. Так как сторона квадрата 
  # такого квадрата равна (sn - 1) * 2 + 1 = 2*sn - 1, то количество внутренних 
  # точек равно (2*sn - 1) ^ 2
  cnt_internal <- (2*sn - 1) ^ 2
  # Для sn == 0 получим cnt_internal == -1, скорретируем это значение вручную.
  cnt_internal[n == 0] = 0
  # Номер точки на границе текущего квадрата. Номер 1 соответствует правому
  # нижнему углу.
  residual <- (n - cnt_internal)
  residual[sn > 0] = residual[sn > 0] %% (8 * sn[sn > 0])
  part <- residual %/% (2*sn)
  p <- 2*sn
  x <- rep(0, length(part))
  x[part == 0] = sn[part == 0]
  x[part == 1] = sn[part == 1] - (residual[part == 1] - p[part == 1])
  x[part == 2] = -sn[part == 2]
  x[part == 3] = -sn[part == 3] + (residual[part == 3] - 3*p[part == 3])
  y = rep(0, length(part))
  y[part == 0] = -sn[part == 0] + residual[part == 0]
  y[part == 1] = sn[part == 1]
  y[part == 2] = sn[part == 2] - (residual[part == 2] - 2*p[part == 2])
  y[part == 3] = -sn[part == 3]
  return(data.frame(x, y))
}


# Размер скатерти (от нуля до ...)
K <- 150

# Чисел на скатерти
N <- (2*K + 1)^2

# Простые числа на скатерти Улама
prime_nums <- primes(N)
df_prime <- num_to_xy(prime_nums)

ggplot() + 
  geom_point(mapping = aes(x = df_prime$x, y = df_prime$y), 
             col = "red", size = 0.5) +
  scale_x_continuous(limits = c(-K, K), breaks = seq(-K, K, 50)) +
  scale_y_continuous(limits = c(-K, K), breaks = seq(-K, K, 50)) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Простые числа на скатерти Улама")


nrow(df_prime)/N
1/log(N)

# Случайные числа на скатерти Улама
rand_nums <- sample(2:N, length(prime_nums))
df_rand <- num_to_xy(rand_nums)

ggplot() + 
  geom_point(mapping = aes(x = df_rand$x, y = df_rand$y), 
             col = "red", size = 0.5) +
  scale_x_continuous(limits = c(-K, K), breaks = seq(-K, K, 50)) +
  scale_y_continuous(limits = c(-K, K), breaks = seq(-K, K, 50)) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Случайные числа на скатерти Улама")


# Размер выборки, не более 2*K
SAMPLE_SIZE <- 100

# Подсчет попавшей в заданный датасет доли выборки из SAMPLE_SIZE случайных точек
rand_sample_part <- function(df) {
  # Точки могут повторяться, реальный размер выборки может оказаться немного меньше SAMPLE_SIZE
  df_sample <- data.frame(x = sample(-K:K, size = SAMPLE_SIZE, replace = TRUE), 
                          y = sample(-K:K, size = SAMPLE_SIZE, replace = TRUE)) %>% distinct()
  return(nrow(merge(df_sample, df))/nrow(df_sample))
}

prime_parts <- replicate(300, rand_sample_part(df_prime))
hist(prime_parts)
mean(prime_parts)
sd(prime_parts)

rand_parts <- replicate(300, rand_sample_part(df_rand))
mean(rand_parts)
hist(rand_parts)
sd(rand_parts)


# Подсчет попавшей в заданный датасет доли выборки из SAMPLE_SIZE случайных точек, лежащих на 
# случайном вертикальном отрезке
vert_sample_part <- function(df) {
  # Точки могут повторяться, реальный размер выборки может оказаться немного меньше SAMPLE_SIZE
  df_sample <- data.frame(x = sample(-K:K, size = 1),
                          y = sample(-K:K, size = SAMPLE_SIZE))
  return(nrow(merge(df_sample, df))/nrow(df_sample))
}

vert_prime_parts <- replicate(300, vert_sample_part(df_prime))
hist(vert_prime_parts)
mean(vert_prime_parts)
sd(vert_prime_parts)

# Подсчет попавшей в заданный датасет доли выборки из SAMPLE_SIZE случайных точек, лежащих на 
# случайном горизонтальном отрезке
horiz_sample_part <- function(df) {
  # Точки могут повторяться, реальный размер выборки может оказаться немного меньше SAMPLE_SIZE
  df_sample <- data.frame(x = sample(-K:K, size = SAMPLE_SIZE),
                          y = sample(-K:K, size = 1))
  return(nrow(merge(df_sample, df))/nrow(df_sample))
}


horiz_prime_parts <- replicate(300, vert_sample_part(df_prime))
hist(horiz_prime_parts)
mean(horiz_prime_parts)
sd(horiz_prime_parts)


# Подсчет попавшей в заданный датасет доли выборки из 100 случайных точек, 
# лежащих на случайной диагонали левый верхний  - правый нижний угол
diag_sample_part <- function(df) {
  # Точки могут повторяться, реальный размер выборки может оказаться немного меньше SAMPLE_SIZE
  ds = sample(0:200, 100)
  xs = sample(-150:-50, 1) + ds
  ys = sample(50:150, 1) - ds
  df_sample <- data.frame(x = xs, y = ys)
  return(nrow(merge(df_sample, df))/nrow(df_sample))
}

diag_prime_parts <- replicate(300, diag_sample_part(df_prime))
hist(diag_prime_parts, breaks = 20)
mean(diag_prime_parts)

diag_rand_parts <- replicate(300, diag_sample_part(df_rand))
hist(diag_rand_parts, breaks = 20)
mean(diag_rand_parts)


prime4_1 <- prime_nums[prime_nums %% 4 == 1]
length(prime4_1)/N
df_prime4_1 <- num_to_xy(prime4_1)


diag_prime4_1_parts <- replicate(300, diag_sample_part(df_prime4_1))
hist(diag_prime4_1_parts, breaks = 20)













rand_sample_part(df_prime)
rand_sample_part(df_rand)

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


k <- 301^2
primes3 <- (2:k)
primes3 <- primes3[(primes3 %% 4 == 1)]
primes3 <- primes3[as.logical(isprime(primes3))]
length(primes3)
# primes3 <- primes3[sample(1:length(primes3), length(primes3) %/% 5)]
df3 <- num_to_xy(primes3)
df4 <- num_to_xy(2:k)

df_rand = data.frame(x = sample(-150:150, size = 50, replace = TRUE), 
                     y = sample(-150:150, size = 50, replace = TRUE))


ggplot() + 
  geom_point(mapping = aes(x = df3$x, y = df3$y), 
             col = "red", size = 0.5)

  
ggplot() + 
  geom_point(mapping = aes(x = df4$x, y = df4$y), 
             col = "red", size = 1)
  
    
# + geom_path(mapping = aes(x = df4$x, y = df4$y))


df_rand = data.frame(x = sample(-150:150, size = 50, replace = TRUE), 
                     y = sample(-150:150, size = 50, replace = TRUE))

df_a <- merge(df3, df_rand)

x <- sample(-100:100, size = 50, replace = TRUE)
y = x
df_diag = data.frame(x = x, y = y + sample(-50:50, size = 1))

df_b <- merge(df3, df_diag)

df_vert <- data.frame(x = sample(-100:100, size = 50, replace = TRUE), y = sample(-100:100, size = 1))
df_c <- merge(df3, df_vert)

nrow(df_a)
nrow(df_b)
nrow(df_c)

c(1, 2, 3)

ps <- sample(1:100000, size = 50)
sum(isprime(ps))
n <- 100000
50*n/(100000*log(n))


sum((df3$x == x) & (df3$y == y))

  geom_point(mapping = aes(x = seq(-100, 100, 4), y = -seq(-100, 100, 4))) +
  geom_point(mapping = aes(x = seq(-100, 100, 4), y = -seq(-100, 100, 4) + 1))

ggplot() + 
  geom_point(mapping = aes(x = seq(-100, 100, 1), y = -seq(-100, 100, 1)))

  geom_point(mapping = aes(x = seq(-100, 100, 2), y = -seq(-100, 100, 2)))

seq(1, 10, 2)


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


