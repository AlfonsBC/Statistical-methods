#######################
# Muestras aleatorias #
#######################
# 1 Población finita
library(mlbench)
data(PimaIndiansDiabetes2)
BD <- PimaIndiansDiabetes2

dim(BD)

head(BD)
N <- nrow(BD) # Tamaño de la población
n <- 5 # muestra
id <- sample.int(N, n, replace = TRUE)

BD[id, ]

# 2 Población infinita
# Bernoulli
rbinom(100, 1, 0.9) # Bernoulli == Bin(1, p)
# Binomial
rbinom(10, 20, 0.9)
# Normal
rnorm(10, 5, 3)
# Poisson
rpois(20, 10)
# Gamma
rgamma(5, 2, 0.3)


##################
# Media muestral #
##################
x <- as.numeric(na.omit(BD$mass))
length(x)
mean(x)

n <- 30
k <- 100000
X_bar <- rep(NA, k)
for (i in 1:k) {
  X_bar[i] <- mean(sample(x, n, replace = TRUE))
}

# mu
mean(x)
# E(X_bar)
mean(X_bar)

# sigma^2/n
var(x)/n
# Var(X_bar)
var(X_bar)


#        Pero además también tenemos la      #
# Aprox - Distribución de muestreo de X_bar  #
summary(X_bar)
a <- 27
b <- 40
d <- 40000
hist(X_bar, border = "white", xlim = c(a, b), 
     axes = FALSE)
axis(1, seq(a, b, by = 2))
axis(2, seq(0, d, by = d/10), las = 2)
abline(v = quantile(X_bar, probs = c(0.025, 0.975)), col = "red", lwd = 2)


