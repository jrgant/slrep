# %% LIBRARY -------------------------------------------------------------------

pacman::p_load(SuperLearner,
               ggplot2,
               ggthemes,
               data.table,
               magrittr)


# %% UNIVERSAL SETUP -----------------------------------------------------------

N <- 10000
n <- nsamp <- 100

X <- runif(N, -4, 4)


# %% SIMULATION 1 --------------------------------------------------------------

set.seed(90210)

# true outcome
Y1 <- -2 * I(X < -3) +
      2.55 * I(X > -2) -
      2 * I(X > 0) +
      4 * I(X > 2) -
      1 * I(X > 3) +
      rnorm(N, 0, 1)

plot(density(Y1))
plot(X, Y1)


# %% SIMULATION 2 --------------------------------------------------------------

set.seed(555)

Y2 <- 6 +
      0.4 * X - 0.36 * X^2 +
      0.005 * X^3 +
      rnorm(N, 0, 1)

plot(density(Y2))
plot(X, Y2)


# %% SIMULATION 3 --------------------------------------------------------------

set.seed(1984)

Y3 <- 2.83 * sin(pi / 2 * X) + rnorm(N, 0, 1)

plot(density(Y3))
plot(X, Y3)


# %% SIMULATION 4 --------------------------------------------------------------

Y4 <- 4 * sin(3 * pi * X) * I(X > 0) + rnorm(N, 0, 1)

plot(density(Y4))
plot(X, Y4)


# %% SAMPLES -------------------------------------------------------------------

truedat <- data.table(X, Y1, Y2, Y3, Y4)
str(truedat)

set.seed(12430)

## Generate samples from Simulation 1

s1samp <- lapply(1:nsamp, function(x) {
      truedat[sample(1:.N, n), c("X", "Y1")][, sid := x]
      }) %>%
      rbindlist %>%
      .[, .(sid, x = X, y1 = Y1)]

str(s1samp)
s1samp[sid == 1, plot(x, y1)]

## Generate samples from Simulation 2

s2samp <- lapply(1:nsamp, function(x) {
      truedat[sample(1:.N, n), c("X", "Y2")][, sid := x]
      }) %>%
      rbindlist %>%
      .[, .(sid, x = X, y2 = Y2)]

str(s2samp)
s2samp[sid == 1, plot(x, y2)]

## Generate samples from Simulation 3

s3samp <- lapply(1:nsamp, function(x) {
      truedat[sample(1:.N, n), c("X", "Y3")][, sid := x]
      }) %>%
      rbindlist %>%
      .[, .(sid, x = X, y3 = Y3)]

str(s3samp)
s3samp[sid == 1, plot(x, y3)]

## Generate samples from Simulation 4

s4samp <- lapply(1:nsamp, function(x) {
      truedat[sample(1:.N, n), c("X", "Y4")][, sid := x]
      }) %>%
      rbindlist %>%
      .[, .(sid, x = X, y4 = Y4)]

str(s4samp)
s4samp[sid == 1, plot(x, y4)]
