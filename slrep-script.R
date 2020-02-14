# %% LIBRARY -------------------------------------------------------------------

pacman::p_load(SuperLearner,
               ggplot2,
               ggthemes,
               data.table,
               magrittr,
               tictoc)


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


# %% SUPER LEARNER -------------------------------------------------------------

listWrappers()

t <- s1samp[sid == 1, .(x, y1 = as.numeric(y1))]

# create GAM learner
gam_tuner <- create.Learner("SL.gam",
                         detailed_names = T,
                         tune = list(deg.gam = 2:4))

gam_tuner

# create LOESS learner
loess_tuner <- create.Learner("SL.loess",
                              detailed_names = T,
                              tune = list(span = c(0.75, 0.5, 0.25, 0.1)))

loess_tuner

# create neural net learner
nnet_tuner <- create.Learner("SL.nnet",
                          detailed_names = T,
                          tune = list(size = 2:5))

nnet_tuner

# create bagging learners
bag_tuner <- create.Learner("SL.ipredbagg",
                            detailed_names = T,
                            tune = list(cp = c(0.01, 0.1, 0)))

bag_tuner

bag_ms <- create.Learner("SL.ipredbagg",
                         detailed_names = T,
                         name_prefix = "SL.ipredbagg_ms",
                         params = list(cp = 0.01, minsplit = 5))

bag_ms

# @NOTE: A couple of the learners appear to have been updated since the
#        book was published. Going with assumed closest learners currently
#        available in the SuperLearner package. See Table 3.2 on page 55 for
#        list of original learners.

sl_lib <- c("SL.glm",
            "SL.glm.interaction",   # sub for SL.interaction
            "SL.randomForest",
            bag_tuner$names,        # sub for SL.bagging(0.01, 0.1, 0)
            bag_ms$names,           # sub for SL.bagging(ms5)
            #"SL.gbm",
            gam_tuner$names,
            nnet_tuner$names,
            "SL.polymars",
            loess_tuner$names,
            "SL.bartMachine"        # sub for SL.bart
)

# @NOTE: The gam model from the gam package has a bug related to setting the
#        'contrasts' parameter in model.matrix. This parameter is supposed to
#        be a list, and until recent versions of R, model.matrix failed
#        silently.
#
#        If I interpret correctly, should only be a problem when we're wanting
#        to make certain contrasts between factor levels, so irrelevant for
#        the procedure below.
#
#        Ignore the warnings.
#
#  SO thread: https://stackoverflow.com/questions/57664927/warning-in-gam-with-
#             release-of-r-3-6-1

tic(msg = "SuperLearner runtime")

sl_test <-
      CV.SuperLearner(Y = t$y1,
                      X = as.data.frame(t[, .(x)]),
                      family = gaussian(),
                      SL.library = sl_lib)
toc()

# gives cross-validated risk
summary(sl_test)
sl_test$whichDiscreteSL

plot(sl_test) +
      ggridges::theme_ridges()
