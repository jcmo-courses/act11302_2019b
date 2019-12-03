# Kolmogorov-Smirnov Test

require(graphics)

x <- rnorm(50)
y <- runif(30)
# Do x and y come from the same distribution?
ks.test(x, y)
# Does x come from a shifted gamma distribution with shape 3 and rate 2?
ks.test(x+2, "pgamma", 3, 2) # two-sided, exact
ks.test(x+2, "pgamma", 3, 2, exact = FALSE)
ks.test(x+2, "pgamma", 3, 2, alternative = "gr")

# test if x is stochastically larger than x2
x2 <- rnorm(50, -1)
plot(ecdf(x), xlim = range(c(x, x2)))
plot(ecdf(x2), add = TRUE, lty = "dashed")
t.test(x, x2, alternative = "g")
wilcox.test(x, x2, alternative = "g")
ks.test(x, x2, alternative = "l")


# Anderson-Darlinkg Test (not working)

if(!require("goftest")){install.packages("goftest")}

x <- rnorm(10, mean=2, sd=1)

ad.test(x, "pnorm", mean=2, sd=1)

# Anderson-Darling Test (actually working)

set.seed(47)
dat <- rgamma(20, shape = 2, scale = 3)

# Shapiro-Wilk generalized goodness-of-fit test
#----------------------------------------------
gof.list <- gofTest(dat, distribution = "gamma")
gof.list

#Results of Goodness-of-Fit Test
#-------------------------------
#
#Test Method:                     Shapiro-Wilk GOF Based on 
#                                 Chen & Balakrisnan (1995)
#
#Hypothesized Distribution:       Gamma
#
#Estimated Parameter(s):          shape = 1.909462
#                                 scale = 4.056819
#
#Estimation Method:               mle
#
#Data:                            dat
#
#Sample Size:                     20
#
#Test Statistic:                  W = 0.9834958
#
#Test Statistic Parameter:        n = 20
#
#P-value:                         0.970903
#
#Alternative Hypothesis:          True cdf does not equal the
#                                 Gamma Distribution.

dev.new()
plot(gof.list)

#----------

# Redo the example above, but use the bias-corrected mle

gofTest(dat, distribution = "gamma", 
        est.arg.list = list(method = "bcmle"))