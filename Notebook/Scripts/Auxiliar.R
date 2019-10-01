## Severidad Gaussiana inversa

if(!require("statmod")){install.packages("statmod")}
library("statmod")
set.seed(12345)
y <- rinvgauss(2000, mean=5, shape=3)
hist(y,100)
alpha <- 1/(mean(y)*mean(1/y)-1)
beta <- alpha/mean(y)



