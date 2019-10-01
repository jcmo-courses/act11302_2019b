logNormal.moment <- function(n, mu, sigma){
  exp(n * mu + (n * sigma)^2 / 2)
}