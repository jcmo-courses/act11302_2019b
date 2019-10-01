IntVal.Poisson <- function (freq){
  if (any(freq<0)) stop("The parameter `freq` is not a probability measure.")
  M <- length(freq)
  mu <- sum((1:M)*freq); sigma2 <- sum((1:M)^2*freq)
  MM <- ceiling(mu + 10 * sqrt(sigma2)) + 6
  fs <- dpois(0:(MM-1), freq[1])
  # Density of S_1 = 1*N_1
  for (j in 2:M)
  {MMM <- trunc((MM-1)/j)
  fj <- rep(0, MM)  
  # Construct the density of j*N_j
  fj[(0:MMM)*j+1] <- dpois(0:MMM, freq[j])
  fs <- convolve(fs, rev(fj), type="o") }
  # fs is the density of S_j = 1*N_1 + ... + j*N_j, j=2..M
  return(fs)  
}