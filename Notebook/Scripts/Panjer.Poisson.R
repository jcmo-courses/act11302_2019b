Panjer.Poisson <- function (p, lambda){
  if (sum(p)>1||any(p<0)) stop("The parameter `p` is not a probability measure.")
  if (lambda * sum(p) > 727) stop("Underflow")
  cumul <- f <- exp(-lambda * sum(p))
  r <- length(p)
  s <- 0
  repeat
  { s <- s+1
  m <- min(s, r)
  last <- lambda / s * sum(1:m * head(p,m) * rev(tail(f,m)))
  f <- c(f,last)
  cumul <- cumul + last
  if (cumul > 0.99999999) break  }
  return(f) 
}