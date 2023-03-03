# Simulate distance sampling data
# From Kéry & Royle (2015, chapter 8.2.5)

sim.pdata <- function(N, sigma, B, keep.all=FALSE) {
  # Function simulates coordinates of individuals on a square
  # Square is [0,2*B] x[0,2*B], with a count location on the center
  # point (B,B)
  # Function arguments:
  #    N: total population size in the square
  #    sigma: scale of half-normal detection function
  #    B: circle radius
  #    keep.all: return the data for y = 0 individuals or not
  
  u1 <- runif(N, 0, 2*B)           # (u1,u2) coordinates of N individuals
  u2 <- runif(N, 0, 2*B)
  d <- sqrt((u1 - B)^2 + (u2 - B)^2) # distance to center point of square
  N.real <- sum(d <= B)           # Population size inside of count circle
  
  # Can only count indidividuals in the circle, so set to zero detection 
  # probability of individuals in the corners (thereby truncating them):
  p <- ifelse(d < B, 1, 0) * exp(-d*d/(2*(sigma^2)))
  
  # Now we decide whether each individual is detected or not
  y <- rbinom(N, 1, p)
  
  # Put all of the data in a matrix:
  #      (note we don't care about y, u, or v normally)
  if (!keep.all) {
    u1 <- u1[y == 1]
    u2 <- u2[y == 1]
    d <- d[y == 1]
  }
  return(list(N = N, sigma = sigma, B = B, u1 = u1, u2 = u2, d = d, y = y, 
              N.real = N.real))
}
