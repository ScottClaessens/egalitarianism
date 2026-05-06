# helper functions from rethinking package
rordlogit <- function (n, phi = 0, a) {
  a <- c(as.numeric(a), Inf)
  k <- 1:length(a)
  if (length(phi) == 1) {
    p <- dordlogit(k, a = a, phi = phi, log = FALSE)
    y <- sample(k, size = n, replace = TRUE, prob = p)
  }
  else {
    y <- rep(NA, n)
    if (n > length(phi)) {
      phi <- rep(phi, ceiling(n/length(phi)))
    }
    for (i in 1:n) {
      p <- dordlogit(k, a = a, phi = phi[i], log = FALSE)
      y[i] <- sample(k, size = 1, replace = TRUE, prob = p)
    }
  }
  y
}

dordlogit <- function (x, phi, a, log = FALSE) {
  a <- c(as.numeric(a), Inf)
  p <- logistic(a[x] - phi)
  na <- c(-Inf, a)
  np <- logistic(na[x] - phi)
  p <- p - np
  if (log == TRUE)
    p <- log(p)
  p
}

logistic <- function (x) {
  p <- 1/(1 + exp(-x))
  p <- ifelse(x == Inf, 1, p)
  p
}
