# Disabled linters:
# nolint start: object_name_linter, commented_code_linter.
#####################################################################

library(igraph)
library(RColorBrewer)
library(ggplot2)
library(stats)
library(rootSolve)
library(parallel)

cores <- detectCores()

# Number of vertices
n <- 1e5
# average degree
c <- 6
# no. of probabilities to be studied
p_points <- 20
# efficiency of quantum repeater protocol
alpha <- 1
# no. of percolation runs for each value of p
ensemble <- 20
# avg of entangled pairs with exponential density
n_avg <- (10 * log(n))**alpha

# The idea of creating just one graph to then
# remove the links is plain bullshit, but to
# reproduce exactly the paper I must follow it.
g_BA <- sample_pa(n, m = c)

#Start with standard percolation:
GCC <- numeric(p_points)
err_GCC <- numeric(p_points)
diam <- numeric(p_points)
err_diam <- numeric(p_points)

tot_perc <- function(p, ensemble = 10, DIAMETER = TRUE) {
  print(c("Starting calc of p=", p))
  res <- lapply(
    seq_len(ensemble),
    function(x) {
      removed <- which(runif(length(E(g_BA))) > p, arr.ind = TRUE)
      mod_g_BA <- g_BA - E(g_BA)[removed]

      GCC <- components(mod_g_BA)$csize[1]
      err_GCC <- (GCC)**2
      diam <- diameter(mod_g_BA)
      err_diam <- (diam)**2

      if (DIAMETER) return(c(GCC, err_GCC, diam, err_diam))
      else return(c(GCC, err_GCC))
    }
  )
  GCC <- sum(sapply(res, "[[", 1)) / ensemble
  err_GCC <- sum(sapply(res, "[[", 2)) / ensemble
  err_GCC <- sqrt(err_GCC - GCC^2)

  if(DIAMETER) {
    diam <- sum(sapply(res, "[[", 3)) / ensemble
    err_diam <- sum(sapply(res, "[[", 4)) / ensemble
    err_diam <- sqrt(err_diam - diam^2)
  }

  if (DIAMETER) return(c(GCC, err_GCC, diam, err_diam))
  else return(c(GCC, err_GCC))
}

# p <- seq(0, 1, len = p_points)
p <- sort(union(c(0.05, 0.1, 0.5, 0.8), seq(0.1, 0.5, len=20))) 

res <- mclapply(
  p,
  tot_perc,
  ensemble = ensemble,
  mc.cores = cores - 2
)

GCC <- sapply(res, "[[", 1)
err_GCC <- sapply(res, "[[", 2)
diam <- sapply(res, "[[", 3)
err_diam <- sapply(res, "[[", 4)

f_p_op0 <- function(p_ext) {
  res <- uniroot.all(
    function(p_op, p_e) {
      y0 <- diam[1 + floor(p_op * p_e * p_points)]
      m <- (diam[1 + floor(p_op * p_e * p_points)] -
              diam[2 + floor(p_op * p_e * p_points)]) / 1.0
      x <- (p_op * p_e * p_points) %% 1

      return(
        (-n_avg * log(p_op))^(1.0 / alpha) - (y0 + m * x)
      )
    },
    c(0.1, 1),
    p_e = p_ext
  )

  if (length(res) == 0) return()

  output <- cbind(rep(p_ext, length(res)), res, res * p_ext)
  return(output)
}

p_ext <- seq(0.1, 1, len = p_points)
# How do we treat the hysteresis region, in which
# we have two different values of p_op? DONE! with uniroot.all
res <- lapply(p_ext, f_p_op0)
res <- res[!sapply(res, is.null)]

p_ext <- sapply(res, "[[", 1)
p_op0 <- sapply(res, "[[", 2)
p_perc <- sapply(res, "[[", 3)

# We use the same (again) random graph g_BA
GCC_perc <- function(p, ensemble = 10) {
  print(c("Starting calc of p=", p))
  res <- lapply(
    seq_len(ensemble),
    function(x) {
      removed <- which(runif(length(E(g_BA))) >= p, arr.ind = TRUE)
      mod_g_BA <- g_BA - E(g_BA)[removed]

      GCC <- components(mod_g_BA)$csize[1]
      err_GCC <- (GCC)**2
      return(c(GCC, err_GCC))
    }
  )
  GCC <- sum(sapply(res, "[[", 1)) / ensemble
  err_GCC <- sum(sapply(res, "[[", 2)) / ensemble
  err_GCC <- sqrt(err_GCC - GCC^2)

  return(c(GCC, err_GCC))
}

res <- mclapply(p_perc, GCC_perc, ensemble, mc.cores = cores - 2)
GCC_ext <- lapply(res, "[[", 1)
err_GCC_ext <- lapply(res, "[[", 2)

GCC_ext <- as.data.frame(unlist(GCC_ext))
GCC_ext <- cbind(GCC_ext, as.data.frame(unlist(err_GCC_ext)))
GCC_ext <- cbind(unlist(p_ext), GCC_ext)
colnames(GCC_ext) <- c("p_ext", "GCC", "err")

GCC <- as.data.frame(unlist(GCC))
GCC <- cbind(GCC, as.data.frame(unlist(err_GCC)))
GCC <- cbind(p, GCC)
colnames(GCC) <- c("p", "GCC", "err")

diam <- as.data.frame(unlist(diam))
diam <- cbind(diam, as.data.frame(unlist(err_diam)))
diam <- cbind(p, diam)
colnames(diam) <- c("p", "diam", "err")

#####################################################################
# nolint end
#####################################################################
