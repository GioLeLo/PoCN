#####################################################################
# Results of the paper from Cuquet, Calsamiglia.
# [DOI: 10.1103/PhysRevLett.103.240503]
# Entanglement Percolation
# Creation date: 09/07/24 - 11:18
# Author: Giolelo
#
# Disabled linters:
# nolint start: object_name_linter, commented_code_linter.
#####################################################################

library(igraph)
library(RColorBrewer)
library(ggplot2)

#####################################################################
# PARALLELIZATION STUFF
library(parallel)

#setup parallel backend to use many processors
cores <- detectCores() - 2
# cl <- makeCluster(cores[1] - 2) #to not blow up the server
# registerDoParallel(cl)
#####################################################################


# Number of vertices
n <- 10000
# Number of graphs in the ensemble
n_ens <- 20
# Percolation probability no. of points
p_steps <- 20
# Connection probability, distilled and not
# p2_dist <- min(1, 2.0 * p - p^2 / 2.0)
# p2 <- 2.0 * p - p^2
p2 <- 2.5 / n
p_scp <- - sqrt(1 - p2) + 1

# Quantum strategy 
# Classical percolation - up to degree 15
# q_strategy <- c(rep(0, 15))
# Q-swaps magic
q_strategy <- c(2, 3)

# percolation procedure
GCC_perc <- function(p, network, ensemble = 10) {
  print(c("Starting calc of p=", p))
  res <- lapply(
    seq_len(ensemble),
    function(x) {
      removed <- which(runif(length(E(network))) >= p, arr.ind = TRUE)
      mod_g_BA <- network - E(network)[removed]

      GCC <- max(components(mod_g_BA)$csize) / n
      err_GCC <- (GCC)**2
      return(c(GCC, err_GCC))
    }
  )
  GCC <- sum(sapply(res, "[[", 1)) / ensemble
  err_GCC <- sum(sapply(res, "[[", 2)) / ensemble
  err_GCC <- sqrt(err_GCC - GCC^2)

  return(c(GCC, err_GCC))
}

# loop over probabilities, p is SCP while p2 is edge creation probability
p_range <- seq(0, 1, length.out = p_steps)
BA_results <- data.frame(matrix(ncol = 5, nrow = p_steps))
colnames(BA_results) <- c("p", "GCC_clean", "err_clean", "GCC_swap", "err_swap")


# We generate one random graph and apply different percolations,
# it's not the best solution but it's faster (needed)
g_BA <- sample_pa(n, m = 3, directed = FALSE)
# Add ids to the vertices, better
V(g_BA)$name <- V(g_BA)
g_BA_clean <- sample_pa(n, m = 3, directed = FALSE)
V(g_BA_clean)$name <- V(g_BA_clean)

# Percolation on clean network
print("Starting percolation on clean network")
clean <- mclapply(
  p_range,
  GCC_perc,
  ensemble = n_ens,
  network = g_BA_clean,
  mc.cores = cores
)
GCC <- lapply(clean, "[[", 1)
err_GCC <- lapply(clean, "[[", 2)

BA_results$p <- p_range
BA_results$GCC_clean <- unlist(GCC)
BA_results$err_clean <- unlist(err_GCC)
print("Done!, applying qstrategy")

# apply quantum strategy
degs <- degree(g_BA)
# We must find all the nodes with deg q and q-swap them
# Resources-heavy and most likely slow, FIND BETTER ALTERNATIVES
for (q_node in which(degs %in% q_strategy, arr.ind = TRUE)) {
  # We q-swap q_node by creating cyclic connections in its neighbours
  q_node_neighbors <- neighbors(g_BA, V(g_BA)[q_node])
  num_neighbors <- length(q_node_neighbors)
  new_edge <- which(runif(num_neighbors) >= p_scp, arr.ind = TRUE)
  # although q-swap it's certain, the generation of cyclic connections
  # happens with probability p given by the SCP, so we generate edges
  # based on that probability, indexing to neighbor i the edge i
  if (num_neighbors == 2 && any(new_edge)) {
    g_BA <- g_BA + edges(c(q_node_neighbors))
  } else {
    actual_new_edge <- c(rbind(
      q_node_neighbors,
      q_node_neighbors[c(2:length(q_node_neighbors), 1)]
    )[, new_edge])
    g_BA <- g_BA + edges(actual_new_edge)
  }
  # then we disconnect the q_swapped vertex
  g_BA <- delete_edges(
    g_BA,
    get.edge.ids(
      g_BA,
      c(rbind(rep(q_node, num_neighbors), as.numeric(q_node_neighbors)))
    )
  )
}

# Percolation on swapped network
print("Starting percolation on qswapped network")
swap <- mclapply(
  p_range,
  GCC_perc,
  ensemble = n_ens,
  network = g_BA,
  mc.cores = cores
)
GCC <- lapply(swap, "[[", 1)
err_GCC <- lapply(swap, "[[", 2)

BA_results$GCC_swap <- unlist(GCC)
BA_results$err_swap <- unlist(err_GCC)
print("Done!")

# stopCluster(cl)

p1 <- ggplot(data = BA_results) +
  geom_line(aes(x = p, y = GCC_clean)) +
  geom_errorbar(aes(
    x = p,
    y = GCC_clean,
    ymin = GCC_clean - err_clean,
    ymax = GCC_clean + err_clean
  )) +
  geom_line(aes(x = p, y = GCC_swap)) +
  geom_errorbar(aes(
    x = p,
    y = GCC_clean,
    ymin = GCC_swap - err_swap,
    ymax = GCC_swap + err_swap
  )) +
  labs(
    title = paste(
      "Percolation in ER net with and without Q-SW with N=",
      n,
      sep = ""
    ),
    x = "p",
    y = "Normalized GCC"
  ) +
  theme_minimal()

print(p1)

ggsave(paste("BA_perc", n, sep = "_"), width = 5, height = 5, device = "png")

#####################################################################
# nolint end
#####################################################################