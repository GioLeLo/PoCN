# for ShapeFile (shp) manipulation
library(sf)
library(lwgeom)
# for .dbf file manipulation
library(foreign)
library(igraph)

library(parallel)
cores <- detectCores() - 3

icc <- c("AL","AD","AT","BY","BE","BA","BG","HR","CY","CZ","DK","EE","FO","FI","FR","DE","GI","GR","HU","IS","IE","IM","IT","RS","LV","LI","LT","LU","MK","MT","MD","MC","ME","NL","NO","PL","PT","RO","RU","SM","RS","SK","SI","ES","SE","CH","UA","GB","VA","RS","GE","ND")

dir_path <- "./RAILS/FullEurope"

# shp_path <- paste(dir_path, "RailrdL.shp", sep = "/")
# # dbf_path <- paste(path, "RailrdL.dbf", sep = "/")

# epsg <- 4258
# sql_query <- paste("SELECT * FROM RailrdL WHERE TEN=1")
# shp <- st_read(shp_path, query = sql_query)
# shp <- st_geometry(shp)
# shp <- st_transform(shp, epsg)

# # To include accuracy: truncate locations at 1e-4
# accuracy <- 5
# links <- data.frame(matrix(nrow = length(shp), ncol = 2))

# links <- as.data.frame(t(sapply(
#   seq.int(length(shp)),
#   function(x) {
#     pts <- nrow(shp[[x]])
#     return(
#       c(
#         paste(
#           round(st_coordinates(st_startpoint(shp[[x]]))[1], digits = accuracy),
#           round(st_coordinates(st_startpoint(shp[[x]]))[2], digits = accuracy),
#           sep = ","
#         ),
#         paste(
#           round(st_coordinates(st_endpoint(shp[[x]]))[1], digits = accuracy),
#           round(st_coordinates(st_endpoint(shp[[x]]))[2], digits = accuracy),
#           sep = ","
#         )
#       )
#     )
#   }
# )))

# colnames(links) <- c("start", "end")
# joints <- as.data.frame(union(links$start, links$end))

# # Now we must search for similar endpoint / startpoint values and follow the path
# nodes_df <- read.csv(paste(dir_path, "Nodes.csv", sep = "/"))

# railway <- graph_from_data_frame(links, directed = FALSE, vertices = joints)

# stations <- paste(
#   round(nodes_df$Lat, digits = accuracy),
#   round(nodes_df$Lon, digits = accuracy),
#   sep = ","
# )
# to_color <- match(stations, V(railway)$name)
# to_color <- to_color[!is.na(to_color)] 
# nodes_selection <- match(V(railway)$name[to_color], stations)

# railway <- set_vertex_attr(
#   railway,
#   "ID",
#   V(railway)[to_color],
#   nodes_df$NodeID[nodes_selection]
# )

# # To simplify the graph:
# # create subgraph of nodes of deg <=2
# # calculate components
# # get components wit >= 2 nodes
# # merge nodes with contract.vertices
# # substitute old nodes with new ones (?)
# # maybe by taking difference of the original graph with the <2 graph
# # and then adding back te merged nodes

# n_stations <- length(to_color)

# # max_length <- 2 * 227 # Twice France's diameter
# max_length <- 0.5 * 227 # Half France's diameter
# paste("Diameter calculated")

# conn <- mclapply(seq(n_stations - 1), function(node1) {
#   print(paste("to do node ", node1))

#   res <- sapply(
#     seq(node1 + 1, n_stations, len = n_stations - node1),
#     function(node2) {
#       # remove all stations except for node1,2 and check edges
#       temp <- delete_vertices(railway, V(railway)[to_color[-c(node1, node2)]])
#       node1_pos <- match(V(railway)$name[to_color[node1]], V(temp)$name)
#       node2_pos <- match(V(railway)$name[to_color[node2]], V(temp)$name)

#       res <- shortest_paths(temp, node1_pos, node2_pos)

#       if (length(res$vpath[[1]]) > 0) {
#         return(V(temp)$ID[node2_pos])
#       }
#     }
#   )
#   res <- unlist(res)
#   return(res)
# },
# mc.cores = cores
# )

# names(conn) <- seq(n_stations - 1)

# conn <- conn[sapply(conn, Negate(is.null))]

# adj_df <- data.frame(matrix(nrow = 0, ncol = 2))

# for (x in seq_along(conn)) {
#   adj_df <- rbind(adj_df, cbind(V(railway)$ID[to_color[x]], conn[[x]]))
# }

# adj_df <- cbind(adj_df, rep(1, length(adj_df[, 1])))

# names(adj_df) <- c("node_from", "node_to", "weight")
  
# fwrite(adj_df, paste(dir_path, "Edges.csv", sep = "/"))

# lapply(icc, function(x) {
for (x in icc) {
  root_path <- "./RAILS/Countries"
  if (x == "CH") { x <- "CHLI" } 
  if (dir.exists(file.path(root_path, x))) {
    path <- paste(root_path, x, sep = "/")
    if (!file.exists(file.path(path, "Edges.csv"))) {
      next()
      # return(NULL)
    }
  } else {
    next()
    # return(NULL)
  }
  if (x == "CHLI") { x <- "CH" }
  country_path <- paste(root_path, x, "Edges.csv", sep = "/")
  country <- as.data.frame(read.csv(country_path))
  write.table(
    country, 
    paste(dir_path, "Edges.csv", sep = "/"),
    append = TRUE
  )
}
# })