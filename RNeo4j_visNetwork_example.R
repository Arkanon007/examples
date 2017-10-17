# https://cran.r-project.org/web/packages/visNetwork/vignettes/Introduction-to-visNetwork.html
# http://visjs.org/docs/network/
# http://visjs.org/docs/network/nodes.html 
install.packages(c("RNeo4j","igraph","visNetwork","Rmarkdown", "Rcpp", "RColorbrewer"))

library(RNeo4j)
library(igraph)
library(visNetwork)
library(RColorBrewer)

graph = startGraph("http://localhost:7474/db/data")

# visNetwork expects the from and to fields as the first two columns, everything beyond that are edge properties
query = "MATCH (n:Person)-[]-(p:Movie) RETURN n.name as from, p.title as to"
edges = cypher(graph, query)
nodes = data.frame(id=unique(c(edges$from, edges$to)))

ig = graph_from_data_frame(edges, directed=F)
clusters = cluster_edge_betweenness(ig)

# Generate a unique color for each cluster
n <- length(clusters)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
col = sample(col_vector, n)

# Choose a color for each cluster membership
selectedcolors <- col[clusters$membership]
nodes$color = selectedcolors

# False, person = square. True, movie = box.
# Index 1 = person, index 2 = movie
myshapes <- c("square", "box")

shapeindexbool <- nodes$id %in% edges$to #to is the movie vector
shapeindexint <- as.integer(shapeindexbool)
shapeindexint <- shapeindexint + 1 # 0 isn't a valid array value in R.

# size of the node
refcount = c( table(edges$from), table(edges$to) )

# set node properties expected by visNetwork
nodes$label = nodes$id
nodes$group = clusters$membership
nodes$shape = myshapes[shapeindexint]

nodes$value = refcount[nodes$id]
nodes$title = paste0("<a href='https://www.google.ca/search?q=",nodes$id,"'>",nodes$id,"</a>")

visNetwork(nodes, edges, width = "100%") %>%   
  visEdges(arrows ="to") %>%                            # arrow "to" for all edges
  visLegend() %>%
  visOptions(nodesIdSelection = TRUE)	%>%		# select by id / label
  visOptions(manipulation = TRUE)