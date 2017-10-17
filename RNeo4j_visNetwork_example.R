# https://cran.r-project.org/web/packages/visNetwork/vignettes/Introduction-to-visNetwork.html
# http://visjs.org/docs/network/
# http://visjs.org/docs/network/nodes.html 
install.packages(c("RNeo4j","igraph","visNetwork","Rmarkdown", "Rcpp", "RColorbrewer"))

library(RNeo4j)
library(igraph)
library(visNetwork)
library(RColorBrewer)

graph = startGraph("http://localhost:7474/db/data")

# for visNetwork to work right, you need a from and to data frame column. Additional properties are nice, but unnecessary.
query = "MATCH (n:Person)-[]-(p:Movie) RETURN n.name as from, p.title as to"
edges = cypher(graph, query)
nodes = data.frame(id=unique(c(edges$from, edges$to)))

ig = graph_from_data_frame(edges, directed=F)
clusters = cluster_edge_betweenness(ig)

# I need to study this one
n <- length(clusters)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
col = sample(col_vector, n)

#You can select values from a vector based on another vector of indexes.
selectedcolors <- col[clusters$membership]
nodes$color = selectedcolors


# False, person = square. True, movie = box.
# Index 1 = person, index 2 = movie
myshapes <- c("square", "box")
#myshapeurls <- c("./actor_icon.png","./movie_icon.png")

shapeindexbool <- nodes$id %in% edges$to #to is the movie vector
shapeindexint <- as.integer(shapeindexbool)
shapeindexint <- shapeindexint + 1 # 0 isn't a valid array value in R.

# size of the node
refcount = c( table(edges$from), table(edges$to) )

#or you could use the group field.
nodes$label = nodes$id
nodes$group = clusters$membership
nodes$shape = myshapes[shapeindexint]

#nodes$shape = "image"
#nodes$image = myshapeurls[shapeindexint]


nodes$value = refcount[nodes$id]
nodes$title = paste0("<a href='https://www.google.ca/search?q=",nodes$id,"'>",nodes$id,"</a>")


visNetwork(nodes, edges, width = "100%") %>%   
  visEdges(arrows ="to") %>%                            # arrow "to" for all edges
  visLegend() %>%
  visOptions(nodesIdSelection = TRUE)	%>%		# select by id / label
  visOptions(manipulation = TRUE)







#visNetwork(nodes, edges, width = "100%", height = "100%") %>%
#visNodes(shape = "square") %>%                        # square for all nodes
#visEdges(arrows = "from") %>%
#visHierarchicalLayout()

#visNetwork(nodes, edges, width = "100%") %>%   
  #visEdges(arrows ="to") %>%                            # arrow "to" for all edges
#  visLegend() %>%
  #visOptions(nodesIdSelection = TRUE)	%>%		# select by id / label
#  visOptions(manipulation = TRUE) %>%			# add / edit nodes relationships
  #visHierarchicalLayout()
  
#  visOptions(selectedBy = "id") %>%			# select by column
  

  #visGroups(groupname = "1", color = "green") %>%    	# green for group "1". Optional. When you set "group" it will autocolor.
#  visGroups(groupname = "2", color = "red")          	# red for group "2". 