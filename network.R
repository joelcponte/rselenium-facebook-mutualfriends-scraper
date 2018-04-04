library(igraph)
library(RCurl)
library(rjson)
library(tidyverse)

N = nrow(friends_df)
friends_matrix = matrix(0,N,N,dimnames = list(friends_df$friends_ids, friends_df$friends_ids))
for (i in 1:N) {
  friends_matrix[i,friends_df$friends_ids %in% mutual_friends_ids_all[[i]]] = 1
}
#make it diagonal to not count the same connection twice
friends_matrix[lower.tri(friends_matrix)] <- 0
colnames(friends_matrix) = friends_df$name
rownames(friends_matrix) = friends_df$name


links <- do.call(rbind, lapply(1:N, function(i) {
  do.call(rbind, lapply(1:N, function(j) {
    if(friends_matrix[i,j]==1) return(data.frame(from=friends_df$name[i], to=friends_df$name[j]))
  }))
}))
g <- graph.data.frame(links, directed=FALSE)

##learn clusters
wc <- label.propagation.community(g)
modularity(wc)


## export links and nodes to json for d3 visualization
names(links) = c("source", "target")
nodes = data.frame(name = NULL, group = NULL)
for (i in 1:length(wc)) {
  nodes = rbind(nodes, data.frame(wc[[i]], rep(i, length(wc[[i]]))))
}
names(nodes) = c("name", "group")


n_links = do.call("rbind", mutual_friends_ids_all %>% lapply(length))
#remove the next line
n_links[533] = 0
n_links = cbind(friends_df[,"name", drop = F], n_links)
nodes = merge(nodes, n_links, by = "name", sort = F)
names(nodes)[1] = "id"
nodes = nodes[nodes$n_links != 0,]
#in case there's a mistake and someone appears in links but not nodes...
links = links[links$source %in% nodes$id,]
links = links[links$target %in% nodes$id,]


json_nodes = rjson::toJSON(unname(split(nodes, 1:nrow(nodes))))
json_links = rjson::toJSON(unname(split(links, 1:nrow(links))))
write_file(paste0('{\n"nodes": ', json_nodes, ', \n', '"links": ', json_links, "\n}"),
           "temp.json")
