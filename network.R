require(igraph)
friends_df$number = 1:nrow(friends_df)


mutual_friends_ids_all[[1]]
friends_df[,c("friends_ids", "number")]


mutual_friends_numbers = lapply(mutual_friends_ids_all, function(x) match(x, friends_df$friends_ids))

all_edges = c()
for (i in 120:length(mutual_friends_numbers)) {
  x = mutual_friends_numbers[[i]][!is.na(mutual_friends_numbers[[i]])]
  for (j in 1:length(x)) {
    all_edges = c(all_edges, i,x[j])
  }
  # current_paste  = paste(paste0(i, ",", x), collapse = ",")
  # all_paste = paste(all_paste, current_paste, collapse = ",")
}

# all_paste = gsub(" ", "", all_paste)

g1 <- graph( edges=all_edges, n=length(unique(all_edges)), directed=F ) 

plot(g1) # A simple plot of the network - we'll talk more about plots later



g <- make_full_graph(5) %du% make_full_graph(5) %du% make_full_graph(5)
g <- add_edges(g, c(1,6, 1,11, 6, 11))
fc <- cluster_fast_greedy(g)
membership(fc)
sizes(fc)