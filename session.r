setwd("F:\\git-repos\\graph-german-premier-league")
rm(list = ls())
options(scipen=999)

library(igraph)

df <- read.table("season2012.tsv", header=TRUE, sep="\t", stringsAsFactors=FALSE)
	
setup_graph <- function(data, m0, m1) {
	data <- transform(data, scoreAT = as.numeric(scoreAT), scoreHT = as.numeric(scoreHT))
	data <- data[data$matchday %in% m0:m1,]

	ht <- data[,"HT"]
	at <- data[,"AT"]
	
	res <- data[,"scoreHT"]-data[,"scoreAT"]
	res <- ifelse(res == 0,-1,res)

	matches <- data.frame(a = c(ht,at), b = c(at,ht), r = c(res,-res))

	g <- graph.data.frame(matches, directed=TRUE)
	
	return(g)
}

plot_graph <- function(g, getSP = c()) {
	E(g)$color <- ifelse(V(g)$r >= 1, hsv(234/360,.85,.64), hsv(288/360,.85,.64))
	
	if(is.vector(getSP) & length(getSP)==2) {
		V(g)$color <- "gray"
		E(g)$label.color <- "gray"
		E(g)$color <- rgb(.7,.7,.7,alpha=1)
	
		sp <- get.all.shortest.paths(g,getSP[1],getSP[2])
		
		dummy <- lapply(sp$res, function(p) {E(g, path=p)$color <<- ifelse(E(g, path=p)$r >= 1, hsv(234/360,.85,.64), hsv(288/360,.85,.64))})
		dummy <- lapply(sp$res, function(p) {E(g, path=p)$label.color <<- E(g, path=p)$color})
		dummy <- lapply(sp$res, function(p) {E(g, path=p)$label <<- E(g,path=p)$r})
		dummy <- lapply(sp$res, function(p) {V(g)[p]$color <<- "lightblue"})
		V(g)[getSP]$color <- "pink"
		
		l <- lapply(sp$res,function(p) sum(E(g, path=p)$r))
	}

	l = layout.fruchterman.reingold(g,repulserad=100)

	plot(g,layout=l,edge.width=1/7,edge.curved=TRUE, edge.label=E(g)$label)
}

predict_outcome_of_match <- function(g, A, B) {
	sp <- get.all.shortest.paths(g, A, B)
	
	if(length(sp$res) == 0) {
		return(NA)
	} else {
		sum_of_scores <- sum(unlist(lapply(sp$res, function(p)sum(E(g,path=p)$r))))
		
		return(ifelse(sum_of_scores == 0), NA, sum_of_scores)
	}
}