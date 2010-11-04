## Generate a digraph file for graphviz with weights and penwidth reflecting
## frequency packages are committed together or nearby...
##
## Input file generated with:
##
## git log --name-only | egrep ^commit\|^packages | \
##   sed -e 's#^packages/##g' | egrep / | sed 's#/.*$##' | uniq > /tmp/prox


## cf http://stackoverflow.com/questions/2333025/graphviz-changing-the-size-of-edge
##
## digraph {
## "Node 1" [shape=diamond, penwidth=3, style=filled,fillcolor="#FCD975"] ;
## "Node 2" [style=filled,fillcolor="#9ACEEB" ] ;
## "Node 3" [shape=diamond, style=filled,fillcolor="#FCD975" ] ;
## "Node 4" [style=filled, fillcolor="#9ACEEB" ]
##
## "Node 1" -> "Node 2" [dir=none,weight=1, penwidth=3] ;
## "Node 1" -> "Node 3" [dir=none, color="#9ACEEB"] ;
## "Node 1" -> "Node 4" [arrowsize=0.5, weight=2.0]
## }
##

x <- as.vector(read.csv("/tmp/prox",header=FALSE,colClasses="character")[,1,drop=T])

paircount <- function(x,win=10,by=10) {
  counts <- list()

  for (i in 1:max(x[,2])) {
    l <- unique(x[i:(i+win)])
    for (j in 2:length(l)) {
      for (k in 1:(j-1)) {
        key <- paste(sort(c(l[j],l[k])),collapse=":")
        if (is.null(counts[[key]]))
          counts[[key]] <- 0
        counts[[key]] <- counts[[key]] + 1
      }
    }
  }

  unlist(counts)
}

out <- paircount(x)

hout <- out[which(out > 30)]

digraph <- function(c,fnm="/tmp/tmp.dot") {
  f <- file(fnm,"w")
  low <- range(c)[1]
  hi <- range(c)[2]

  nodes <- unique(c(gsub(".*:","",names(c)),gsub(":.*","",names(c))))
  cat("digraph {\n",file=f)
  cat(sprintf("\"%s\";",nodes),sep="\n",file=f)
  cat("\n",file=f)
  pen <- (log(c)-log(low))/(log(hi)-log(low))*8+1
  wt <- (c-low)/(hi-low)*.8+.2
  cat(sprintf("\"%s\" [penwidth=%.0f,weight=%.2f,dir=none]",gsub(":","\" -> \"",names(c)),pen,wt),sep="\n",file=f)
  cat("}\n",file=f)
  close(f)
}

digraph(hout)
