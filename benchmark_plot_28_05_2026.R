library("RColorBrewer")

options(stringsAsFactors=FALSE)
profs <- read.csv("benchmarking data.csv")
#profs$group <- profs$University
#profs$group[profs$University %in% c("UTS","Soton")] <- "UTS/Soton"
profs$group <- "Go8"
profs$group[profs$University %in% c("UTS","Soton")] <- "UTS/Soton"
profs$groupFac <- as.factor(profs$group)
K <- length(unique(profs$group))

plotcols <- brewer.pal(K+1,"Dark2")
plot(profs$PapersWhenPromoted,profs$hIndexWhenPromoted,
     xlab="Number of Scopus Publications",
     ylab="Scopus h-index",
     xlim=c(0,max(profs$PapersWhenPromoted,na.rm=TRUE)),
     ylim=c(0,max(profs$hIndexWhenPromoted,na.rm=TRUE)),
     pch=14+as.numeric(profs$groupFac),
     col=plotcols[as.numeric(profs$groupFac)],
     #main="Scopus Metrics of Selected Professors calculated at Date of Promotion"
     main="")
legend("topleft",legend=c(levels(profs$groupFac),"me"),pch=14+c(1:(K+1)),
       col=plotcols)
points(42,12,pch=14+K+1,cex=2,col=plotcols[K+1])

