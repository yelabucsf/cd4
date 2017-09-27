library(data.table);
library(ggplot2);
library(directlabels);

rst.0hr <- read.table("0hr.knownResults.txt",sep="\t",comment.char="",header=T);
rst.48hr <- read.table("48hr.knownResults.txt",sep="\t",comment.char="",header=T);
rst.shared <- read.table("shared.knownResults.txt",sep="\t",comment.char="",header=T);

rst.0hr[,1] <- sapply(as.character(rst.0hr[,1]),function(x) {strsplit(x,"/")[[1]][[1]]})
rst.48hr[,1] <- sapply(as.character(rst.48hr[,1]),function(x) {strsplit(x,"/")[[1]][[1]]})
rst.shared[,1] <- sapply(as.character(rst.shared[,1]),function(x) {strsplit(x,"/")[[1]][[1]]})


rst.0hr[,7] <- as.numeric(sapply(as.character(rst.0hr[,7]),function(x) {strsplit(x,"%")[[1]][[1]]}))
rst.48hr[,7] <- as.numeric(sapply(as.character(rst.48hr[,7]),function(x) {strsplit(x,"%")[[1]][[1]]}))
rst.shared[,7] <- as.numeric(sapply(as.character(rst.shared[,7]),function(x) {strsplit(x,"%")[[1]][[1]]}))

rst.0hr[,9] <- as.numeric(sapply(as.character(rst.0hr[,9]),function(x) {strsplit(x,"%")[[1]][[1]]}))
rst.48hr[,9] <- as.numeric(sapply(as.character(rst.48hr[,9]),function(x) {strsplit(x,"%")[[1]][[1]]}))
rst.shared[,9] <- as.numeric(sapply(as.character(rst.shared[,9]),function(x) {strsplit(x,"%")[[1]][[1]]}))

rst.0hr.df <- data.frame(name=rst.0hr[,1], enrichment=rst.0hr[,7]/rst.0hr[,9], prop=rst.0hr[,7], neg.log.p=-rst.0hr[,4])

ggplot.0hr <- ggplot(aes(prop, enrichment), data=rst.0hr.df)+geom_point(aes(size=neg.log.p),data=rst.0hr.df)+theme_bw()+geom_text(data=subset(rst.0hr.df, neg.log.p > 150),aes(prop, enrichment,label=name))
ggsave(ggplot.0hr, file="0hr.pdf");
##direct.label(ggplot.0hr, method = "bumpup")

rst.48hr.df <- data.frame(name=rst.48hr[,1], enrichment=rst.48hr[,7]/rst.48hr[,9], prop=rst.48hr[,7], neg.log.p=-rst.48hr[,4])

ggplot.48hr <- ggplot(aes(prop, enrichment), data=rst.48hr.df)+geom_point(aes(size=neg.log.p),data=rst.48hr.df)+theme_bw()+geom_text(data=subset(rst.48hr.df, neg.log.p > 150),aes(prop, enrichment,label=name))
ggsave(ggplot.48hr, file="48hr.pdf");

##direct.label(ggplot.0hr, method = "bumpup")

rst.shared.df <- data.frame(name=rst.shared[,1], enrichment=rst.shared[,7]/rst.shared[,9], prop=rst.shared[,7], neg.log.p=-rst.shared[,4])

ggplot.shared <- ggplot(aes(prop, enrichment), data=rst.shared.df)+geom_point(aes(size=neg.log.p),data=rst.shared.df)+theme_bw()+geom_text(data=subset(rst.shared.df, neg.log.p > 150),aes(prop, enrichment,label=name))
ggsave(ggplot.shared, file="shared.pdf");

##direct.label(ggplot.0hr, method = "bumpup")

# pdf("0hr.pdf");
# plot(rst.0hr[,7], -rst.0hr[,4]);
# dev.off();
#
# pdf("48hr.pdf");
# plot(rst.48hr[,7], -rst.48hr[,4]);
# dev.off();
#
# pdf("shared.pdf");
# plot(rst.shared[,7], -rst.shared[,4]);
# dev.off();
