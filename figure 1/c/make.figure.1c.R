library(data.table);
library(ggplot2);

# peaks.shared <- fread("shared.custom.genomeOntology.txt");
# peaks.48hr.diff <- fread("48hr.custom.genomeOntology.txt");
# peaks.0hr.diff <- fread("0hr.custom.genomeOntology.txt");
# peaks.48hr.all <- fread("48hr.all.custom.genomeOntology.txt");
# peaks.0hr.all <- fread("0hr.all.custom.genomeOntology.txt");
# peaks.shared.down <- fread("shared.down.custom.genomeOntology.txt");
# peaks.48hr.down.diff <- fread("48hr.down.custom.genomeOntology.txt");
# peaks.0hr.down.diff <- fread("0hr.down.custom.genomeOntology.txt");

##peaks.shared <- fread("shared.custom.genomeOntology.v2.txt");
peaks.48hr.diff <- fread("48hr.custom.genomeOntology.v2.txt");
peaks.0hr.diff <- fread("0hr.custom.genomeOntology.v2.txt");
##peaks.48hr.all <- fread("48hr.all.custom.genomeOntology.v2.txt");
##peaks.0hr.all <- fread("0hr.all.custom.genomeOntology.v2.txt");
peaks.shared.down <- fread("shared.down.custom.genomeOntology.v2.txt");
peaks.48hr.down.diff <- fread("48hr.down.custom.genomeOntology.v2.txt");
peaks.0hr.down.diff <- fread("0hr.down.custom.genomeOntology.v2.txt");


##peaks.shared <- peaks.shared[order(peaks.shared[,1,with=F]),];
peaks.48hr.diff <- peaks.48hr.diff[order(peaks.48hr.diff[,1,with=F]),];
peaks.0hr.diff <- peaks.0hr.diff[order(peaks.0hr.diff[,1,with=F]),];
peaks.shared.down <- peaks.shared.down[order(peaks.shared.down[,1,with=F]),];
peaks.48hr.down.diff <- peaks.48hr.down.diff[order(peaks.48hr.down.diff[,1,with=F]),];
peaks.0hr.down.diff <- peaks.0hr.down.diff[order(peaks.0hr.down.diff[,1,with=F]),];
peaks.48hr.all <- peaks.48hr.all[order(peaks.48hr.all[,1,with=F]),];
peaks.0hr.all <- peaks.0hr.all[order(peaks.0hr.all[,1,with=F]),];

names <- c("3'","5'","B.cell","CD34","Intergenic","Other.T","T.naive","TTS","Th.stim","Th0","Th1","Th17","Th2","Tmem","Treg","Exon","Intron","Non-coding","Other","Promoter");

peaks.48hr.diff <- peaks.48hr.diff[c(1:15,20:24),];
peaks.0hr.diff <- peaks.0hr.diff[c(1:15,20:24),];
peaks.shared <- peaks.shared[c(1:15,20:24),];

peaks.48hr.down.diff <- peaks.48hr.down.diff[c(1:15,20:24),];
peaks.0hr.down.diff <- peaks.0hr.down.diff[c(1:15,20:24),];
peaks.shared.down <- peaks.shared.down[c(1:15,20:24),];

peaks.48hr.all <- peaks.48hr.all[c(1:15,20:24),];
peaks.0hr.all <- peaks.0hr.all[c(1:15,20:24),];

peaks.df <- rbind(data.frame(name=names, overlaps=peaks.48hr.diff[,6,with=F], proportion=peaks.48hr.diff[,6,with=F]/28017, log.enrichment=peaks.48hr.diff[,9,with=F], neg.log.p=abs(peaks.48hr.diff[,10,with=F]), condition="48hr"),
data.frame(name=names, overlaps=peaks.0hr.diff[,6,with=F], proportion=peaks.0hr.diff[,6,with=F]/8298,log.enrichment=peaks.0hr.diff[,9,with=F], neg.log.p=abs(peaks.0hr.diff[,10,with=F]), condition="0hr"),
data.frame(name=names, overlaps=peaks.shared[,6,with=F], proportion=peaks.shared[,6,with=F]/27445,log.enrichment=peaks.shared[,9,with=F], neg.log.p=abs(peaks.shared[,10,with=F]), condition="shared"),

data.frame(name=names, overlaps=peaks.48hr.down.diff[,6,with=F], proportion=peaks.48hr.down.diff[,6,with=F]/24665, log.enrichment=peaks.48hr.down.diff[,9,with=F], neg.log.p=abs(peaks.48hr.down.diff[,10,with=F]), condition="48hr.down"),
data.frame(name=names, overlaps=peaks.0hr.down.diff[,6,with=F], proportion=peaks.0hr.down.diff[,6,with=F]/17313,log.enrichment=peaks.0hr.down.diff[,9,with=F], neg.log.p=abs(peaks.0hr.down.diff[,10,with=F]), condition="0hr.down"),
data.frame(name=names, overlaps=peaks.shared.down[,6,with=F], proportion=peaks.shared.down[,6,with=F]/21780,log.enrichment=peaks.shared.down[,9,with=F], neg.log.p=abs(peaks.shared.down[,10,with=F]), condition="shared.down"),

data.frame(name=names, overlaps=peaks.48hr.all[,6,with=F], proportion=peaks.48hr.all[,6,with=F]/24665, log.enrichment=peaks.48hr.all[,9,with=F], neg.log.p=abs(peaks.48hr.all[,10,with=F]), condition="48hr.all"),
data.frame(name=names, overlaps=peaks.0hr.all[,6,with=F], proportion=peaks.0hr.all[,6,with=F]/17313,log.enrichment=peaks.0hr.all[,9,with=F], neg.log.p=abs(peaks.0hr.all[,10,with=F]), condition="0hr.all"))

# data.frame(name=names, overlaps=peaks.48hr.all[,6,with=F], proportion=peaks.48hr.all[,6,with=F]/28017, log.enrichment=peaks.48hr.all[,9,with=F], neg.log.p=abs(peaks.48hr.all[,10,with=F]), condition="48hr.all"),
# data.frame(name=names, overlaps=peaks.0hr.all[,6,with=F], proportion=peaks.0hr.all[,6,with=F]/8298,log.enrichment=peaks.0hr.all[,9,with=F], neg.log.p=abs(peaks.0hr.all[,10,with=F]), condition="0hr.all"))

colnames(peaks.df) <- c("names","overlaps","proportion","log.enrichment","neg.log.p","condition");

ggplot <- ggplot(aes(proportion, exp(log.enrichment),label=names),data=peaks.df)+geom_point(aes(size=neg.log.p))+theme_bw()+geom_text(nudge_y=.25)+facet_wrap(~condition);
ggsave(ggplot, file="all.pdf",width=7,height=7)



peaks.down.df <- rbind(
                  data.frame(name=names, overlaps=peaks.48hr.down.diff[,6,with=F], proportion=peaks.48hr.down.diff[,6,with=F]/24665, log.enrichment=peaks.48hr.down.diff[,9,with=F], neg.log.p=abs(peaks.48hr.down.diff[,10,with=F]), condition="48hr.down"),
                  data.frame(name=names, overlaps=peaks.0hr.down.diff[,6,with=F], proportion=peaks.0hr.down.diff[,6,with=F]/17313,log.enrichment=peaks.0hr.down.diff[,9,with=F], neg.log.p=abs(peaks.0hr.down.diff[,10,with=F]), condition="0hr.down"),
                  data.frame(name=names, overlaps=peaks.shared.down[,6,with=F], proportion=peaks.shared.down[,6,with=F]/21780,log.enrichment=peaks.shared.down[,9,with=F], neg.log.p=abs(peaks.shared.down[,10,with=F]), condition="shared.down"))
# data.frame(name=names, overlaps=peaks.48hr.all[,6,with=F], proportion=peaks.48hr.all[,6,with=F]/28017, log.enrichment=peaks.48hr.all[,9,with=F], neg.log.p=abs(peaks.48hr.all[,10,with=F]), condition="48hr.all"),
# data.frame(name=names, overlaps=peaks.0hr.all[,6,with=F], proportion=peaks.0hr.all[,6,with=F]/8298,log.enrichment=peaks.0hr.all[,9,with=F], neg.log.p=abs(peaks.0hr.all[,10,with=F]), condition="0hr.all"))

colnames(peaks.down.df) <- c("names","overlaps","proportion","log.enrichment","neg.log.p","condition");

ggplot <- ggplot(aes(proportion, exp(log.enrichment),label=names),data=peaks.down.df)+geom_point(aes(size=neg.log.p))+theme_bw()+geom_text(nudge_y=.25)+facet_wrap(~condition);
ggsave(ggplot, file="down.pdf",width=7,height=7)
