library(data.table);
library(ggplot2);
library(pheatmap);

rna.covars <- fread("covariates.baseline.matrix.eqtl.txt",row.names=1);
atac.covars <- fread("covariates.medianlog.txt",row.names=);

rna.covars.names <- rna.covars$V1;
rna.covars[,V1:=NULL];

atac.covars.names <- atac.covars$V1;
atac.covars[,V1:=NULL];

rna.covars.cor <- cor(t(rna.covars[1:29,]));
rownames(rna.covars.cor) <- colnames(rna.covars.cor) <- rna.covars.names[1:29];

atac.covars.cor <- cor(t(atac.covars[1:29,]));
rownames(atac.covars.cor) <- colnames(atac.covars.cor) <- atac.covars.names[1:29];

pdf("rna.covars.cor.pdf");
pheatmap(rna.covars.cor,cluster_rows=F, cluster_cols=F);
dev.off();

pdf("atac.covars.cor.pdf")
pheatmap(atac.covars.cor, cluster_rows=F, cluster_cols=F);
dev.off();
