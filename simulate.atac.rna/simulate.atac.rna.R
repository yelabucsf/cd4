library(qvalue);

## simulate atac and rna

atac.beta <- 0.4;
rna.beta <- 0.4;

n <- 100;

p <- 0.3;

hap1 <- rbinom(n, 1, 0.3);
hap2 <- rbinom(n, 1, 0.3);

gen <- hap1+hap2;

atac.ps <- NULL;
rna.ps <- NULL;
ps <- NULL;

for (i in 1:1000) {
  atac <- atac.beta*gen+rnorm(n,0.5);
  rna <- rna.beta*gen+rnorm(n,0.5);
  atac.ps <- c(atac.ps, summary(lm(atac~gen))$coefficients[2,4]);
  rna.ps <- c(rna.ps, summary(lm(rna~gen))$coefficients[2,4]);
  ps <- c(ps, summary(lm(rna~atac))$coefficients[2,4]);
}

atac.qs <- qvalue(atac.ps)$qvalues;
rna.qs <- qvalue(rna.ps)$qvalues;
qs <- qvalue(ps)$qvalues;
