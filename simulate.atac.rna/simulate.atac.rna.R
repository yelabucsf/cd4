library(qvalue);

## simulate atac and rna

atac.beta <- 0.6;
rna.beta <- 0.6;

n <- 100;

p <- 0.3;

hap1 <- rbinom(n, 1, 0.3);
hap2 <- rbinom(n, 1, 0.3);

gen <- hap1+hap2;

atac.ps <- NULL;
rna.ps <- NULL;
atac.r2 <- NULL;
rna.r2 <- NULL;
ps <- NULL;

for (i in 1:1000) {
  atac <- atac.beta*gen+rnorm(n,0.05);
  rna <- rna.beta*gen+rnorm(n,0.05);
  atac.ps <- c(atac.ps, summary(lm(atac~gen))$coefficients[2,4]);
  atac.r2 <- c(atac.r2, summary(lm(atac~gen))$r.squared);
  rna.ps <- c(rna.ps, summary(lm(rna~gen))$coefficients[2,4]);
  rna.r2 <- c(rna.r2, summary(lm(rna~gen))$r.squared);
  ps <- c(ps, summary(lm(rna~atac))$coefficients[2,4]);
}

atac.qs <- qvalue(atac.ps)$qvalues;
rna.qs <- qvalue(rna.ps)$qvalues;
qs <- qvalue(ps)$qvalues;
