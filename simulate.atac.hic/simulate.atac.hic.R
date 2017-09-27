## simulate atac and hic

ind1 <- c(2,5,2,5,2)+rnorm(5);
ind2 <- c(2,3,2,3,2)+rnorm(5);
ind3 <- c(2,10,2,10,2)+rnorm(5);
ind4 <- c(2,5,2,5,2)+rnorm(5);
ind5 <- c(2,3,2,3,2)+rnorm(5);
ind6 <- c(2,10,2,10,2)+rnorm(5);

atac <- rbind(ind1,ind2,ind3,ind4,ind5,ind6);

atac.norm <- t(apply(atac, 1, function(x) {(x-mean(x))}))

atac.norm2 <- t(apply(atac, 1, function(x) {(x-mean(x[c(2,4)]))}))
