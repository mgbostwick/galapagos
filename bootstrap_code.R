set.seed(1)
bootstrap_coefs <- data.frame(matrix(ncol = ncol(x.matrix)+1, nrow = 0))
colnames(bootstrap_coefs) <- c("(Intercept)", colnames(x.matrix))

for (i in 1:1000){
  bootstrap_samples <- sample(1:nrow(x.matrix), size = nrow(x.matrix), replace = TRUE)
  weights <- table(factor(bootstrap_samples, levels = 1:nrow(x.matrix)))
  bs.elasticnet = glmnet(x.matrix,y.response,weights = weights,alpha=best_alpha, lambda = best.lambda)
  
  bs.coef <- t(data.frame(predict(bs.elasticnet,type="coefficients",s=best.lambda)[1:(ncol(x.matrix)+1),]))
  bootstrap_coefs <- rbind(bootstrap_coefs, bs.coef)
}

row.names(bootstrap_coefs) <- c(seq(1:1000))
top.names <- unique(c(elastic10.names, fwd.coefs10))
top.boostrap <- bootstrap_coefs[,top.names]

ggplot(data = melt(top.boostrap), aes(x=variable, y=value)) + geom_boxplot() + coord_flip()

pct_zeroes <- data.frame(colSums(top.boostrap != 0)/nrow(top.boostrap))
pct_zeroes <- rownames_to_column(pct_zeroes)
colnames(pct_zeroes) <- c("variable", "pct_zero")

ggplot(pct_zeroes) + geom_bar(stat='identity', aes(x=variable, y=pct_zero)) + coord_flip()