setwd("/Users/michaelbostwick/Documents/Galapagos")

library(tidyverse)
library(ggplot2)
library(readxl)
library(glmnet)
library(mpath)
library(reshape2)
library(gridExtra)
library(leaps)
library(bestsubset)

source("modeling.function.R")

load("BASES_CENSO_UPA_spss/clean_data.RData")
vars <-  read_excel("Variables.xlsx", sheet = "vars", range = "C2:I241")

netincome_include <- vars[which(is.na(vars$`Net Income`)),1]$`Variable Name`



full_hist <- ggplot(data = reduced_data) + geom_histogram(mapping = aes(x = netincome))
reduced_hist <- ggplot(data = reduced_data) + geom_histogram(mapping = aes(x = netincome)) + 
  scale_x_continuous(limits = c(-3e+04, 3e+04))
pdf("Paper/images/netincome_histograms.pdf",width=12,height=8)
grid.arrange(full_hist, reduced_hist, nrow = 1)
dev.off()

netincome_x.df <- subset(reduced_data[abs(reduced_data$netincome) < sd(reduced_data$netincome),], select = netincome_include)
netincome <- reduced_data[abs(reduced_data$netincome) < sd(reduced_data$netincome), 'netincome']


netincome.matrix <- model.matrix(~., netincome_x.df)[,-1]
col_vars <- apply(netincome.matrix, 2, var)
zero_variance <- names(col_vars[col_vars == 0])

netincome_x.nonzero <- netincome.matrix[,!colnames(netincome.matrix) %in% zero_variance]


### Diagnostics to find linear dependencies and correlations between variables
your.matrix <- netincome_x.nonzero
rankifremoved <- sapply(1:ncol(your.matrix), function (x) qr(your.matrix[,-x])$rank)
which(rankifremoved == max(rankifremoved))

corrs <-cor(netincome_x.nonzero)
corrs[upper.tri(corrs)] <- NA
diag(corrs) <- NA

corrs_reshape <- melt(corrs, na.rm = TRUE)
(high_corrs <- corrs_reshape[abs(corrs_reshape$value) > 0.9,])


netincome.models <- fit.models(model.name = "netincome", x.data = as.data.frame(netincome_x.nonzero), 
                                y.response = netincome, response.family = "gaussian")
