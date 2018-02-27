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

invasive_include <- vars[which(is.na(vars$`Surface area with invasive species`)),1]$`Variable Name`
invasive_x.df <- subset(reduced_data, select = invasive_include)

reduced_data$invasive <- 1*(reduced_data$percinv > 0)

invasive_hist <- ggplot(data = reduced_data) + geom_bar(mapping = aes(x = factor(invasive)))
nonzero_hist <- ggplot(data = reduced_data[reduced_data$percinv > 0,], aes(x = percinv)) + 
  geom_histogram() + stat_bin(binwidth = 10)
log_hist <- ggplot(data = reduced_data[reduced_data$percinv > 0,], aes(x = log10(percinv))) + 
  geom_histogram() + stat_bin(binwidth = 0.25)

pdf("Paper/images/invasive_histograms.pdf",width=12,height=8)
grid.arrange(invasive_hist, nonzero_hist, log_hist, nrow = 1)
dev.off()

invasive_binary.models <- fit.models(model.name = "invasive_binary", x.data = invasive_x.df, 
                                   y.response = reduced_data$invasive, response.family = "binomial")


log_invasive_nonzero <- log10(reduced_data[reduced_data$percinv > 0, 'percinv'])

invasive.matrix <- model.matrix(~., invasive_x.df[reduced_data$invasive == 1,])[,-1]
col_vars <- apply(invasive.matrix, 2, var)
zero_variance <- names(col_vars[col_vars == 0])

invasive_x.nonzero <- invasive.matrix[,!colnames(invasive.matrix) %in% zero_variance]
remove_vars <- c("ga15_cualCOMPRA DE ESMERIL", "tp52_g")
invasive_x.nonzero <- as.data.frame(invasive_x.nonzero[,!colnames(invasive_x.nonzero) %in% remove_vars])

invasive_nonzero.models <- fit.models(model.name = "invasive_nonzero", x.data = invasive_x.nonzero, 
                                y.response = log_invasive_nonzero, response.family = "gaussian")

### Diagnostics to find linear dependencies and correlations between variables
your.matrix <- invasive_x.nonzero
rankifremoved <- sapply(1:ncol(your.matrix), function (x) qr(your.matrix[,-x])$rank)
which(rankifremoved == max(rankifremoved))

corrs <-cor(invasive_x.nonzero)
corrs[upper.tri(corrs)] <- NA
diag(corrs) <- NA

corrs_reshape <- melt(corrs, na.rm = TRUE)
(high_corrs <- corrs_reshape[abs(corrs_reshape$value) > 0.9,])
