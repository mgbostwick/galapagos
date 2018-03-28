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

# Load modeling function
source("modeling.function.R")

# Load clean data and subset to appropriate variables for this analysis
load("BASES_CENSO_UPA_spss/clean_data.RData")
vars <-  read_excel("Variables.xlsx", sheet = "vars", range = "C2:I241")
invasive_include <- vars[which(is.na(vars$`Surface area with invasive species`)),1]$`Variable Name`
invasive_x.df <- subset(reduced_data, select = invasive_include)
reduced_data$invasive <- factor(reduced_data$percinv > 0)
levels(reduced_data$invasive) = c("Zero", "Positive")

# Plot histograms and output to PDF
invasive_hist <- ggplot(data = reduced_data) + geom_bar(mapping = aes(x = invasive)) +
  theme(text = element_text(size=14))
nonzero_hist <- ggplot(data = reduced_data[reduced_data$percinv > 0,], aes(x = percinv)) + 
  geom_histogram() + stat_bin(binwidth = 10) + theme(text = element_text(size=14))
log_hist <- ggplot(data = reduced_data[reduced_data$percinv > 0,], aes(x = log10(percinv))) + 
  geom_histogram() + stat_bin(binwidth = 0.25) + theme(text = element_text(size=14))
pdf("Paper/images/invasive_histograms.pdf",width=12,height=8)
grid.arrange(invasive_hist, nonzero_hist, log_hist, nrow = 1)
dev.off()

# Call modeling function for binary data
invasive_binary.models <- fit.models(model.name = "invasive_binary", x.data = invasive_x.df, 
                                   y.response = reduced_data$invasive, response.family = "binomial")

# To find top 5 variable order
betas <-  invasive_binary.models$betas
betas[abs(betas$s11)>0,'variable']
invasive_binary.models$fwdorder

# Prepare nonzero data and remove zero variance/linear dependent columns
log_invasive_nonzero <- log10(reduced_data[reduced_data$percinv > 0.1, 'percinv'])
invasive.matrix <- model.matrix(~., invasive_x.df[reduced_data$percinv > 0.1,])[,-1]
col_vars <- apply(invasive.matrix, 2, var)
zero_variance <- names(col_vars[col_vars == 0])
invasive_x.nonzero <- invasive.matrix[,!colnames(invasive.matrix) %in% zero_variance]
remove_vars <- c("ga15_cualCOMPRA DE ESMERIL", "tp52_g")
invasive_x.nonzero <- as.data.frame(invasive_x.nonzero[,!colnames(invasive_x.nonzero) %in% remove_vars])

# Call modeling function for nonzero data
invasive_nonzero.models <- fit.models(model.name = "invasive_nonzero", x.data = invasive_x.nonzero, 
                                y.response = log_invasive_nonzero, response.family = "gaussian")

# To find top 5 variable order
betas <-  invasive_nonzero.models$betas
betas[abs(betas$s7)>0,'variable']
invasive_nonzero.models$fwdorder

### Diagnostics to find linear dependencies and correlations between variables
your.matrix <- invasive_x.nonzero
rankifremoved <- sapply(1:ncol(your.matrix), function (x) qr(your.matrix[,-x])$rank)
which(rankifremoved == max(rankifremoved))

corrs <-cor(invasive_x.nonzero)
corrs[upper.tri(corrs)] <- NA
diag(corrs) <- NA

corrs_reshape <- melt(corrs, na.rm = TRUE)
(high_corrs <- corrs_reshape[abs(corrs_reshape$value) > 0.9,])
