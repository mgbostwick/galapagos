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
library(bestglm)

# Load modeling function
source("modeling.function.R")

# Load clean data and subset to appropriate variables for this analysis
load("BASES_CENSO_UPA_spss/clean_data.RData")
vars <-  read_excel("Variables.xlsx", sheet = "vars", range = "C2:I241")
workers_include <- vars[which(is.na(vars$`Number workers supported`)),1]$`Variable Name`
workers_x.df <- subset(reduced_data, select = workers_include)

workers_nonzero <- reduced_data[reduced_data$fulltimework > 0, 'fulltimework']
reduced_data$workers_binary <- factor(reduced_data$fulltimework > 0)
levels(reduced_data$workers_binary) = c("Zero", "Positive")

# Plot histograms and output to PDF
all_workers_hist <- ggplot(data = reduced_data) + geom_bar(mapping = aes(x = workers_binary)) +
  xlab("fulltimework") + theme(text = element_text(size=14))
nonzero_hist <- ggplot(data = reduced_data[reduced_data$fulltimework > 0,]) + 
  geom_histogram(mapping = aes(x = fulltimework)) + theme(text = element_text(size=14))
log_nonzero_hist <- ggplot(data = reduced_data[reduced_data$fulltimework > 0,]) + 
  geom_histogram(mapping = aes(x = log10(fulltimework))) + theme(text = element_text(size=14))
pdf("Paper/images/worker_histograms.pdf",width=12,height=8)
grid.arrange(all_workers_hist, nonzero_hist, log_nonzero_hist, nrow = 1)
dev.off()

# Call modeling function on binary data
worker_binary.models <- fit.models(model.name = "workers_binary", x.data = workers_x.df, 
                                y.response = reduced_data$workers_binary, response.family = "binomial")

# To find top 5 variable order
betas <-  worker_binary.models$betas
betas[abs(betas$s0)>0,'variable']
worker_binary.models$fwdorder

# Prepare nonzero data and remove zero variance/linear dependent columns
workers_x.nonzero.pre <- subset(reduced_data[reduced_data$fulltimework > 0,], select = workers_include)
num_workers_log <- log10(reduced_data[reduced_data$fulltimework > 0, 'fulltimework'])
workers.matrix <- model.matrix(~., workers_x.nonzero.pre)[,-1]
col_vars <- apply(workers.matrix, 2, var)
zero_variance <- names(col_vars[col_vars == 0])
workers_x.nonzero <- workers.matrix[,!colnames(workers.matrix) %in% zero_variance]
remove_vars <- c("ga15_cualLLANTAS", "`ENERGIA_ELENERGIA SOLAR PUBLICA`", "`ENERGIA_ELGENERADOR PRIVADO`")
workers_x.nonzero <- as.data.frame(workers_x.nonzero[,!colnames(workers_x.nonzero) %in% remove_vars])

# Call modeling function for nonzero data
worker_nonzero.models <- fit.models(model.name = "workers_nonzero", x.data = workers_x.nonzero, 
                             y.response = num_workers_log, response.family = "gaussian")

# To find top 5 variable order
betas <-  worker_nonzero.models$betas
betas[abs(betas$s7)>0,'variable']
worker_nonzero.models$fwdorder


### Diagnostics to find linear dependencies and correlations between variables
your.matrix <- workers_x.nonzero
rankifremoved <- sapply(1:ncol(your.matrix), function (x) qr(your.matrix[,-x])$rank)
which(rankifremoved == max(rankifremoved))

corrs <-cor(workers_x.nonzero)
corrs[upper.tri(corrs)] <- NA
diag(corrs) <- NA

corrs_reshape <- melt(corrs, na.rm = TRUE)
(high_corrs <- corrs_reshape[abs(corrs_reshape$value) > 0.9,])


