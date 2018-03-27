fit.models <- function(model.name, x.data, y.response, response.family){
  
  source("plot.cv.big.R")
  environment(plot.cv.big) <- asNamespace('glmnet')
  
  # For testing
  #model.name <- "production"
  #x.data <- production_x.df
  #y.response <- log_productivity
  #response.family <- "gaussian"
  
  x.matrix <- model.matrix(~., x.data)[,-1]
  
  # Elastic net modeling
  set.seed(1)
  foldid=sample(1:10,size=length(y.response),replace=TRUE)
  if (response.family == "gaussian"){
    cv1=cv.glmnet(x.matrix,y.response,family=response.family,foldid=foldid,alpha=1)
    cv.75=cv.glmnet(x.matrix,y.response,family=response.family,foldid=foldid,alpha=0.75)
    cv.5=cv.glmnet(x.matrix,y.response,family=response.family,foldid=foldid,alpha=.5)
    cv.25=cv.glmnet(x.matrix,y.response,family=response.family,foldid=foldid,alpha=0.25)
  }
  if (response.family == "binomial") {
    cv1=cv.glmnet(x.matrix,y.response,family=response.family,type.measure = "class",foldid=foldid,alpha=1)
    cv.75=cv.glmnet(x.matrix,y.response,family=response.family,type.measure = "class",foldid=foldid,alpha=0.75)
    cv.5=cv.glmnet(x.matrix,y.response,family=response.family,type.measure = "class",foldid=foldid,alpha=.5)
    cv.25=cv.glmnet(x.matrix,y.response,family=response.family,type.measure = "class",foldid=foldid,alpha=0.25)
  } 
  if (response.family == "multinomial") {
    cv1=cv.glmnet(x.matrix,y.response,family=response.family,type.measure = "class",foldid=foldid,alpha=1)
    cv.75=cv.glmnet(x.matrix,y.response,family=response.family,type.measure = "class",foldid=foldid,alpha=0.75)
    cv.5=cv.glmnet(x.matrix,y.response,family=response.family,type.measure = "class",foldid=foldid,alpha=.5)
    cv.25=cv.glmnet(x.matrix,y.response,family=response.family,type.measure = "class",foldid=foldid,alpha=0.25)
  }
  
  # Determine best alpha value
  models <- vector(mode="list", length=4)
  
  models[[1]] <- cv.25
  models[[2]] <- cv.5 
  models[[3]] <- cv.75 
  models[[4]] <- cv1

  alphas <- c(0.25,0.5,0.75,1)

  cv1.mse <- cv1$cvm[cv1$lambda == cv1$lambda.1se]
  cv.75.mse <- cv.75$cvm[cv.75$lambda == cv.75$lambda.1se]
  cv.5.mse <- cv.5$cvm[cv.5$lambda == cv.5$lambda.1se]
  cv.25.mse <- cv.25$cvm[cv.25$lambda == cv.25$lambda.1se]
  
  mses <- c(cv.25.mse, cv.5.mse, cv.75.mse, cv1.mse)
  best_model <- models[[which.min(mses)]]
  best_alpha <- alphas[which.min(mses)]
  
  elastic.full <- glmnet(x.matrix,y.response,family=response.family,alpha=best_alpha)
 
  # Find lambda value that includes 5 variables
  found_df = FALSE
  df = 5
  while (found_df == FALSE){
    if (length(elastic.full$lambda[elastic.full$df == df]) > 0){
      lambda.5var <- elastic.full$lambda[elastic.full$df == df]  
      found_df = TRUE
    } else {
      df = df + 1
    }
  }
  
  # Find RMSE for 5 variable model
  found_df = FALSE
  df = 5
  while (found_df == FALSE){
    if (length(best_model$cvm[best_model$glmnet.fit$df == df]) > 0){
      elastic5.cvm <- best_model$cvm[best_model$glmnet.fit$df == df][1]  
      found_df = TRUE
    } else {
      df = df + 1
    }
  }
  
  betas <- as.data.frame(as.matrix(elastic.full$beta))
  betas <- rownames_to_column(betas, var = "variable")
  
  # Find variable names of first 5 variables
  elastic.coef5 <- coef(elastic.full,s=lambda.5var[1])[1:ncol(x.matrix),] 
  elastic.coef5.nonzero <- elastic.coef5[elastic.coef5!=0][-1]
  elastic5.names <- names(elastic.coef5.nonzero)

  # Find direction of relationship for 5 variables
  sign_check <- function(x) {
    if (x > 0) {
      result <- "(+)"
    }
    else if (x < 0) {
      result <- "(-)"
    }
    return(result)
  }
  
  elastic5.signs <- lapply(elastic.coef5.nonzero, sign_check)
  
  ############ Different procedure for multinomial model #############
  if (response.family == "multinomial"){elastic.coef.best <- coef(elastic.full,s=best.lambda)
  if (best_model$lambda.1se == best_model$lambda[1]){
    best.lambda = best_model$lambda.min
  } else {
    best.lambda = best_model$lambda.1se
  }
  
  
  elastic.coef.best <- coef(elastic.full,s=best.lambda)
  elastic.coef.best1 <- elastic.coef.best$`1`[1:ncol(x.matrix),] 
  elastic.coef.best1 <- as.data.frame(elastic.coef.best1[elastic.coef.best1!=0])
  elastic.coef.best1 <- rownames_to_column(elastic.coef.best1, "variable")
  elastic.coef.best2 <- elastic.coef.best$`2`[1:ncol(x.matrix),] 
  elastic.coef.best2 <- as.data.frame(elastic.coef.best2[elastic.coef.best2!=0])
  elastic.coef.best2 <- rownames_to_column(elastic.coef.best2, "variable")
  elastic.coef.best3 <- elastic.coef.best$`3`[1:ncol(x.matrix),] 
  elastic.coef.best3 <- as.data.frame(elastic.coef.best3[elastic.coef.best3!=0])
  elastic.coef.best3 <- rownames_to_column(elastic.coef.best3, "variable")
  elastic.coef.best4 <- elastic.coef.best$`4`[1:ncol(x.matrix),] 
  elastic.coef.best4 <- as.data.frame(elastic.coef.best4[elastic.coef.best4!=0])
  elastic.coef.best4 <- rownames_to_column(elastic.coef.best4, "variable")
  elastic.coef.best5 <- elastic.coef.best$`5`[1:ncol(x.matrix),] 
  elastic.coef.best5 <- as.data.frame(elastic.coef.best5[elastic.coef.best5!=0])
  elastic.coef.best5 <- rownames_to_column(elastic.coef.best5, "variable")
  
  coefs_list <- list(elastic.coef.best1,elastic.coef.best2,elastic.coef.best3,elastic.coef.best4,elastic.coef.best5)
  
  elastic.merged_coefs <- coefs_list %>%
    Reduce(function(dtf1,dtf2) full_join(dtf1,dtf2,by="variable"), .)
  
  
  colnames(elastic.merged_coefs) <- c("variable", 'percperm','perctemp','perctill','percpasture','percbrush') 
  elastic.merged_coefs$variable <- gsub("[^[:alnum:] ]", "", elastic.merged_coefs$variable)
  
  path <- sprintf("Paper/fullcoeflist_%s.csv", model.name)
  write.csv(elastic.merged_coefs, path, row.names = FALSE)
  
  best_model$cvm[21]
  
  elastic.coef.5 <- coef(elastic.full,s=0.05)
  elastic.coef5.1 <- elastic.coef.5$`1`[1:ncol(x.matrix),] 
  elastic.coef5.1 <- elastic.coef5.1[elastic.coef5.1!=0][-1]
  elastic.coef5.2 <- elastic.coef.5$`2`[1:ncol(x.matrix),] 
  elastic.coef5.2 <- elastic.coef5.2[elastic.coef5.2!=0][-1]
  elastic.coef5.3 <- elastic.coef.5$`3`[1:ncol(x.matrix),] 
  elastic.coef5.3 <- elastic.coef5.3[elastic.coef5.3!=0][-1]
  elastic.coef5.4 <- elastic.coef.5$`4`[1:ncol(x.matrix),] 
  elastic.coef5.4 <- elastic.coef5.4[elastic.coef5.4!=0][-1]
  #elastic.coef.5 <- coef(elastic.full,s=0.06)
  elastic.coef5.5 <- elastic.coef.5$`5`[1:ncol(x.matrix),] 
  elastic.coef5.5 <- elastic.coef5.5[elastic.coef5.5!=0][-1]
  #elastic.coef5.5
  
  elastic5.signs1 <- lapply(elastic.coef5.1, sign_check)
  elastic5.signs2 <- lapply(elastic.coef5.2, sign_check)
  elastic5.signs3 <- lapply(elastic.coef5.3, sign_check)
  elastic5.signs4 <- lapply(elastic.coef5.4, sign_check)
  elastic5.signs5 <- lapply(elastic.coef5.5, sign_check)
  
  
  elastic5.names1 <- trimws(gsub("[^[:alnum:] ]", "", names(elastic.coef5.1)))
  elastic5.names1 <- paste(elastic5.names1, elastic5.signs1, sep=" ") 
  elastic5.names2 <- trimws(gsub("[^[:alnum:] ]", "", names(elastic.coef5.2)))
  elastic5.names2 <- paste(elastic5.names2, elastic5.signs2, sep=" ") 
  elastic5.names3 <- trimws(gsub("[^[:alnum:] ]", "", names(elastic.coef5.3)))
  elastic5.names3 <- paste(elastic5.names3, elastic5.signs3, sep=" ") 
  elastic5.names4 <- trimws(gsub("[^[:alnum:] ]", "", names(elastic.coef5.4)))
  elastic5.names4 <- paste(elastic5.names4, elastic5.signs4, sep=" ") 
  elastic5.names5 <- trimws(gsub("[^[:alnum:] ]", "", names(elastic.coef5.5)))
  elastic5.names5 <- paste(elastic5.names5, elastic5.signs5, sep=" ") 
  max.length <- max(length(elastic.coef5.1),length(elastic.coef5.2),length(elastic.coef5.3),length(elastic.coef5.4),
      length(elastic.coef5.5))
  
  elastic.names.list <- list(elastic5.names1,elastic5.names2,elastic5.names3,elastic5.names4,elastic5.names5)
  for (i in 1:5){
    dif <- max.length - length(elastic.names.list[[i]])
    elastic.names.list[[i]] <- c(sort(elastic.names.list[[i]]), rep(" ", dif))
  }
 
   
  top5coef <- cbind(elastic.names.list[[1]],elastic.names.list[[2]],elastic.names.list[[3]],
                    elastic.names.list[[4]],elastic.names.list[[5]])
  colnames(top5coef) <- c('percperm','perctemp','perctill','percpasture','percbrush')
  
  
  path <- sprintf("Paper/%s_top5names.csv", model.name)
  write.csv(top5coef, path, row.names = FALSE)
  
  }
    
  
  # Output Elastic Net CV plot  
  path <- sprintf("Paper/images/elastic_cv_%s.pdf", model.name)
  pdf(path,width=12,height=8)
  par(mar=c(5,5,5,2)+0.1)
  plot.cv.big(best_model, cex.axis = 1.5, cex.lab = 2);title(main=sprintf("Alpha = %s", best_alpha), line = 3, cex.main = 2.25);
  dev.off()

  # Determine optimal lambda value
  if (best_model$lambda.1se == best_model$lambda[1]){
    best.lambda = best_model$lambda.min
  } else {
    best.lambda = best_model$lambda.1se
  }
  
  
  elastic.coef.best <- coef(elastic.full,s=best.lambda)[1:ncol(x.matrix),] 
  elastic.coef.best.nonzero <- elastic.coef.best[elastic.coef.best!=0]
  elastic.best.names <- names(elastic.coef.best.nonzero)
  elastic.full.cvm <- best_model$cvm[best_model$lambda == best.lambda]
  elastic.predicts <- predict(elastic.full, newx = x.matrix,  type = "response", s = best.lambda)
  elastic.resids <- (y.response - elastic.predicts)
  
  
  # Forward Stepwise modeling
  if (response.family == "gaussian"){
    best.fwd <- regsubsets(y.response~., data=x.data ,nvmax=min(100,nrow(x.data)),method = "forward")
    best.fwd.summary <- summary(best.fwd)
    
    fwd.coefs <- coef(best.fwd, which.min(best.fwd.summary$bic))
    incl_vars <- names(fwd.coefs)[-1]
    incl_x <- x.matrix[,incl_vars]
    fwd.predicts <- incl_x %*% fwd.coefs[-1] + fwd.coefs[1]
    fwd.resids <- (y.response - fwd.predicts)
    
    fwd.coefs5 <- colnames(x.matrix)[best.fwd$vorder[1:5]]
    fwd.order <- fwd.coefs5
    
    xdata.5 <- as.data.frame(x.matrix[,fwd.coefs5])
    fwd.fit5 <- lm(y.response ~ ., data = xdata.5)
    fwd.coefs5.signs <- lapply(coef(fwd.fit5)[-1], sign_check)
    
    fwd5.r2 <- best.fwd.summary$rsq[5]
    fwd.full.r2 <- best.fwd.summary$rsq[which.min(best.fwd.summary$bic)]
    
    path <- sprintf("Paper/images/forward_nvars_%s.pdf", model.name)
    pdf(path,width=12,height=8)
    par(mar=c(5,5,5,2)+0.1)
    plot(best.fwd.summary$bic ,xlab="Number of Variables ",ylab="BIC", type='l',
         xlim=c(length(best.fwd.summary$bic),0), cex.axis = 1.5, cex.lab = 2)
    min.bic <- which.min(best.fwd.summary$bic)
    points(min.bic,best.fwd.summary$bic[min.bic],col="red",cex=2,pch=20)
    dev.off()
  } 
  if (response.family == "binomial") {
    fwd.data <- cbind(x.data,y.response)
    model.null = glm(y.response ~ 1, data=fwd.data, family = binomial(link="logit"))
    
    model.full = glm(y.response ~ ., data=fwd.data, family = binomial(link="logit"))
    
    fwd.steps <- step(model.null,scope = list(upper=model.full), direction="forward", data=fwd.data,
                    k = log(nrow(fwd.data)), trace = 0)
    
    path <- sprintf("Paper/images/forward_nvars_%s.pdf", model.name)
    pdf(path,width=12,height=8)
    par(mar=c(5,5,5,2)+0.1)
    plot(fwd.steps$anova$AIC, type= "l", ylab = "BIC", xlab = "Number of Variables",
         xlim=c(length(fwd.steps$anova$AIC),0), cex.axis = 1.5, cex.lab = 2)
    points(which.min(fwd.steps$anova$AIC),min(fwd.steps$anova$AIC),col="red",cex=2,pch=20)
    dev.off()
    
    # fwd.coefs5 <- fwd.steps$anova$Step[2:6]
    # class(fwd.coefs5) <- class(fwd.coefs5)[-match("AsIs", class(fwd.coefs5))]
    # fwd.coefs5 <- trimws(gsub("[^[:alnum:] ]", "", fwd.coefs5))
    
    fwd.coefs5 <- names(fwd.steps$coefficients)[2:6]
    fwd.order <- fwd.coefs5
    fwd.coefs5.signs <- lapply(fwd.steps$coefficients[2:6], sign_check)
  
    
    fwd.coefs <- fwd.steps$coefficients
    
    fwd.predicted <- 1*(fwd.steps$fitted.values > 0.5)
    fwd.correct <- (fwd.predicted == fwd.steps$y)
    fwd.plot <- data.frame(cbind(fwd.steps$fitted.values, fwd.predicted, fwd.correct))
    colnames(fwd.plot) <- c("Predicted Probability", "Predicted", "Correct")
    path <- sprintf("Paper/images/binarypreds_%s.pdf", model.name)
    pdf(path,width=12,height=8)
    ggplot(fwd.plot, aes(`Predicted Probability`, factor(Predicted))) + geom_point(aes(colour = factor(fwd.correct)))
    dev.off()
    
    fwd.steps5 <- step(model.null,scope = list(upper=model.full), direction="forward", data=fwd.data,
                      k = log(nrow(fwd.data)), steps = 5, trace = 0)
    
    fwd5.r2 <- mean(1*(fwd.steps5$fitted.values > 0.5) == fwd.steps5$y)
    fwd.full.r2 <- mean(fwd.correct)
  }
  if (response.family == "gaussian"){
    path <- sprintf("Paper/images/resids_%s.pdf", model.name)
    pdf(path,width=12,height=8)
    par(mfrow=c(2,2))
    par(mar=c(5,5,5,2)+0.1)
    plot(elastic.predicts, elastic.resids, cex.axis = 1.5, cex.lab = 2, cex.main = 2,
         main = "Elastic Net Residuals", xlab = "Predicted Values", ylab = "Residual Values")
    qqnorm(elastic.resids,  cex.axis = 1.5, cex.lab = 2, cex.main = 2)
    plot(fwd.predicts, fwd.resids, cex.axis = 1.5, cex.lab = 2,cex.main = 2, 
         main = "Forward Selection Residuals", xlab = "Predicted Values", ylab = "Residual Values")
    qqnorm(fwd.resids, cex.axis = 1.5, cex.lab = 2, cex.main = 2)
    dev.off()
    par(mfrow=c(1,1))

  }
  
  elastic5.names <- trimws(gsub("[^[:alnum:] ]", "", elastic5.names))
  elastic5.names <- paste(elastic5.names, elastic5.signs, sep=" ") 
  
  fwd.coefs5 <- trimws(gsub("[^[:alnum:] ]", "", fwd.coefs5))
  fwd.coefs5 <- paste(fwd.coefs5, fwd.coefs5.signs, sep=" ") 
  
  if (length(elastic5.names) > length(fwd.coefs5)){
    dif <- length(elastic5.names) - length(fwd.coefs5)
    fwd.coefs5 <- c(sort(fwd.coefs5), rep(" ", dif))
    elastic5.names <- sort(elastic5.names)
  } else if(length(elastic5.names) < length(fwd.coefs5)){
    dif <- length(fwd.coefs5) - length(elastic5.names) 
    elastic5.names <- c(sort(elastic5.names), rep(" ", dif))
    fwd.coefs5 <- sort(fwd.coefs5)
  } else {
    elastic5.names <- sort(elastic5.names)
    fwd.coefs5 <- sort(fwd.coefs5)
  }
  
  top5coef <- cbind(elastic5.names, fwd.coefs5)
  colnames(top5coef) <- c("elasticnet", "forward")
  

  path <- sprintf("Paper/%s_top5names.csv", model.name)
  #write.csv(top5coef, path, row.names = FALSE)

  elastic.coef.best.nonzero <- as.data.frame(elastic.coef.best.nonzero)
  elastic.coef.best.nonzero <- rownames_to_column(elastic.coef.best.nonzero)
  fwd.coefs <- as.data.frame(fwd.coefs)
  fwd.coefs <- rownames_to_column(fwd.coefs)
  merged_best_coefs <- full_join(elastic.coef.best.nonzero, fwd.coefs, by="rowname")
  colnames(merged_best_coefs) <- c("variable", "elastic", "forward")
  merged_best_coefs$variable <- gsub("[^[:alnum:] ]", "", merged_best_coefs$variable)
  path <- sprintf("Paper/fullcoeflist_%s.csv", model.name)
  #write.csv(merged_best_coefs, path, row.names = FALSE)
  
  results <- list("elastic5.cvm" = elastic5.cvm, "elastic.full.cvm" = elastic.full.cvm, 
                  "fwd5.r2" = fwd5.r2, "fwd.full.r2" = fwd.full.r2, "betas" = betas, "fwdorder" = fwd.order)

  return(results)
}

