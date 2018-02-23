fit.models <- function(model.name, x.data, y.response, response.family){
  
  # For testing
  x.data <- workers_x.df
  y.response <- reduced_data$workers_binary
  response.family <- "binomial"
  
  x.matrix <- model.matrix(~., x.data)[,-1]
  
  # Elastic net modeling
  set.seed(1)
  foldid=sample(1:10,size=length(y.response),replace=TRUE)
  if (response.family == "gaussian"){
    cv1=cv.glmnet(x.matrix,y.response,family=response.family,foldid=foldid,alpha=1)
    cv.75=cv.glmnet(x.matrix,y.response,family=response.family,foldid=foldid,alpha=0.75)
    cv.5=cv.glmnet(x.matrix,y.response,family=response.family,foldid=foldid,alpha=.5)
    cv.25=cv.glmnet(x.matrix,y.response,family=response.family,foldid=foldid,alpha=0.25)
  } else {
    cv1=cv.glmnet(x.matrix,y.response,family=response.family,type.measure = "class",foldid=foldid,alpha=1)
    cv.75=cv.glmnet(x.matrix,y.response,family=response.family,type.measure = "class",foldid=foldid,alpha=0.75)
    cv.5=cv.glmnet(x.matrix,y.response,family=response.family,type.measure = "class",foldid=foldid,alpha=.5)
    cv.25=cv.glmnet(x.matrix,y.response,family=response.family,type.measure = "class",foldid=foldid,alpha=0.25)
  }
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
  #lambda.seq <- c(seq(1, 0.64, by=-0.05), elastic.nvars$lambda)
  #elastic.nvars <- glmnet(x.matrix,y.response,family=response.family,alpha=best_alpha, lambda = lambda.seq)

  found_df = FALSE
  df = 6
  while (found_df == FALSE){
    if (length(elastic.full$lambda[elastic.full$df == df]) > 0){
      lambda.10var <- elastic.full$lambda[elastic.full$df == df]  
      found_df = TRUE
    } else {
      df = df + 1
    }
  }
  
  
  elastic.coef10 <- coef(elastic.full,s=lambda.10var[1])[1:ncol(x.matrix),] 
  elastic.coef10.nonzero <- elastic.coef10[elastic.coef10!=0][2:6]
  elastic10.names <- names(elastic.coef10.nonzero)

  path <- sprintf("Paper/images/elastic_cv_%s.pdf", model.name)
  pdf(path,width=12,height=8)
  par(mfrow=c(2,2))
  plot(cv1);title(main="Alpha = 1", line = 2.1);
  plot(cv.75);title(main="Alpha = .75", line = 2.1);
  plot(cv.5);title(main="Alpha = .5", line = 2.1);
  plot(cv.25);title(main="Alpha = .25", line = 2.1);
  dev.off()
  #plot(log(cv1$lambda),cv1$cvm,pch=19,col="red",xlab="log(Lambda)",ylab=cv1$name,  main="Alpha = 1")
  #points(log(cv.5$lambda),cv.5$cvm,pch=19,col="grey", main="Alpha = 0.5")
  #points(log(cv0$lambda),cv0$cvm,pch=19,col="blue", main="Alpha = 0")
  par(mfrow=c(1,1))
  
  elastic.coef.best <- coef(elastic.full,s=best_model$lambda.1se)[1:ncol(x.matrix),] 
  elastic.coef.best.nonzero <- elastic.coef.best[elastic.coef.best!=0]
  elastic.best.names <- names(elastic.coef.best.nonzero)

  if (best_model$lambda.1se == best_model$lambda[1]){
    best.lambda = best_model$lambda.min
  } else {
    best.lambda = best_model$lambda.1se
  }
  
  elastic.predicts <- predict(elastic.full, newx = x.matrix,  type = "response", s = best.lambda)
  #elastic.predict_class <- predict(elastic.full, newx = x.matrix,  type = "class", s = best.lambda)
  elastic.resids <- (y.response - elastic.predicts)
  
  #mean(y.response == elastic.predict_class)
  
  # Forward/Backward modeling
  if (response.family == "gaussian"){
    best.fwd <- regsubsets(y.response~., data=x.data ,nvmax=200,method = "forward")
    best.fwd.summary <- summary(best.fwd)
    
    path <- sprintf("Paper/images/forward_nvars_%s.pdf", model.name)
    pdf(path,width=12,height=8)
    par(mfrow=c(2,1))
    plot(best.fwd.summary$cp ,xlab="Number of Variables ",ylab="Cp", type='l')
    min.cp <- which.min(best.fwd.summary$cp)
    points(min.cp,best.fwd.summary$cp[min.cp],col="red",cex=2,pch=20)
    plot(best.fwd.summary$bic ,xlab="Number of Variables ",ylab="BIC", type='l')
    min.bic <- which.min(best.fwd.summary$bic)
    points(min.bic,best.fwd.summary$bic[min.bic],col="red",cex=2,pch=20)
    dev.off()
    
    par(mfrow=c(1,1))
    
    fwd.coefs <- coef(best.fwd, which.min(best.fwd.summary$bic))
    incl_vars <- names(fwd.coefs)[-1]
    incl_x <- x.matrix[,incl_vars]
    fwd.predicts <- incl_x %*% fwd.coefs[-1] + fwd.coefs[1]
    fwd.resids <- (y.response - fwd.predicts)
    
    fwd.coefs10 <- colnames(x.matrix)[best.fwd$vorder[2:6]]
  } else {
    fwd.data <- cbind(x.data,y.response)
    model.null = glm(y.response ~ 1, data=fwd.data, family = binomial(link="logit"))
    
    model.full = glm(y.response ~ ., data=fwd.data, family = binomial(link="logit"))
    
    fwd.steps <- step(model.null,scope = list(upper=model.full), direction="forward", data=fwd.data, trace = 0)
    
  }
  if (response.family == "gaussian"){
    path <- sprintf("Paper/images/resids_%s.pdf", model.name)
    pdf(path,width=12,height=8)
    par(mfrow=c(2,2))
    plot(elastic.predicts, elastic.resids)
    qqnorm(elastic.resids)
    plot(fwd.predicts, fwd.resids)
    qqnorm(fwd.resids)
    dev.off()
    par(mfrow=c(1,1))
  
  
    best.subset <- bs(x.matrix, y.response, k = 5, time.limit = 60)
    
    bs.beta = coef(best.subset)
    bs.coef10 = apply(bs.beta != 0, 2, which)[2:6]
    bs.coef10.names <- colnames(x.matrix)[bs.coef10]
    print("Best subset solution:")
    print(best.subset$status)
  }
  
  top10coef <- cbind(sort(elastic10.names), sort(fwd.coefs10), sort(bs.coef10.names))
  top10coef <- gsub("_", " ", top10coef)
  colnames(top10coef) <- c("elasticnet", "forward", "subset")
  path <- sprintf("Paper/%s_top10names.csv", model.name)
  write.csv(top10coef, path, row.names = FALSE)

  elastic.coef.best.nonzero <- as.data.frame(elastic.coef.best.nonzero)
  elastic.coef.best.nonzero <- rownames_to_column(elastic.coef.best.nonzero)
  fwd.coefs <- as.data.frame(fwd.coefs)
  fwd.coefs <- rownames_to_column(fwd.coefs)
  merged_best_coefs <- full_join(elastic.coef.best.nonzero, fwd.coefs, by="rowname")
  colnames(merged_best_coefs) <- c("variable", "elastic", "forward")
  merged_best_coefs$variable <- gsub("_", " ", merged_best_coefs$variable)
  path <- sprintf("Paper/fullcoeflist_%s.csv", model.name)
  write.csv(merged_best_coefs, path, row.names = FALSE)
  
  results <- list("best.elastic" = best_model, "best.alpha" = best_alpha, "elastic.coef" = elastic.coef.best.nonzero,
                  "elastic.top10" = elastic10.names, "fwd.coefs" = fwd.coefs, "best.subset" = bs.coef10.names)

  return(results)
}




#which(duplicated(production_x, MARGIN = 2))

# for (i in 1:10){
#   lambda.nvar <- elastic.full$lambda[elastic.full$df == 4]
#   elastic.coef <- coef(elastic.full,s=lambda.nvar)[1:ncol(x.matrix),] 
#   elastic.nonzero <- elastic.coef[elastic.coef!=0][2:(2+4-1)]
#   elastic.names <- names(elastic.nonzero)
#   elastic.names
#   #nvars.list <- rbind(nvars.list, elastic.names)
#   print(i)
#   print(elastic.names)
# }
