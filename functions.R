conv=function(x) {
  x[,dim(x)[2]]= as.factor(ifelse(x[,dim(x)[2]]==HL,"1","-1"))
  return(x)
}
myfunLR=function(X1,y,NL){
  LRuw=adaCW_RF_LR(X1,y,stopping = NL,n_rounds=50)
  ratio=c(LRuw$final_ratio,LRw$final_ratio)
  return(ratio)
}  #compute weight

myfunRF=function(X1,y,NL){
  
  RFuw=adaCW_RF(X1,y,stopping = NL,n_rounds=50)
  RFw=ada_weight(X1,y,stopping = NL,n_rounds=50)
  ratio=c(RFuw$final_ratio,RFw$final_ratio)
  return(ratio)
}
adaCW_RF = function(X, y, n_rounds = 30, stopping=0.001 ,verbose = FALSE){
  
  # check data types
  if(!all(y %in% c(-1,1)))
    stop("y must take values in -1, 1")
  N_pos=table(y)[2]
  N_neg=table(y)[1] 
  w_pos =w_neg=1  #initialize w1=w2=1
  class_w=c(rep(w_pos,N_pos),rep(w_neg,N_neg))
  
  for(i in seq(n_rounds)){
    #classifier
    r<-randomForest(y ~ ., data = data.frame(X), weights = c(w_neg,w_pos),importance=FALSE,do.trace=F,ntree=10)
    
    pred = as.integer(as.character(stats::predict(r, data.frame(X), type="class")))
    
    table(pred=pred,true=y)   
    
    #unweighted normalized class error
    error =pred != as.numeric(as.character(y))
    e_pos=sum(error[which(y=="1")])/N_pos
    e_neg=sum(error[which(y=="-1")])/N_neg
    #  
    if( e_pos < stopping|| e_pos < e_neg ){
      break
    }
    
    #updata class weight
    w_pos=w_pos*exp(e_pos)
    w_neg=w_neg*exp(e_neg)
    
    #assign weight to indi
    for(j in 1:length(class_w)) {
      if(y[j]==1) {
        class_w[j]=w_pos
      }  else {
        class_w[j]=w_neg
      }
    }
    
  }
  
  #normalize class weight for WLR use
  
  temp=w_pos+w_neg
  before_normalize=c(w_pos,w_neg)
  w_pos=w_pos/temp
  w_neg=w_neg/temp
  
  out = list(iter=i,final_ratio=w_pos/w_neg, cw=c(w_neg,w_pos),model=r,
             pos_error=e_pos)
  class(out) = "adaCW_RF"
  out
}
ada_weight = function(X, y, n_rounds = 30,stopping=0.001, verbose = FALSE){
  
  # check data types
  if(!all(y %in% c(-1,1)))
    stop("y must take values in -1, 1")
  N_pos=table(y)[2]
  N_neg=table(y)[1] 
  w_pos =w_neg=1  #initialize w1=w2=1
  n = dim(X)[1]
  indi_w=rep(1/n, n)
  
  for(i in seq(n_rounds)){
    #classifier
    #classifier
    r<-randomForest(y ~ ., data = data.frame(X), weights = c(w_neg,w_pos),importance=TRUE,do.trace=F,ntree=10)
    
    pred = as.integer(as.character(stats::predict(r, data.frame(X), type="class")))
    
    table(pred=pred,true=y)   
    
    #unweighted rare class error
    error =pred != as.numeric(as.character(y))
    E_pos=sum(error[which(y=="1")])/N_pos
    E_neg=sum(error[which(y=="-1")])/N_neg
    #
    if(E_pos < stopping ||  E_pos < E_neg  ){
      break
    }
    #weighted class error
    w_error = indi_w*(pred != as.numeric(as.character(y)))
    e_pos=sum(w_error[which(y=="1")])/N_pos
    e_neg=sum(w_error[which(y=="-1")])/N_neg
    
    #updata individual weights
    indi_w = indi_w*exp(-pred*as.numeric(as.character(y)))
    indi_w = indi_w/sum(indi_w)
    
    #updata class weight
    w_pos=w_pos*exp(e_pos)
    w_neg=w_neg*exp(e_neg)
    
  }
  
  #normalize class weight for WLR use
  
  temp=w_pos+w_neg
  w_pos=w_pos/temp
  w_neg=w_neg/temp
  
  out = list(iter=i,final_ratio=w_pos/w_neg, cw=c(w_neg,w_pos), model=r,
             pos_error=E_pos)
  class(out) = "ada_weighted"
  out
}
adaCW_LR = function(X, y, n_rounds = 30,stopping=1e-3 , verbose = FALSE){
  
  # check data types
  if(!all(y %in% c(-1,1)))
    stop("y must take values in -1, 1")
  N_pos=table(y)[2]
  N_neg=table(y)[1] 
  w_pos =w_neg=1  #initialize w1=w2=1
  class_w=c(rep(w_pos,N_pos),rep(w_neg,N_neg))
  
  for(i in seq(n_rounds)){
    #classifier
    
    fit<-glm(y ~ ., data = data.frame(X),family=binomial,weights=class_w)
    probs=predict(fit,data.frame(X),type="response")
    pred=rep("-1",dim(X)[1])
    pred[probs>.5]="1"
    
    #unweighted normalized class error
    error =pred != as.numeric(as.character(y))
    e_pos=sum(error[which(y=="1")])/N_pos
    e_neg=sum(error[which(y=="-1")])/N_neg
    #   ||e_pos < e_neg
    if( e_pos < stopping  ){
      break
    }
    #updata class weight
    w_pos=w_pos*exp(e_pos)
    w_neg=w_neg*exp(e_neg)
    
    #assign weight to indi
    for(j in 1:length(class_w)) {
      if(y[j]==1) {
        class_w[j]=w_pos
      }  else {
        class_w[j]=w_neg
      }
    }
    table(pred=pred,true=y)  
    
  }
  #normalize class weight for WLR use
  
  temp=w_pos+w_neg
  w_pos=w_pos/temp
  w_neg=w_neg/temp
  
  out = list(iter=i,final_ratio=w_pos/w_neg, cw=c(w_neg,w_pos),model=fit,wt=table(class_w),
             error=e_pos)
  class(out) = "adaCW_LR"
  out
}
adaCW_SVM = function(X, y, n_rounds = 30,stopping=0.001 ,verbose = FALSE){
  
  # check data types
  if(!all(y %in% c(-1,1)))
    stop("y must take values in -1, 1")
  N_pos=table(y)[2]
  N_neg=table(y)[1] 
  w_pos =w_neg=1  #initialize w1=w2=1
  class_w=c(rep(w_pos,N_pos),rep(w_neg,N_neg))
  wts=c(w_neg,w_pos)
  names(wts)=levels(y)
  for(i in seq(n_rounds)){
    #classifier svm
    svmfit=svm(y~.,data=data.frame(X,y),kernel="radial",scale=F,class.weights=wts)
    pred=predict(svmfit,data.frame(X,y))
    table(pred=pred,true=y)   
    
    #unweighted normalized class error
    error =pred != as.numeric(as.character(y))
    e_pos=sum(error[which(y=="1")])/N_pos
    e_neg=sum(error[which(y=="-1")])/N_neg
    
    #updata class weight
    w_pos=w_pos*exp(e_pos)
    w_neg=w_neg*exp(e_neg)
    wts[1]=w_neg
    wts[2]=w_pos
    
    #assign weight to indi
    for(j in 1:length(class_w)) {
      if(y[j]==1) {
        class_w[j]=w_pos
      }  else {
        class_w[j]=w_neg
      }
    }
    if(abs(e_pos) <  stopping){
      break
    }
    
  }
  
  #normalize class weight for WLR use
  
  temp=w_pos+w_neg
  before_normalize=c(w_pos,w_neg)
  w_pos=w_pos/temp
  w_neg=w_neg/temp
  
  out = list(iter=i,final_ratio=w_pos/w_neg, cw=c(w_neg,w_pos),
             pos_error=e_pos)
  class(out) = "adaCW_SVM"
  out
}

#########diffboost LR as weak classifier
{
  
  diffboostCW_LR=function (X, y,  n_rounds = 100, verbose = FALSE, 
                           control = NULL) 
  {
    if (!all(y %in% c(-1, 1))) 
      stop("y must take values in -1, 1")
    if (!is.data.frame(X)) 
      stop("X must be a data frame")
    
    
    #initialize class weights
    w_pos=w_neg=1
    class_w=c(rep(w_pos,table(y)[2]),rep(w_neg,table(y)[1]))
    
    #initialize individual weights
    D_pos=rep(1/table(y)[2],table(y)[2])
    D_neg=rep(1/table(y)[1],table(y)[1])
    
    #index of positive and negative example
    P_index=which(y==1)
    N_index=which(y!=1)
    
    trees = list()
    alphas= list()
    E_POS=list()
    F1=list()
    for (i in seq(n_rounds)) {
      
      model<-glm(y~.,data=X,family=binomial(link="logit"),weights=class_w)
      Pred=predict(model,X,type="response")  #prob
      pred=rep("-1",dim(X)[1])
      pred[Pred>.5]="1"
      conf=confusionMatrix(as.factor(pred), y,  mode = "prec_recall", positive="1")
      f1=conf$byClass[7]
      F1[[i]]=f1
      e_pos=sum(D_pos*(pred[P_index]!=y[P_index]))
      e_neg=sum(D_neg*(pred[N_index]!=y[N_index]))
      
      {  # if (e_pos <1e-08 ) {
        #   if (i == 1) {
        #  trees[[i]] = model
        #   alphas[[i]]= 1
        #   E_POS[[i]]=e_pos
        #   terms = model$terms
        #   break
        # }
        #   break
        # }
      } 
      alpha = max(1/2 *log((1 - e_pos)/e_pos),0)
      if (alpha==Inf) {
        alpha=1
      }
      D_pos=D_pos*exp(-alpha*as.integer(pred[P_index])*as.integer(as.character(y[P_index])))
      D_neg=D_neg*exp(-alpha*as.integer(pred[N_index])*as.integer(as.character(y[N_index])))
      D_pos=D_pos/sum(D_pos)
      D_neg=D_neg/sum(D_neg)
      
      #updata class weight
      w_pos=w_pos*exp(e_pos)
      w_neg=w_neg*exp(e_neg)
      
      #assign weight to indi
      for(j in 1:length(class_w)) {
        #   if(y[j]==1 || (y[j]==-1  && y[j]!=pred[j] )) {  assign FP example weight same as FN, not working
        if(y[j]==1 ) {
          class_w[j]=w_pos
        }  else {
          class_w[j]=w_neg
        }
      }
      #   print(conf)
      c(e_pos,e_neg,alpha,w_pos,w_neg)
      
      if (i == 1) {
        terms = model$terms
      }
      else {
        model$terms = NULL
      }
      trees[[i]] = model
      alphas[[i]] = alpha
      E_POS[[i]]=e_pos
      if (verbose & (i%%10 == 0)) 
        cat("Iteration: ", i, "\n")
    }
    
    out = list(alphas = unlist(alphas),e_p=unlist(E_POS),trees = trees,CW=c(w_pos,w_neg) , terms = terms)
    class(out) = "adaboost"
    Predict = Predict.diffboost_LR(out, X,y,type="response")
    out$confusion_matrix=table(Predict,y)
    out
  }
  
  Predict.diffboost_LR= function (object, X, y,type = c("response", "prob"), n_tree = NULL, 
                                  ...) 
  {
    type = match.arg(type)
    if (is.null(n_tree)) {
      tree_seq = seq_along(object$alphas)
    }
    else {
      if (n_tree > length(object$alphas)) 
        stop("n_tree must be less than the number of trees used in fit")
      tree_seq = seq(1, n_tree)
    }
    f=0
    
    for (i in 1:length(object$trees)) {
      tree = object$trees[[i]]
      tree$terms = object$terms
      Pred= predict(tree, as.data.frame(X),type = "response")
      pred=rep("-1",dim(X)[1])
      pred[Pred>.5]="1"
      f= f + object$alphas[i] * as.integer(pred)
      
    }
    if (type == "response") {
      sign(f)
    }
    else if (type == "prob") {
      1/(1 + exp(-2 * f))
    }
  }
  
}


#########adaboost-C RF as weak classifier
{
  diffboostCW_RF=function (X, y, tree_depth = 5, n_rounds = 100, verbose = FALSE, 
                           control = NULL) 
  {
    if (!all(y %in% c(-1, 1))) 
      stop("y must take values in -1, 1")
    if (!is.data.frame(X)) 
      stop("X must be a data frame")
    
    
    #initialize class weights
    w_pos=w_neg=1
    class_w=c(rep(w_pos,table(y)[2]),rep(w_neg,table(y)[1]))
    
    #initialize individual weights
    D_pos=rep(1/table(y)[2],table(y)[2])
    D_neg=rep(1/table(y)[1],table(y)[1])
    
    #index of positive and negative example
    P_index=which(y==1)
    N_index=which(y!=1)
    
    trees = list()
    alphas= list()
    E_POS=list()
    for (i in seq(n_rounds)) {
      set.seed(1)
      model<-randomForest(y ~ ., data = data.frame(X), classwt= c(w_neg,w_pos),importance=FALSE,do.trace=F,ntree=50)
      
      pred = as.integer(as.character(stats::predict(model, data.frame(X), type="class")))
      
      e_pos=sum(D_pos*(pred[P_index]!=y[P_index]))
      e_neg=sum(D_neg*(pred[N_index]!=y[N_index]))
      
      # if (e_pos <1e-08 ) {
      #   if (i == 1) {
      #  trees[[i]] = model
      #   alphas[[i]]= 1
      #   E_POS[[i]]=e_pos
      #   terms = model$terms
      #   break
      # }
      #   break
      # }
      
      alpha = max(1/2 *log((1 - e_pos)/e_pos),0)
      if (alpha==Inf) {
        alpha=1
      }
      
      D_pos=D_pos*exp(-alpha*pred[P_index]*as.integer(as.character(y[P_index])))
      D_neg=D_neg*exp(-alpha*pred[N_index]*as.integer(as.character(y[N_index])))
      D_pos=D_pos/sum(D_pos)
      D_neg=D_neg/sum(D_neg)
      
      #updata class weight
      w_pos=w_pos*exp(e_pos)
      w_neg=w_neg*exp(e_neg)
      
      #assign weight to indi
      table(pred,y)
      c(e_pos,e_neg,alpha,w_pos,w_neg)
      
      if (i == 1) {
        terms = model$terms
      }
      else {
        model$terms = NULL
      }
      trees[[i]] = model
      alphas[[i]] = alpha
      E_POS[[i]]=e_pos
      if (verbose & (i%%10 == 0)) 
        cat("Iteration: ", i, "\n")
    }
    
    out = list(alphas= unlist(alphas),e_p=unlist(E_POS),trees = trees,CW=c(w_pos,w_neg) , terms = terms)
    class(out) = "adaboost"
    Predict = Predict.diffboost_RF(out, X,y,type="response")
    out$confusion_matrix=table(Predict,y)
    out
  }
  
  Predict.diffboost_RF= function (object, X,y, type = c("response", "prob"), n_tree = NULL)
  {
    type = match.arg(type)
    if (is.null(n_tree)) {
      tree_seq = seq_along(object$alphas)
    }
    else {
      if (n_tree > length(object$alphas)) 
        stop("n_tree must be less than the number of trees used in fit")
      tree_seq = seq(1, n_tree)
    }
    f= 0
    
    for (i in 1:length(object$trees)) {
      tree = object$trees[[i]]
      tree$terms = object$terms
      pred= as.integer(as.character(stats::predict(tree, data.frame(X), type = "class")))
      f = f + object$alphas[i] * pred
      
    }
    
    if (type == "response") {
      sign(f)
    }
    else if (type == "prob") {
      1/(1 + exp(-2 * f))
    }
  }
}


#########adaboost-C SVM as weak classifier
diffboostCW_SVM=function (X, y, n_rounds = 100,KERNEL="radial",DEGREE=2,verbose=FALSE)
{
  if (!all(y %in% c(-1, 1))) 
    stop("y must take values in -1, 1")
  if (!is.data.frame(X)) 
    stop("X must be a data frame")
  
  
  #initialize class weights
  w_pos=w_neg=1
  class_w=c(rep(w_pos,table(y)[2]),rep(w_neg,table(y)[1]))
  wts=c(w_neg,w_pos)
  names(wts)=levels(y)
  
  #initialize individual weights
  D_pos=rep(1/table(y)[2],table(y)[2])
  D_neg=rep(1/table(y)[1],table(y)[1])
  
  #index of positive and negative example
  P_index=which(y==1)
  N_index=which(y!=1)
  
  trees = list()
  alphas= list()
  E_POS=list()
  for (i in seq(n_rounds)) {
    svmfit=svm(y~.,data=data.frame(X,y),kernel=KERNEL,degree=DEGREE,scale=F,class.weights=wts)
    pred=predict(svmfit,data.frame(X,y))
    
    e_pos=sum(D_pos*(pred[P_index]!=y[P_index]))
    e_neg=sum(D_neg*(pred[N_index]!=y[N_index]))
    
    # if (e_pos <1e-08 ) {
    #   if (i == 1) {
    #  trees[[i]] = model
    #   alphas[[i]]= 1
    #   E_POS[[i]]=e_pos
    #   terms = model$terms
    #   break
    # }
    #   break
    # }
    
    alpha = max(1/2 *log((1 - e_pos)/e_pos),0)
    if (alpha==Inf) {
      alpha=1
    }
    
    D_pos=D_pos*exp(-alpha*as.integer(as.character(pred[P_index]))*as.integer(as.character(y[P_index])))
    D_neg=D_neg*exp(-alpha*as.integer(as.character(pred[N_index]))*as.integer(as.character(y[N_index])))
    D_pos=D_pos/sum(D_pos)
    D_neg=D_neg/sum(D_neg)
    
    #updata class weight
    w_pos=w_pos*exp(e_pos)
    w_neg=w_neg*exp(e_neg)
    wts[1]=w_neg
    wts[2]=w_pos
    
    #assign weight to indi
    table(pred,y)
    c(e_pos,e_neg,alpha,w_pos,w_neg)
    
    if (i == 1) {
      terms = svmfit$terms
    }
    else {
      svmfit$terms = NULL
    }
    trees[[i]] = svmfit
    alphas[[i]] = alpha
    E_POS[[i]]=e_pos
    if (verbose & (i%%10 == 0)) 
      cat("Iteration: ", i, "\n")
  }
  
  out = list(alphas= unlist(alphas),e_p=unlist(E_POS),trees = trees,CW=wts , terms = terms)
  class(out) = "adaboost"
  Predict = Predict.diffboost_SVM(out, X,y,type="response")
  out$confusion_matrix=table(Predict,y)
  out
}

Predict.diffboost_SVM= function (object, X,y, type = c("value","response", "prob"), n_tree = NULL)
{
  type = match.arg(type)
  if (is.null(n_tree)) {
    tree_seq = seq_along(object$alphas)
  }
  else {
    if (n_tree > length(object$alphas)) 
      stop("n_tree must be less than the number of trees used in fit")
    tree_seq = seq(1, n_tree)
  }
  f= 0
  
  for (i in 1:length(object$trees)) {
    tree = object$trees[[i]]
    tree$terms = object$terms
    pred= as.integer(as.character(stats::predict(tree, data.frame(X), type = "class")))
    f = f + object$alphas[i] * pred
    
  }
  
  if (type == "response") {
    sign(f)
  }
  else if (type == "value") {
    f
  }
  else if (type == "prob") {
    1/(1 + exp(-2 * f))
  }
}
