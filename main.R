setwd('/Users/jiahe/Desktop/Imbalanced Data/noisy data/39bus and sytehtic data/39 bus')
file_list<- mixedsort(list.files())
total= lapply(file_list, function(x) read.table(x,header=T)) 

for (i in 1:num.line) {
  total[[i]]$outcome=as.factor(total[[i]]$outcome)
  total[[i]]=conv(total[[i]])
  
}


####LR
TMP=list()
rep=20
for(k in 1:rep) {
  tmp=matrix(NA,nrow=length(BL),ncol=5)
  colnames(tmp)=c("Diff","adaCW","N-:N+","1:1","adaboost")
  for(j in 1:length(BL)){
    i=BL[j]
    total[[i]]=conv(total[[i]])
    Data=total[[i]]
    colnames(Data)[dim(Data)[2]]="y"
    
    set.seed(k)
    Sample=c(sample(which(Data$y==1),3),
             sample(which(Data$y==-1),length(which(Data$y==-1))/2))
    Train=Data[Sample,]
    tst=Data[-Sample,]
    
    X1=Train[,-dim(Train)[2]]
    y=Train$y
    
    ###DiffBoost
    model1<-diffboostCW_LR(X1,y,n_rounds = 10)
    pred=Predict.diffboost_LR(model1,tst,type="response")
    table=table(pred=pred,true=tst$y)
    tmp[j,1]=table[2,2]/sum(table[,2])
    
    ###AdaCW
    wt=adaCW_LR(X1,y,n_rounds = 50)$cw  # computed weight
    myweight=ifelse(y=="1",wt[2],wt[1])
    model<-glm(y~.,data=X1,family=binomial,weights=myweight)
    Test=predict(model,tst,type="response")
    Test_r=rep("-1",dim(tst)[1])
    Test_r[Test>.5]="1"
    tmp[j,2]=confusionMatrix(as.factor(Test_r), tst[,dim(tst)[2]], 
                             mode = "prec_recall", positive="1")$byClass[6]
    
    ######N-:N+
    wt=rev(table(y)/length(y))  # empirical weight
    myweight=ifelse(y=="1",wt[2],wt[1])
    model<-glm(y~.,data=X1,family=binomial,weights=myweight)
    Test=predict(model,tst,type="response")
    Test_r=rep("-1",dim(tst)[1])
    Test_r[Test>.5]="1"
    tmp[j,3]=confusionMatrix(as.factor(Test_r), tst[,dim(tst)[2]], 
                             mode = "prec_recall", positive="1")$byClass[6]
    
    ##1:1
    model<-glm(y~.,data=X1,family=binomial)
    Test=predict(model,tst,type="response")
    Test_r=rep("-1",dim(tst)[1])
    Test_r[Test>.5]="1"
    tmp[j,4]=confusionMatrix(as.factor(Test_r), tst[,dim(tst)[2]], 
                             mode = "prec_recall", positive="1")$byClass[6]
    
    ###adaboost
    model<-adaboost_LR(X1,y,n_rounds = 5)
    Test=Predict.diffboost_LR(model,tst,type="response")
    tmp[j,5]= confusionMatrix(as.factor(Test), tst[,dim(tst)[2]], 
                              mode = "prec_recall", positive="1")$byClass[6]
    
  } 
  
  TMP[[k]]=tmp
}

colMeans(Reduce("+",TMP))/rep 


####RF
TMP=list()
rep=20
for(k in 1:rep) {
  tmp=matrix(NA,nrow=length(BL),ncol=5)
  colnames(tmp)=c("Diff","adaCW","N-:N+","1:1","adaboost")
  for(j in 1:length(BL)){
    i=BL[j]
    total[[i]]=conv(total[[i]])
    Data=total[[i]]
    colnames(Data)[dim(Data)[2]]="y"
    
    set.seed(k)
    Sample=c(sample(which(Data$y==1),3),
             sample(which(Data$y==-1),length(which(Data$y==-1))/2))
    Train=Data[Sample,]
    tst=Data[-Sample,]
    
    X1=Train[,-dim(Train)[2]]
    y=Train$y
  
  ###DiffBoost
  model1<-diffboostCW_RF(X1,y,n_rounds = 50)
  Test=Predict.diffboost_RF(model1,tst,type="response")
  tmp[i,1]=confusionMatrix(as.factor(Test), tst[,dim(tst)[2]], 
                           mode = "prec_recall", positive="1")$byClass[6]
  
  ###AdaCW
  wt=adaCW_RF(X1,y,n_rounds = 50)$cw  # computed weight
  model=randomForest(y~.,data=X1, classwt =wt,ntree=5)
  Test=predict(model,tst) 
  tmp[i,2]=confusionMatrix(Test, tst[,dim(tst)[2]], 
                           mode = "prec_recall", positive="1")$byClass[6]
  
  ######N-:N+
  wt=rev(table(y)/length(y))  # empirical weight
  model=randomForest(y~.,data=X1,classwt =wt,ntree=5)
  Test=predict(model,tst)
  tmp[i,3]=confusionMatrix(Test, tst[,dim(tst)[2]], 
                           mode = "prec_recall", positive="1")$byClass[6]
  
  ##1:1
  model=randomForest(y~.,data=X1,ntree=5)
  Test=predict(model,tst)
  tmp[i,4]=confusionMatrix(Test, tst[,dim(tst)[2]], 
                           mode = "prec_recall", positive="1")$byClass[6]
  
  } 
  TMP[[k]]=tmp
}



###SVM
BL=c(7,8,15,23,30,39)
TMP=list()
for(k in 1:rep) {
  tmp=matrix(NA,nrow=length(BL),ncol=4)
  colnames(tmp)=c("Diff","adaCW","N-:N+","1:1")
  for(j in 1:length(BL)){
    i=BL[j]
    total[[i]]=conv(total[[i]])
    Data=total[[i]]
    colnames(Data)[dim(Data)[2]]="y"
    
    set.seed(k+120)
    Sample=c(sample(which(Data$y==1),7),
             sample(which(Data$y==-1),length(which(Data$y==-1))/2))
    Train=Data[Sample,]
    tst=Data[-Sample,]
    
    X1=Train[,-dim(Train)[2]]
    y=Train$y
    
    ###DiffBoost
    model1<-diffboostCW_SVM(X1,y,n_rounds = 10)
    Test=Predict.diffboost_SVM(model1,tst,type="response")
    tmp[j,1]=confusionMatrix(as.factor(Test), tst[,dim(tst)[2]], 
                             mode = "prec_recall", positive="1")$byClass[6]
    
    ###AdaCW
    wts=adaCW_SVM(X1,y,n_rounds=10)$cw
    svmfit=svm(y~.,data=X1,class.weights=wts)
    Test=predict(svmfit,tst)
    tmp[j,2]=confusionMatrix(Test, tst[,dim(tst)[2]], 
                             mode = "prec_recall", positive="1")$byClass[6]
    
    ######N-:N+
    wts=rev(table(y)/length(y))  
    names(wts)=levels(y)
    svmfit=svm(y~.,data=X1,class.weights=wts)
    Test=predict(svmfit,tst)
    tmp[j,3]=confusionMatrix(Test, tst[,dim(tst)[2]], 
                             mode = "prec_recall", positive="1")$byClass[6]
    
    ##1:1
    svmfit=svm(y~.,data=X1)
    Test=predict(svmfit,tst)
    tmp[j,4]=confusionMatrix(Test, tst[,dim(tst)[2]], 
                             mode = "prec_recall", positive="1")$byClass[6]
    
  } 
  
  TMP[[k]]=tmp
}
colMeans(Reduce("+",TMP))/rep 
  




  