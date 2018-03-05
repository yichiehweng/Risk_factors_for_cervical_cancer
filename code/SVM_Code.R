#Install SVM package
install.packages("e1071")
require(e1071)
#Import data
df<-read.csv("/home/yichiehweng/python/machineLearning/SVM/risk_factors_cervical_cancer/data/data.csv")
df[df == "?"] <- NA
df <- subset(df, select = -c(27,28) )
f=function(x){
  x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
  x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
  x #display the column
}
df=data.frame(apply(df,2,f))
df[["Biopsy"]]<-factor(df[["Biopsy"]])
#Separate train and test data set
smp.size = floor(0.8*nrow(df)) 
set.seed(516)                     
train.ind = sample(seq_len(nrow(df)), smp.size)
train = df[train.ind, ] # 80%
test = df[-train.ind, ] # 20%
#SVM Model training
model = svm(formula = Biopsy ~ .,  data = train)
summary(model)
# Perdiction
train.pred = predict(model, train)
test.pred = predict(model, test)
# Model Performace of train data
table(real=train$Biopsy, predict=train.pred)
confus.matrix = table(real=train$Biopsy, predict=train.pred)
sum(diag(confus.matrix))/sum(confus.matrix)
#Model Performace of testing data
table(real=test$Biopsy, predict=test.pred)
confus.matrix = table(real=test$Biopsy, predict=test.pred)
sum(diag(confus.matrix))/sum(confus.matrix)