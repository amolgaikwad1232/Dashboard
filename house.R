# Load required libraries
library(tseries)
library(stats)
library(forecast)

#### Question 1

data = read.csv(file.choose(),header = TRUE)
View(data)
class(data)
data = na.omit(data)
head(data)



head(data)
fit=glm(data$property_type~., data=data, family='binomial')
coef(fit)
set.seed(32)
n=nrow(data)
indexes = sample(n,n*(80/100)) # the ratio of trainset is 80% and testset is 20%
trainset = data[indexes,]
testset = data[-indexes,]
##binomial is used for logistic regression
model=glm( data$property_type~., data=data, family='binomial')

pred=predict(fit,testset,type='response')
print(pred)

yhat=rep(0,length(pred))
yhat[pred>0.5]=1

actual=testset$Exited
accuarcy=mean(yhat==actual)
print(accuarcy)
T=table(actual,yhat)
#compute recall
Recall=T[2,2]/(T[2,2]+T[2,1])
print(Recall)
#compute Precision
Precision=T[2,2]/(T[2,2]+T[1,2])
print(Precision)
print(T)

