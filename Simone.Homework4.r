#Read data into R
Auto = read.csv(url("http://faculty.marshall.usc.edu/gareth-james/ISL/Auto.csv"), header=T, na.strings = "?")
Auto = na.omit(Auto)
attach(Auto)
###11a
#Use if/else statement to determine if data gets a 1 or 0
mpg01 <- ifelse(mpg > median(mpg), 1, 0)
df = data.frame(mpg01, Auto)

###11b
plot(df)

###11c
set.seed(1)
#Split into training and Test Group
train=sample(1:nrow(df),nrow(df)/2)
test=(-train)
mpg.test=mpg[test]
Train = df[train,]
Test = df[test,]
attach(Train)

###11d
library(MASS)
library(ISLR)
lda.fit=lda(mpg01 ~ cylinders + displacement + horsepower + weight + year, data = Train)
lda.fit

#Test Error
lda.pred = predict(lda.fit, Test)
lda.class=lda.pred$class
table(lda.class, Test$mpg01)
1 - mean(lda.class == Test$mpg01)

###11e
qda.fit=qda(mpg01 ~ cylinders + displacement + horsepower + weight + year, data = Train)
qda.fit

qda.pred = predict(qda.fit, Test)
qda.class=qda.pred$class
table(qda.class, Test$mpg01)
1 - mean(qda.class == Test$mpg01)


###11f
logit.fit=glm(mpg01 ~ cylinders + displacement + horsepower + weight + year, data = Train, family = binomial)
summary(logit.fit)

probs = predict(logit.fit, Test, type = "response")
pred = rep("0", 196)
pred[probs>0.5] = "1"
table(pred, Test$mpg01)
mean(pred!=Test$mpg01)

###11g
library(class)
attach(df)
train.X = cbind(df$cylinders,df$displacement,df$horsepower,df$weight,df$year)[train,]
test.X = cbind(df$cylinders,df$displacement,df$horsepower,df$weight,df$year)[test,]
test.mpg = mpg01[test]
train.mpg = mpg01[train]

#K = 1
knn.pred=knn(train.X, test.X, train.mpg, k=1)
table(knn.pred, test.mpg)
knn.table <- table(knn.pred, test.mpg)
sum = knn.table[1,1]+knn.table[2,1]+knn.table[1,2]+knn.table[2,2]
1 - ((knn.table[1,1]+knn.table[2,2])/sum)

#K = 4
knn.pred4=knn(train.X,test.X,train.mpg,k=4)
table(knn.pred4, test.mpg)
knn.table <- table(knn.pred4, test.mpg)
sum = knn.table[1,1]+knn.table[2,1]+knn.table[1,2]+knn.table[2,2]
1 - ((knn.table[1,1]+knn.table[2,2])/sum)

#K = 7
knn.pred7=knn(train.X,test.X,train.mpg,k=7)
table(knn.pred7, test.mpg)
knn.table <- table(knn.pred7, test.mpg)
sum = knn.table[1,1]+knn.table[2,1]+knn.table[1,2]+knn.table[2,2]
1 - ((knn.table[1,1]+knn.table[2,2])/sum)  
