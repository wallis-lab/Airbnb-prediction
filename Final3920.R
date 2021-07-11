install.packages("Hmisc")
install.packages('caret')
install.packages("e1071")
install.packages("ggplot2")
install.packages("ggthemes")
library(ggplot2)
library(ggthemes)
library(caret)
library(Hmisc)
library(randomForest)
library(caret)

MDstatsMelt <-read.csv("AB_NYC_2019.csv") # get the data 
summary(MDstatsMelt) # look

str(MDstatsMelt)# check the NA or 

sum(complete.cases(MDstatsMelt$reviews_per_month)) 
sum(!complete.cases(MDstatsMelt$reviews_per_month)) # number of missing value

data2=na.omit(MDstatsMelt) # delect the missing data


label1=factor(c("0","1","1")) # change the roon type become 0,1
data2$room_type= label1[data2$room_type]

num=length(data2$price)

i=1


for (i in 1:num){
  if (as.numeric(data2[[10]][i])<=50){
    data2[[10]][i]="Level_1"
  }
  else if (as.numeric(data2[[10]][i])>50 & as.numeric(data2[[10]][i])<=100){
    data2[[10]][i]="Level_2"
  }
  else if (as.numeric(data2[[10]][i])>100 & as.numeric(data2[[10]][i])<=200){
    data2[[10]][i]="Level_3"
  }
  else{
    data2[[10]][i]="Level_4"
  }}

data2$price = as.factor(data2$price )

str(data2)




# Scatterplot
pairs(~price+number_of_reviews+reviews_per_month+room_type+neighbourhood_group+availability_365,data = data2,
      main = "Scatterplot")



p <- ggplot(data2, aes(x=price, y=number_of_reviews)) + geom_boxplot()
p
p + coord_flip()

ggplot(data2, aes(x=price, y=number_of_reviews)) + 
  geom_boxplot(notch=TRUE)

ggplot(data2, aes(x=price, y=number_of_reviews)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)

p <- ggplot(data2, aes(x=price, y=reviews_per_month)) + 
  geom_boxplot()
p
p + coord_flip()

ggplot(data2, aes(x=price, y=reviews_per_month)) + 
  geom_boxplot(notch=TRUE)

ggplot(data2, aes(x=price, y=reviews_per_month)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)

#Basic box plot
p <- ggplot(data2, aes(x=price, y=availability_365)) + 
  geom_boxplot()
p
p + coord_flip()# Rotate the box plot
# Notched box plot
ggplot(data2, aes(x=price, y=availability_365)) + 
  geom_boxplot(notch=TRUE)
# Change outlier, color, shape and size
ggplot(data2, aes(x=price, y=availability_365)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
#  Box plot with mean points
p + stat_summary(fun.y=mean, geom="point", shape=23, size=4)

#hist(data2$number_of_reviews~data2$availability_365+data2$reviews_per_month)

hist(data2$availability_365, 
     main="number_of_reviews", 
     xlab="", 
     border="blue", 
     col="green", 
     xlim=c(50,400), 
     las=1, 
     breaks=3, 
     prob = TRUE)

lines(density(data2$availability_365))




data3= data2[c("neighbourhood_group" ,"price","room_type","number_of_reviews","reviews_per_month","availability_365")]

describe(data3)
ind = sample(2,nrow(data3), replace= TRUE, prob= c(0.7,.3))
train = data3[ind==1,]
test = data3[ind==2,]

set.seed(222)
rf=randomForest(price~.,data=train)
print(rf)  # 43.09
#attributes(rf)
install.packages("caret")

plot(rf)


n=length(names(data3))
print(n)
rate=1
for(i in 1:(n)){
  set.seed(1234)
  rf_train<-randomForest(price~.,data=train,mtry=i)
  rate[i]<-mean(rf_train$err.rate)   #
  print(rf_train)    
}
rate
plot(rate)
set.seed(222)
rf = randomForest(price~., data=train, ntree=200,
                  mtry=2, importance =TRUE)
print(rf) # 42.36 before    150 ntree 42.25   200=42.27

hist(treesize(rf), main="No. of nudes for the tree", col="green")

varImpPlot(rf)



p1 = predict(rf,train, type="response")
#as.factor(train$price)
confusionMatrix(p1,train$price) # 0.6661 
p2 = predict(rf,test) 
confusionMatrix(p2,test$price) 


test_matrix=table(p2,test$price)
sum(diag(test_matrix))/sum(test_matrix) # get the accuracy


table(actual=test$price,predicted=predict(rf,newdata = test,type = "class"))

label1=factor(c("0","1","1")) # change the roon type become 0,1
MDstatsMelt$room_type= label1[MDstatsMelt$room_type]

plot(MDstatsMelt$room_type,MDstatsMelt$price)



