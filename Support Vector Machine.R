install.packages("e1071")
install.packages(caTools)
library(e1071) #svm function
library(caTools) #split function

# Performing SVM model on the whole data
svm_model = svm(Species ~ ., data=iris, kernel="linear") #change kernel to radial, polynomial,...
plot(svm_model, data=iris, Petal.Width~Petal.Length, slice = list(Sepal.Width=3, Sepal.Length=4))
#Note: Since we're visualizing the effect of the petal length & petal width (as predictor variables) on the response, the slice list function is used for keeping the other variable i.e. sepal length & sepal width constant at the specified values.
pred = predict(svm_model,iris)
tab = table(Predicted=pred, Actual = iris$Species)
tab
acc=sum(diag(tab)/sum(tab)) #accuracy of the model
acc

#Performing the model on Split data
#Splitting the data into training set & test set
set.seed(123) #OPTIONAL .. setting a seed using set.seed(). Seeds allow you to create a starting point for randomly generated numbers, so that each time your code is run the same answer is generated.
#The advantage of doing this in your sampling is that you or anyone else can recreate the exact same training and test sets by using the same seed. 
split = sample.split(iris$Species, SplitRatio = 0.75) #common split ratios : 0.7:0.8
training_set = subset(iris, split == TRUE)
test_set = subset(iris, split == FALSE)
View(training_set)
View(test_set)

# Fitting SVM to the Training set
svm = svm(Species ~ ., data = training_set, kernel = 'polynomial') #change kernel to radial,linear,..
svm #to view the characteristics of the svm model

# Predicting the Test set results
y_pred = predict(svm, test_set)
tab = table(Predicted=y_pred, Actual =test_set$Species)
tab

plot(svm, Petal.Width ~ Petal.Length, data = training_set,slice = list(Sepal.Width=3, Sepal.Length=4))
plot(svm, Petal.Width ~ Petal.Length, data = test_set,slice = list(Sepal.Width=3, Sepal.Length=4))

acc=sum(diag(tab)/sum(tab))
acc

##Other way to make training and test set
train=sample(150,100)  #100 random numbers from 1:150 to use it as index
sv=svm(Species~.,data=d[train,]) #SVM Radial basis default
pr=predict(sv,d[-train,1:4]) #prediction on test set
cm=table(d[-train,5],pr)
acc=sum(diag(cm))/sum(cm)
acc 
err=1-acc

##Some notes :
#in some data you may need to scale the data ( Normalize with mean=0 and SD=1 )
d=iris
d[,-5]=scale(d[,-5])
#and if the classification culomn need to verify as levels
d$Species=factor(d$Species)

#Adding a new data point and predicting its class
NewFlower=data.frame(5.4,3.7,5.2,2,"Unknown")
names(NewFlower)=c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width","Species")
rbind(iris,NewFlower) #Adding a new data point to an existing data frame
predict(svm, NewFlower) #predicting the class of the new data point
tab=table(Predicted=pred1, Actual=NewFlower$Species)
tab