#import libraries
library(caret)
#import data
data("iris")
df<-iris

#get train data set
training_index <- createDataPartition(df$Species , p = 0.80 , list = F)
#create validation dataset
validation<- df[-training_index,]
#use remaining data for training and testing
df1<-df[training_index,]

#summaries the data
#dimensions of data
dim(df1)

#list of type of each attribute
sapply(df1 , class)

#levels in dataset
levels(df1$Species)

#class distribution
persentage <- prop.table(table(df1$Species))*100
cbind(freq = table(df1$Species) , percentage = persentage)

#statistical summary
summary(df1)

#split input and output
x <- df1[,1:4]
y <- df1[,5]

#box plot 
par(mfrow = c(1,4))
for( i in 1:4){
  boxplot(x[,i],main =names(iris)[i])
}

#barplot
plot(y)

#scatterplot matrix
featurePlot(x=x, y=y, plot="ellipse")

featurePlot(x=x , y=y,plot = "box")

#density plots for each attributes
scales <- list(x=list(relation="free"),y=list(relation="free"))
featurePlot(x=x,y=y,plot = "density",scale = scales)

#run the algorithm using 10 fold cross validation 
control <- trainControl(method = "cv" , number = 10)
metric <- "Accuracy"


#run different algorithms to get best accuracy
#1) linear model
set.seed(10)
fit_lda <- train(Species~. ,data = df1 , method = "lda" ,metric = metric ,trControl = control)
#2) non linear model 
set.seed(10)
fit_cart <-  train(Species~. , data = df1 , method = "rpart",metric = metric , trControl = control)
#3) KNN
set.seed(10)
fit_knn <- train(Species~.,data=df1 , method= "knn", metric = metric , trControl=control)
#4) SVM
set.seed(10)
fit_svm <- train(Species~. , data=df1 , method = "svmRadial" , metric = metric , trControl=control)
#5) random forest
set.seed(10)
fit_ran <-train(Species~. , data=df1 , method = "rf",metric=metric , trControl = control)

#summaries the accuracy on models 
results <- resamples(list(lda=fit_lda ,cart=fit_cart , knn=fit_knn , svm=fit_svm,rf=fit_ran))
summary(results)

#plot the models
dotplot(results)
#print lda
print(fit_lda)
print(fit_cart)
print(fit_knn)
print(fit_svm)

#make predictions 
predictions <- predict(fit_lda , validation)
confusionMatrix(predictions , validation$Species)
