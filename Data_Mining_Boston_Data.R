#load dataset
install.packages("MASS")
library(MASS)
install.packages("leaps")
library(leaps)
data(Boston)
View(Boston)

#creating a new dataframe 
Boston_df <- data.frame(Boston)

#sample of 75% for training
set.seed(12434842)
b_samp <-sample(x= nrow(Boston_df), size =nrow(Boston_df)*0.75)
Boston_train <- Boston_df[b_samp,] #train
Boston_test <- Boston_df[-b_samp,] #test

#Generalized Linear Model####
glmboston <- glm(medv~., data = Boston_train) #all variables included
summary(glmboston)
#all variables excluding crim, indus and age are significant

#variable selection
nullmodel <- glm(medv~1,data = Boston_train)
fullmodel <- glm(medv~.,data = Boston_train)
bestmod <- step(nullmodel,
                scope = list(lower = nullmodel,upper =fullmodel,
                             direction = "both"))
#Removing crim will increase AIC- so retained in the model
#model will have all variables except indus and age

#Best Model
glmbostonbest <- glm(medv~lstat + rm + ptratio + dis + nox + chas + zn + black +
                       rad + tax + crim, data = Boston_train)
bostonbest <- summary(glmbostonbest)

#In sample prediction
inpred_boston <- predict(glmbostonbest)
#Avg SSE
insamp_glm_sse <- mean((inpred_boston-Boston_train$medv)^2)
#Avg SSE- 20.16

#Out of Sample prediction
outpred_boston <- predict(object = glmbostonbest, newdata = Boston_test)
#Avg SSE
outsamp_glm_sse <- mean((outpred_boston-Boston_test$medv)^2)
#Avg SSE- 27.62

#Regression Tree ####

#Tree with default values
install.packages("rpart")
library(rpart)
boston_train_tree <- rpart(formula = medv ~., data = Boston_train, cp=0.001) #default cp value
summary(boston_train_tree)

#draw the tree
plot(boston_train_tree)
text(boston_train_tree) #congested tree- overfitting

#pruning- build a generic tree
plotcp(boston_train_tree) #found cp = 0.0066, nodes = 10

#revised tree
boston_prune_tree <-prune.rpart(boston_train_tree, cp=0.0066)
summary(boston_prune_tree)
#draw the revised tree
plot(boston_prune_tree)
text(boston_prune_tree) #tree with 10 nodes

#in sample prediction
intree_pred <- predict(boston_prune_tree)
#Avg SSE
intree_pred_sse <- mean((intree_pred-Boston_train$medv)^2)
#Avg SSE- 12.86

#out of sample prediction
outtree_pred <- predict(object = boston_prune_tree, newdata = Boston_test)
#Avg SSE
outtree_pred_sse <-mean((outtree_pred - Boston_test$medv)^2)
#Avg SSE- 30.81


#Generalized Additive Model
install.packages("mgcv")
library(mgcv)
bostonadditive <- gam(medv~ s(crim)+s(zn)+s(indus)+chas+s(nox)+s(rm)
                      +s(age)+s(dis)+rad+s(tax)+s(ptratio)+
                        s(black)+s(lstat), data = Boston_train)
summary(bostonadditive)
#apart from 'chas' and 'rad', spline terms are added for all variables. 
#why not chas and rad ?- not sure - theses variables are discrete int while others
#are continuous numbers- may be becasue of that ?- not sure !

bostonadditive1 <- gam(medv~ s(crim)+zn+s(indus)+chas+s(nox)+s(rm)
                       +age+s(dis)+rad+s(tax)+s(ptratio)+
                         s(black)+s(lstat), data = Boston_train)
summary(bostonadditive1)

#bostonadditive2 <- gam(medv~ s(crim)+s(indus)+chas+s(nox)+s(rm)
#+age+s(dis)+rad+s(tax)+s(ptratio)+
# black+s(lstat), data = Boston_train)
#summary(bostonadditive2)

#bostonadditive3 <- gam(medv~ s(crim)+s(indus)+s(nox)+s(rm)
# +age+s(dis)+rad+s(tax)+s(ptratio)+
# black+s(lstat), data = Boston_train)
#summary(bostonadditive3)

#bostonadditive4 <- gam(medv~ s(crim)+s(indus)+s(nox)+s(rm)
#     +s(dis)+rad+s(tax)+s(ptratio)+
#    black+s(lstat), data = Boston_train)
#summary(bostonadditive4)



#in sample prediction
insamp_bostonadditive <- predict(bostonadditive1)
insamp_bostonadditive_sse <- mean((insamp_bostonadditive- Boston_train[,"medv"])^2)
#in sample Avg SSE- 6.78

#out of sample prediction
outsamp_bostonadditive <- predict(object=bostonadditive1,newdata = Boston_test)
outsamp_bostonadditive_sse <- mean((outsamp_bostonadditive- Boston_test[,"medv"])^2)
#out sample Avg SSE- 17.86

#Neural Network
install.packages("stats")
library(stats)
install.packages("nnet")
library(nnet)

#change the scale of inputs- using range scale- standardize
Boston_df$crimsd <- (Boston_df$crim- min(Boston_df$crim))/(max(Boston_df$crim)-min(Boston_df$crim))
Boston_df$znsd <- (Boston_df$zn- min(Boston_df$zn))/(max(Boston_df$zn)-min(Boston_df$zn))
Boston_df$indussd <- (Boston_df$indus- min(Boston_df$indus))/(max(Boston_df$indus)-min(Boston_df$indus))
Boston_df$chassd<- (Boston_df$chas- min(Boston_df$chas))/(max(Boston_df$chas)-min(Boston_df$chas))
Boston_df$noxsd <- (Boston_df$nox- min(Boston_df$nox))/(max(Boston_df$nox)-min(Boston_df$nox))
Boston_df$rmsd <- (Boston_df$rm- min(Boston_df$rm))/(max(Boston_df$rm)-min(Boston_df$rm))
Boston_df$agesd <- (Boston_df$age- min(Boston_df$age))/(max(Boston_df$age)-min(Boston_df$age))
Boston_df$dissd <- (Boston_df$dis- min(Boston_df$dis))/(max(Boston_df$dis)-min(Boston_df$dis))
Boston_df$radsd <- (Boston_df$rad- min(Boston_df$rad))/(max(Boston_df$rad)-min(Boston_df$rad))
Boston_df$taxsd <- (Boston_df$tax- min(Boston_df$tax))/(max(Boston_df$tax)-min(Boston_df$tax))
Boston_df$ptratiosd <- (Boston_df$ptratio- min(Boston_df$ptratio))/(max(Boston_df$ptratio)-min(Boston_df$ptratio))
Boston_df$blacksd <- (Boston_df$black- min(Boston_df$black))/(max(Boston_df$black)-min(Boston_df$black))
Boston_df$lstatsd <- (Boston_df$lstat- min(Boston_df$lstat))/(max(Boston_df$lstat)-min(Boston_df$lstat))
##Boston_df$medvsd <- (Boston_df$medv- min(Boston_df$medv))/(max(Boston_df$medv)-min(Boston_df$medv))

#using typical standardisation- meand and sd
Boston_df$sd.crim<-(Boston_df$crim-mean(Boston_df$crim))/sd(Boston_df$crim);
Boston_df$sd.zn<-(Boston_df$zn-mean(Boston_df$zn))/sd(Boston_df$zn);
Boston_df$sd.indus<-(Boston_df$indus-mean(Boston_df$indus))/sd(Boston_df$indus);
Boston_df$sd.chas<-(Boston_df$chas-mean(Boston_df$chas))/sd(Boston_df$chas);
Boston_df$sd.nox<-(Boston_df$nox-mean(Boston_df$nox))/sd(Boston_df$nox);
Boston_df$sd.rm<-(Boston_df$rm-mean(Boston_df$rm))/sd(Boston_df$rm);
Boston_df$sd.age<-(Boston_df$age-mean(Boston_df$age))/sd(Boston_df$age);
Boston_df$sd.dis<-(Boston_df$dis-mean(Boston_df$dis))/sd(Boston_df$dis);
Boston_df$sd.rad<-(Boston_df$rad-mean(Boston_df$rad))/sd(Boston_df$rad);
Boston_df$sd.tax<-(Boston_df$tax-mean(Boston_df$tax))/sd(Boston_df$tax);
Boston_df$sd.ptratio<-(Boston_df$ptratio-mean(Boston_df$ptratio))/sd(Boston_df$ptratio);
Boston_df$sd.black<-(Boston_df$black-mean(Boston_df$black))/sd(Boston_df$black);
Boston_df$sd.lstat<-(Boston_df$lstat-mean(Boston_df$lstat))/sd(Boston_df$lstat);

#random sampling
#sample of 75% for training and remaining for vaildation
set.seed(10669218)
nnet_samp <-sample(x= nrow(Boston_df), size =nrow(Boston_df)*0.75)
nnet_train <- Boston_df[nnet_samp,] #train
nnet_vaild <- Boston_df[-nnet_samp,] #validation

#sampling training to get training and testing dataset
set.seed(10669218)
nnet_samp_train <-sample(x= nrow(nnet_train), size =nrow(nnet_train)*0.75)
neural_train <- nnet_train[nnet_samp_train, ]
neural_test <- nnet_train[-nnet_samp_train, ]



train_sse <- 0
test_sse <- 0
layer_size<- 0


#train networks with sizes of hidden units ranging from 0 to 20
for (n in 0:20)
{
  train_predict<-0
  test_predict<-0
  #for each size, train 10 networks with different random starting points
  for(i in 1:10)
  {
    set.seed(i);
    netmod <- nnet(medv ~sd.crim+sd.zn+sd.indus+sd.chas+sd.nox+
                     sd.rm+sd.age+sd.dis+sd.rad+sd.tax+sd.ptratio+
                     sd.black+sd.lstat,size= n, data=neural_train, 
                   rang = 0.00001, linout = TRUE, maxit = 10000, decay = 0, skip = TRUE)
    train_predict<-train_predict+predict(netmod, neural_train)
    test_predict<-test_predict+predict(netmod, neural_test)
  }
  #average outcomes of 10 networks
  neural_train$pred_medv<-train_predict/i
  neural_test$pred_medv<-test_predict/i
  
  # calculate the sum of squared residuals for training and validation sets, SSEs
  test_sse<-rbind(test_sse, mean((neural_test$medv-neural_test$pred_medv)^2))
  train_sse<-rbind(train_sse, mean((neural_train$medv-neural_train$pred_medv)^2))
  layer_size <- rbind(layer_size,n)
  train_predict<-0
  test_predict<-0
}

neustat<-data.frame(test_sse,train_sse,layer_size)
neural_res = neustat[neustat$test_sse > 0,]

View(neural_res)

#plotting

install.packages("ggplot2")
library(ggplot2)
install.packages("reshape2")
library(reshape2)

p <- melt(neural_res, id.vars="layer_size")

ggplot(p, aes(layer_size,value, col=variable)) + geom_point() + geom_line()


test_sse <- 0
train_sse <- 0
layer_size <- 0
decay_value <- 0

for (n in 9:12)
{
  for (w in c(0, 0.01, 0.001, 0.0001))
  {
    train_predict <- 0
    test_predict <- 0
    # for each size, train 10 networks with different random starting points
    for(i in 1:10)
    {
      set.seed(i)
      netmod <- nnet(medv ~sd.crim+sd.zn+sd.indus+sd.chas+sd.nox+
                       sd.rm+sd.age+sd.dis+sd.rad+sd.tax+sd.ptratio+
                       sd.black+sd.lstat,size= n, data=neural_train, 
                     rang = 0.00001, linout = TRUE, maxit = 10000, decay = w, skip = TRUE)
      train_predict<-train_predict+predict(netmod, neural_train)
      test_predict<-test_predict+predict(netmod, neural_test)
    }
    # average outcomes of 10 networks
    neural_train$pred_medv<-train_predict/i
    neural_test$pred_medv<-test_predict/i
    
    # calculate the sum of squared residuals for training and validation sets, SSEs
    test_sse<-rbind(test_sse, mean((neural_test$medv-neural_test$pred_medv)^2));
    train_sse<-rbind(train_sse, mean((neural_train$medv-neural_train$pred_medv)^2));
    layer_size<-rbind(layer_size,n);
    decay_value<-rbind(decay_value,w);
    train_predict<-0;
    test_predict<-0;
  }
}

neustat2<-data.frame(test_sse,layer_size,decay_value)
neustat2$decay_value<-as.factor(neustat2$decay_value)
neural_res2 = neustat2[neustat2$layer_size > 0,]

p2 <- melt(neural_res2, id.vars="layer_size")

ggplot(neural_res2, aes(layer_size,test_sse,group=decay_value, col=decay_value)) +
  geom_point() +
  geom_line()

#final model
n <- 11
decay_value <- 0.001

netmodfinal<-nnet(medv~sd.crim+sd.zn+sd.indus+sd.chas+sd.nox+
                    sd.rm+sd.age+sd.dis+sd.rad+sd.tax+sd.ptratio+
                    sd.black+sd.lstat,size = n, data=nnet_train, rang = 0.00001,
                  linout = TRUE, maxit = 10000, decay = decay_value, skip = TRUE)

#in sample SSE
insampred<-predict(netmodfinal, nnet_train)
mean((nnet_train$medv-insampred)^2) #2.02

#out of sample SSE
outsampred<-predict(netmodfinal, nnet_vaild)
mean((nnet_vaild$medv-outsampred)^2) #30.72
