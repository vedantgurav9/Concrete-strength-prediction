concrete<-read.csv(file.choose(),stringsAsFactors = FALSE)
View(concrete)
str(concrete)

hist(concrete$strength)
normalise<-function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
concrete_n<-as.data.frame(lapply(concrete,normalise))
summary(concrete_n$strength)

concrete_train<-concrete_n[1:721,]
concrete_test<-concrete_n[722:1030,]

install.packages('neuralnet')
library(neuralnet)

concrete_model<-neuralnet(strength ~ cement + slag + ash + water + superplasticizer + coarseagg + fineagg + age, data = concrete_train)

plot(concrete_model)

model_results<- compute(concrete_model,concrete_test[1:8])

predicted_strength<-model_results$net.result
cor(predicted_strength, concrete_test$strength)

concrete_model2<-neuralnet(strength ~ cement + slag + ash + water + superplasticizer + coarseagg + fineagg + age, data = concrete_train, hidden = 5)

plot(concrete_model2)

model2_results<-compute(concrete_model2,concrete_test[1:8])
predicted_strength2<-model2_results$net.result
cor(predicted_strength2,concrete_test$strength)
