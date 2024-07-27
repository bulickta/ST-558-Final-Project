library(plumber)
library(tidyverse)
data <- read.csv("~/diabetes_binary_health_indicators_BRFSS2015.csv") |>
  mutate(Diabetes_binary = factor(Diabetes_binary,labels=c("Non_Diabetic","Diabetic")),HighBP=factor(HighBP,labels=c("Normal","High")),HighChol=factor(HighChol,labels=c("Normal","High")),CholCheck=factor(CholCheck,labels=c("No","Yes")),Smoker=factor(Smoker,labels=c("No","Yes")),Stroke=factor(Stroke,labels=c("No","Yes")),HeartDiseaseorAttack=factor(HeartDiseaseorAttack,labels=c("No","Yes")),PhysActivity=factor(PhysActivity,labels=c("No","Yes")),Fruits=factor(Fruits,labels=c("No","Yes")),Veggies=factor(Veggies,labels=c("No","Yes")),HvyAlcoholConsump=factor(HvyAlcoholConsump,labels=c("No","Yes")),AnyHealthcare=factor(AnyHealthcare,labels=c("No","Yes")),NoDocbcCost=factor(NoDocbcCost,labels=c("No","Yes")),GenHlth=factor(GenHlth,labels=c("Excellent","Very Good","Good","Fair","Poor")),DiffWalk=factor(DiffWalk,labels=c("No","Yes")),Sex=factor(Sex,labels=c("Female","Male")),Age=factor(Age,labels=c("18-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80 and older")),Education=factor(Education,labels=c("No school/Kindergarten","Elementary Education","Some High School","High School Graduate","Some College or Technical School","College Graduate")),Income=factor(Income,labels=c("Under $10k","$10k-$15k","$15k-$20k","$20k-$25k","$25k-$35k","$35k-$50k","$50k-$75k","Over $75k")))
commonValues <- data |>
  summarize(across(is.factor),count =  n())
set.seed(28)
index <- createDataPartition(data$Diabetes_binary,p = 0.7,list=FALSE)
training <- data[index,]
test <- data[-index,]
bestModel <-  train(Diabetes_binary~.,data=training,method="glm",family="binomial",preProcess=c("center","scale"),trControl=trainControl(method="cv",number=5,summaryFunction = mnLogLoss, classProbs = TRUE),metric="logLoss")

#* @apiTitle Best Diabetes Model
#* @apiDescription This API allows a user to predict if a specified set of explanatory variables either predicts a person to diabetic or non-diabetic, using most common defaults as applicable

#* Prediction Endpoint
#* @param HighBP
#* @param HighChol
#* @param CholCheck
#* @param BMI
#* @param Smoker
#* @param Stroke
#* @param HeartDiseaseorAttack
#* @param PhysActivity
#* @param Fruits
#* @param Veggies
#* @param HvyAlcoholConsump
#* @param AnyHealthcare
#* @param NoDocbcCost
#* @param GenHlth
#* @param MentHlth
#* @param PhysHlth
#* @param DiffWalk
#* @param Sex
#* @param Age
#* @param Education
#* @param Income
#* @get /pred
function(HighBP = , HighChol = , CholCheck = , BMI = , Smoker = , HeartDiseaseorAttack = , PhysActivity = , Fruits = , Veggies = , HvyAlcoholConsump = , AnyHealthcare = , NoDocbcCost = , GenHlth = , MentHlth = , PhysHlth = , DiffWalk = , Sex = , Age = , Education = , Income = ) {
  input <- rbind(HighBP,HighChol,CholCheck,BMI,Smoker,HeartDiseaseoorAttack,PhysActivity,Fruits,Veggiese,HvyAlcoholConsump,AnyHealthcare,NoDocbcCost,GenHlth,MentHlth,PhysHlth,DiffWalk,Sex,Age,Education,Income)
  predict(bestModel,newdata=input,type="raw")
}

#* Information Endpoint
#* @get /info
function() {
  list("Thomas Bulick","INSERT URL HERE")
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
    as.numeric(a) + as.numeric(b)
}

# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JSON
        pr_set_serializer(serializer_unboxed_json())
}
