library(plumber)
library(tidyverse)
library(caret)
data <- read.csv("/API/diabetes_binary_health_indicators_BRFSS2015.csv") |>
  mutate(Diabetes_binary = factor(Diabetes_binary,labels=c("Non_Diabetic","Diabetic")),HighBP=factor(HighBP,labels=c("Normal","High")),HighChol=factor(HighChol,labels=c("Normal","High")),
         CholCheck=factor(CholCheck,labels=c("No","Yes")),Smoker=factor(Smoker,labels=c("No","Yes")),Stroke=factor(Stroke,labels=c("No","Yes")),HeartDiseaseorAttack=factor(HeartDiseaseorAttack,labels=c("No","Yes")),
         PhysActivity=factor(PhysActivity,labels=c("No","Yes")),Fruits=factor(Fruits,labels=c("No","Yes")),Veggies=factor(Veggies,labels=c("No","Yes")),HvyAlcoholConsump=factor(HvyAlcoholConsump,labels=c("No","Yes")),
         AnyHealthcare=factor(AnyHealthcare,labels=c("No","Yes")),NoDocbcCost=factor(NoDocbcCost,labels=c("No","Yes")),GenHlth=factor(GenHlth,labels=c("Excellent","Very Good","Good","Fair","Poor")),
         DiffWalk=factor(DiffWalk,labels=c("No","Yes")),Sex=factor(Sex,labels=c("Female","Male")),Age=factor(Age,labels=c("18-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80 and older")),
         Education=factor(Education,labels=c("No school/Kindergarten","Elementary Education","Some High School","High School Graduate","Some College or Technical School","College Graduate")),
         Income=factor(Income,labels=c("Under $10k","$10k-$15k","$15k-$20k","$20k-$25k","$25k-$35k","$35k-$50k","$50k-$75k","Over $75k")))
nums <- data|>
  select(where(is.numeric))
meanNums <- list()
meanNums$BMI <- round(mean(nums[,1]),0)
meanNums$MentHlth <- round(mean(nums[,2]),0)
meanNums$PhysHlth <- round(mean(nums[,3]),0)

facts <- data |>
  select(where(is.factor))
commonFacts <- c()
for (i in 1:length(facts)){
  var <- table(facts[i])
  commonFacts[i] <- names(var[1])
}

set.seed(28)
index <- createDataPartition(data$Diabetes_binary,p = 0.7,list=FALSE)
training <- data[index,]
test <- data[-index,]
bestModel <-  train(Diabetes_binary~.,data=training,method="glm",family="binomial",preProcess=c("center","scale"),trControl=trainControl(method="cv",number=5,summaryFunction = mnLogLoss, classProbs = TRUE),metric="logLoss")
test[5,]
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
function(HighBP = commonFacts[2], HighChol = commonFacts[3], CholCheck = commonFacts[4], BMI = meanNums$BMI, Smoker = commonFacts[5], Stroke = commonFacts[6], HeartDiseaseorAttack = commonFacts[7], PhysActivity = commonFacts[8], Fruits = commonFacts[9], Veggies = commonFacts[10], HvyAlcoholConsump = commonFacts[11], AnyHealthcare = commonFacts[12], NoDocbcCost = commonFacts[13], GenHlth = commonFacts[14], MentHlth = meanNums$MentHlth, PhysHlth = meanNums$PhysHlth, DiffWalk = commonFacts[15], Sex = commonFacts[16], Age = commonFacts[17], Education = commonFacts[18], Income = commonFacts[19]) {
  training[1,] <- NA
  input <- training[1,]
  input$HighBP <- HighBP
  input$HighChol <- HighChol
  input$CholCheck <- CholCheck
  input$BMI <- as.integer(BMI)
  input$Smoker <- Smoker
  input$Stroke <- Stroke
  input$HeartDiseaseorAttack <- HeartDiseaseorAttack
  input$PhysActivity <- PhysActivity
  input$Fruits <- Fruits
  input$Veggies <- Veggies
  input$HvyAlcoholConsump <- HvyAlcoholConsump
  input$AnyHealthcare <- AnyHealthcare
  input$NoDocbcCost <- NoDocbcCost
  input$GenHlth <- GenHlth
  input$MentHlth <- as.integer(MentHlth)
  input$PhysHlth <- as.integer(PhysHlth)
  input$DiffWalk <- DiffWalk
  input$Sex <- Sex
  input$Age <- Age
  input$Education <- Education
  input$Income <- Income
  predict(bestModel,newdata=input)
}

#Query pred with defaults (should give "non-diabetic"): http://localhost:8000/pred
#Query pred with custom settings that give a "diabetics" outcome: http://localhost:8000/pred?HighBP=High&HighChol=High&CholCheck=Yes&BMI=40&Smoker=Yes&Stroke=No&HeartDiseaseorAttack=No&PhysActivity=No&Fruits=Yes&Veggies=Yes&HvyAlcoholConsump=No&AnyHealthcare=Yes&NoDocbcCost=No&GenHlth=Poor&MentHlth=15&PhysHlth=30&DiffWalk=Yes&Sex=Female&Age=80%20and%20older&Education=No%20school%2FKindergarten&Income=Under%20%2410k
#Query pred with cutsom settings that give a "non-diabteic" outcome: http://localhost:8000/pred?HighBP=High&HighChol=High&CholCheck=No&BMI=40&Smoker=Yes&Stroke=Yes&HeartDiseaseorAttack=Yes&PhysActivity=No&Fruits=Yes&Veggies=Yes&HvyAlcoholConsump=Yes&AnyHealthcare=No&NoDocbcCost=No&GenHlth=Poor&MentHlth=20&PhysHlth=20&DiffWalk=Yes&Sex=Female&Age=18-24&Education=No%20school%2FKindergarten&Income=Under%20%2410k


#* Information Endpoint
#* @get /info
function() {
  list("Thomas Bulick","https://bulickta.github.io/ST-558-Final-Project/")
}

#Query info: http://localhost:8000/info
