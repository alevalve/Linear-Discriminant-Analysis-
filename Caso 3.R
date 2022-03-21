#Case 3 discriminant analysis
#Econometrics 1
# 3CO 2021 ULACIT

#Call and rename data set

Confianza_data <- suppressWarnings(read_excel("Caso 3 Data.xlsx"))
names(Confianza_data)

#Cargar librerias 
library(tidyverse)
install.packages("caret")
library(caret)
install.packages("lattice")
install.packages("MASS")
library(MASS)
library(dplyr)
install.packages("mda")
library(mda)
install.packages("class")
library(class)

#Divide the data into the train and test set

train.data <- sample(c(TRUE, FALSE), nrow(Confianza_data), replace = T, prob = c(0.8,0.2))

train <- Confianza_data[train.data, ]
test <- Confianza_data[!train.data, ]


#Normalize the dataset

prepoc.param <- train %>%
preProcess(method = c("center", "scale"))
train.transformed <- prepoc.param %>% predict(train)
test.transformed <- prepoc.param%>% predict(test)

#LDA discriminant analysis

#Estimate model

#Summary train.transformed

summary(train.transformed)

#Sort column names Company Size

unique(train.transformed$`Company.Size`)

#Count number of samples

table(train.transformed$`Company.Size`)

#Correlation

cor(train.transformed[,1:3])

#Checking classes for their separation

data1 <- prcomp(train.transformed[,1:3], scale. = TRUE)
biplot(data1)
summary(data1)

names(train.transformed)

#Model 1

model <- lda(Company.Size ~., data = train.transformed)
model
plot(model)

plot(model,main="Linear discmininant plot")

#Model prediction
predictions <- model %>% predict(test.transformed)
names(predictions)

## Class predictions

head(predictions$class, 25)

## Predicted Probabilities of Class Membership
head(predictions$posterior, 25)

## Lineal discriminant
head(predictions$x, 25)

## Model Precision
mean(predictions$class==test.transformed$Company.Size)

#Cuadratic Model Analysis

model2 <- qda(Company.Size ~ Trust.in.the.State + Trust.in.the.state.administrations + Trust.in.local.government, train.transformed)
model2

lda.data <- cbind(train.transformed, predict(model)$x)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = Company.Size))+ggtitle("LDA Graphic")

