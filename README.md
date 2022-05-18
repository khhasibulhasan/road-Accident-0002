# road-Accident-0002
install.packages("shiny")
install.packages("party")
install.packages("shinydashboard")

library (shiny)
library(party)
library(shinydashboard)

practice <- read.csv("Road.csv", header = TRUE)

View(practice)

names(practice)
head(practice)
tail(practice)
summary(practice)
str(practice)

nrow(practice)
ncol(practice)
dim(practice)

practice$Road_SurfaceA <- as.factor(practice$Road_Surface)
str(practice)

set.seed(1234)

pd<- sample(2, nrow(practice), replace = TRUE, prob = c(0.85, 0.15))
pd

dotrain <- practice [pd==1, ]
dovalidate <- practice[pd==2,]

dim(dotrain)
dim(dovalidate)


practice_tree <- ctree(Road_SurfaceF~Dictrict+Road_Type+Light+Weather, data = train)
practice_tree

print(practice_tree)


plot(practice_tree)

plot(practice_tree, type="simple")

predict(practice_tree)

ABtab<- table(predict(practice_tree), train$Road_SurfaceF)

print(ABtab)

sum(diag(ABtab))/sum(ABtab)
1-sum(diag(ABtab))/sum(ABtab)

practice_predict <- table(predict(practice_tree, newdata=validate), validate$Road_SurfaceF)
print(practice_predict)

sum((diag(practice_predict))/sum(practice_predict))
1-sum((diag(practice_predict))/sum(practice_predict))
