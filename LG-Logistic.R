# === TP D'ECONOMETRIE : IONOSPHERE ===
#           Lucas Gonçalves

# Question: on cherche à modéliser la réaction de la ionosphere (haute couche de
# l'atmosphere) mesurée par la variable `Class` en fonction d'impulsions
# électromagnétiques caractérisées par 34 mesures `V1` ... `V34`. => "Good : 1" "Bad : 0"

install.packages('caret', dependencies = TRUE)
install.packages("pROC")

library(ggplot2)
library(caret)
library(pROC)
wd <- getwd()
setwd("~/Git/econometrie-tp")

Ionosphere <- read.table(file = 'Data/Ionosphere.csv',header=TRUE, sep=';')


# Comme on peut le voir avec cette commande, on a un dataset equilibré donc nous n'avons pas besoin de corriger le poids
# d'un dataset unbalanced
table(Ionosphere$Class)
# RES : 
#  0 -> 126 1 -> 225

dt = sort(sample(nrow(Ionosphere), nrow(Ionosphere)*.7))
train <-  Ionosphere[dt,] #Data pour le model
test  <-  Ionosphere[-dt,] #Data pour le predict

cor(Ionosphere, use="complete.obs")

# Regression logistique --- Model : 
log_reg <- glm(Class ~ ., data= train, family = 'binomial')
step(log_reg, direction="backward")
summary(log_reg)

# RES :
#
#     Null deviance: 458.28  on 350  degrees of freedom
#     Residual deviance: 111.05  on 317  degrees of freedom
#     AIC: 179.05
#

# Regression Logistique --- Training : 
res_predict <- predict(log_reg, newdata = test, type = "response")
train_y <- as.integer(res_predict > 0.5)

# Matrice de confusion
table(train_y, test$Class)

confusionMatrix(
  data= as.factor(train_y), 
  reference = as.factor(test$Class), 
  positive = "1")

# === Calcul de la sensibilité idéale (threshold)

sensibility <- function(treshold, df) {
  out <- sum(as.integer(res_predict > treshold) == 1 & df$Class == 1) / sum(df$Class == 1)
  return(out)
}

specificity <- function(treshold, df) {
  out <- sum(as.integer(res_predict > treshold) == 0 & df$Class == 0) / sum(df$Class == 0)
  return(out)
}

treshold <- seq(0,1, 0.001)

# Calcul de sensibility : 
sens <- sapply(treshold, sensibility, df = test)
spec <- sapply(treshold, specificity, df = test)

data2plot <- data.frame(treshold = rep(treshold,2),
                        value = c(sens,spec),
                        tag   = rep(c("sensitivity", "specificity"), 
                                    each = length(treshold)))

# Tableau sensitivity x sensibility
ggplot(data2plot, aes(x=treshold, y=value)) + 
  geom_line(aes(col=tag)) + 
  theme_bw() + theme(legend.title = element_blank())




# Area under roc : 


data2plot <- data.frame(treshold=treshold, 
                        sensitivity = sens,
                        specificity = spec)

ggplot(data2plot, aes(x= 1 - specificity, y=sensitivity)) + geom_line() + theme_bw()

#Calc AUR
auc(test$Class, res_predict)
# res = Area under the curve: 0.8423















