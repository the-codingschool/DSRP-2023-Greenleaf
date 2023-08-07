library(tidyr)
library(janitor)
library(dplyr)
library(tidymodels)
library(dplyr)
library(reshape2)
library(rsample)
library(Metrics)
library(parsnip)
library(yardstick)
library(ggplot2)
library(tidyverse)
library(imputeTS)
library(xgboost)
library(ranger)
install.packages("caret")
library(caret)
library(maps)


ggplot(class_results,aes(x= class_results$boost_pred, y=class_results$diabcntrlmeds,
                         color = class_results$boost_pred )) + geom_jitter()

##Machine Learning Bar Plot

a <- data.frame(Variables=c("diabcntrlmeds ","insure5", "neighpov1519", "insure20r "), Percentage=c(80,91,67, 83), frame=rep('a',4)) 
ggplot(a, aes(x=Variables, y= Percentage, fill=Variables))  + 
  geom_bar(stat='identity')+
  labs(title="Boost Testing")+
  coord_flip()


