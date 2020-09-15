#required library call
library(tidyverse)
library(ggplot2)

#read sequentially the name of the files from a directory
setwd("D:/umea/files/files/protected/")

fold_files <- list.files(pattern = "*.txt", full.names = TRUE)
fold_files <- data.frame(fold_files)
fold_files <- separate(fold_files, fold_files, c("root", "file"), sep ="/")
file_name <- as.data.frame(fold_files[,2])

#declare the error data.frame for future use
error <- data.frame(rsquared = rep(0,nrow(fold_files)),
                    rse = rep(0,nrow(fold_files)))

for(i in 1:nrow(file_name))
{
  #reading data
  k <- as.character(file_name[i,1])
  a <- read.table(k, header = FALSE)
  colnames(a) <- c("v1","v2","v3","v4","v5","v6","v7","v8","v9","v10","v11","v12","v13")
  
  #train/test division
  train <- a[1:1000,]
  test <- a[1001:1080,]
  
  #multiple regression model
  mod <- lm(v13 ~ v1+v2+v3+v4+v5+v6+v7+v8+v9+v10+v11+v12, data=train)
  pred <- predict.lm(mod,test)
  pred <- as.data.frame(pred)
  
  #rse error calculation
  rse <- sigma(mod)/mean(pred$pred)
  
  #plotting and saving the prediction of the regression model
  plot_pred <- as.data.frame(cbind(test[,13],pred[,1]))
  colnames(plot_pred) <- c("acout","pred")
  path <- paste0("D:/umea/result",file_name[i,1],".pdf")
  
  ggplot(plot_pred, aes(x=acout,y=pred))+ geom_point()+geom_abline()
  ggsave(path)
  
  #saving the error from evaluation/prediction with the test data
  error[i,1] <- summary(mod)$r.squared
  error[i,2] <- rse
}

#plotting the error
error_file <- as.data.frame(cbind(file_name[,1],error[,1:2]))
write.table(error_file, "D:/umea/error_result.txt")
error_file$name <- as.factor(error_file$`file_name[, 1]`)
ggplot(error_file,aes(x=name,y=rse))+geom_point(color = "red")+
  theme(axis.text.x = element_text(face="bold", color="black", size=10, angle=30))
ggsave("D:/rse_error_plot.pdf")

