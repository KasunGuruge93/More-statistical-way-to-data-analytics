
library(caTools)

getwd()  # get the current working directory
setwd("/Users/kasun/OneDrive/Desktop/Data_analysis/R") # Set the directory




# Importing the dataset
# dataset = read.csv('basketballData.csv')  # Get the dataset
dataset = read.csv('50_Startups.csv')

str(dataset)
head(dataset)

tail(dataset)

attach(dataset)
# subset(dataset, select=-c(X5))
#######################




# THE FUNCTION

pred = function (dataset, responce) {
  
  dataset= cbind(dataset, responce)
  
  my.class <- rep('empty', ncol(dataset))  #create a empty dataset that equal length with dataset
  
  for (i in 1:ncol(dataset)) {
    my.class[i] <- class(dataset[, i])
    
    
    
    #  check whether the data columns are numeric or categorical
    
    
    if (my.class[i] == "integer" || my.class[i] == "numeric") {
      #For Numeric variable
      
      print("######################## Get the mean , mode and median of the numeric variables#######################################")
      meanv = paste("Mean of ", colnames(dataset[i]), "is :" )
      print(meanv)
      print(mean(dataset[, i]))
      modev = paste("Mode of ", colnames(dataset[i]), "is :" )  
      print(modev)
      print(mode(dataset[, i]))
      medianv = paste("Median of ", colnames(dataset[i]), "is :")
      print(medianv)
      print(median(dataset[, i]))
      
      
      cat("\n")
      
      
      dataset[, i][is.na(dataset[, i])] = mean(dataset[, i], na.rm = TRUE) # Impute the missing values using "mean"
      
      print("###################### #Identify outliers########################################")
      outliersofdata = paste("Outliers of ",
                             colnames(dataset[i]),
                             "is  :")
      print(outliersofdata)
      print(boxplot.stats(dataset[, i])$out)
      
      cat("\n")
     
      boxplot(dataset[, i]) #Summarize each variable using a proper visualization tool
      
      
      
      
      
      
      
    } else {
      #For categorical variable
      
      print("#########calling categorical variable#######################")
      
      val <- unique(dataset[, i][!is.na(dataset[, i])])
      mode <- val[which.max(tabulate(match(dataset[, i], val)))]
      modeis = paste("mode is", mode)
      print(modeis)
      
      dataset[, i][is.na(dataset[, i])] = mode  # Impute the missing values using "mode"
      
      # outliersofdata = paste("Outliers of ",
      #                        colnames(dataset[i]),
      #                        "is ",
      #                        boxplot.stats(dataset[, i])$out) #Identify outliers
      # print(outliersofdata)
      
      # hist(dataset[, i]) #Summarize each variable using a proper visualization tool
    }
  }
  
  
  
  
  
  print("#####predicting#######################")
  
  count = 0
  
  
  for (r in sapply(responce, function(x) {
    all(na.omit(x) %in% 0:1)
  })) {
    if (r == T) {
      count = count + 1
    }
  }
  
  if (count != length(responce)) {
   
    print("###############calling multiple linear regression model#########################")
    
    if (my.class[i] == "factor"){
      dataset[i] = factor(dataset[i],
                               levels = c('New York', 'California', 'Florida'),  #Encode the categorical variable
                               labels = c(1, 2, 3))
    }
    
    head(dataset)
    
    set.seed(123)
    split = sample.split(responce, SplitRatio = 0.8)
    training_set = subset(dataset, split == TRUE)
    test_set = subset(dataset, split == FALSE)
    
    mod = lm(formula = responce ~ ., data = training_set)
    
    summary(mod)
    
    
    predict = predict(mod, newdata = test_set)
    
    print("Predicted Results are ")
    
    print(predict)
    
    
    #diagnostic metrics and plots
    library(ggplot2)
    
    ggplot() +
      geom_point(aes(x = X3 , y = responce), colour = 'red') +
      geom_line (aes(x = X3 , y = predict), colour = 'blue') +
      ggtitle('dcw') +
      xlab('wc') +
      ylab('wcw')
    
    
    
    par(mfrow=c(2,2))
    plot(mod)
    
    
    
    
    
  } else{
    
    
    print("###########################calling logistic regression model##########################")
  
    # if (my.class[i] == "factor"){
    #   dataset[i] = factor(dataset[i],
    #                       levels = c('New York', 'California', 'Florida'),   #Encode the categorical variable
    #                       labels = c(1, 2, 3))
    # }
    
    print(is.data.frame(dataset))
    print(head(dataset))
    library(caTools)
    set.seed(123)
    split = sample.split(responce, SplitRatio = 0.8)
    training_set = subset(dataset, split == TRUE)
    test_set = subset(dataset, split == FALSE)
    
    substr=subset(training_set, select=-c(responce))
    substest=subset(test_set, select=-c(responce))
    # Feature Scaling
    substr = scale(substr)
    substest = scale(substest)



    # Fitting Logistic Regression to the Training set
    classifier = glm(formula = responce ~ .,
                     family = binomial,
                     data = training_set)
    par(mfrow=c(2,2))
    plot(classifier)

  }
  
  
  
  
}





x = data.frame(responce=rbinom(n = 50, size = 1, prob = 0.1))
print(x)
y = seq(1, 10, length.out = 50)
print(y)
z = c(1, 2, 3, 4)



#pred(dataset, y)
pred(dataset, x)
  

