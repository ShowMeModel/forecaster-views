library(ggplot2)
library(scales)
library(dplyr)
library(lazyeval)
library(reshape2)
library(caret)

set.seed(42)

calculateMRSE <- function(predicted, gold){
  all <- cbind(predicted, gold)
  all <- all %>% mutate( RSE = ( (predicted/V_168) - 1 ) ** 2 )
  return(mean(all$RSE))
}

# Read in the data.csv and analyse the basic statistics of V(n)

setwd("C:\\Projekty\\Tooploox")

dataset <- read.csv(file="data.csv", 
                    sep=",", 
                    header=FALSE, 
                    stringsAsFactors=FALSE,
                    col.names=c("id", paste("V", seq(1, 168), sep="_")))

# Check n = {24;72;168} as well

chosen <- dataset[,c('V_24', 'V_72', 'V_168')]
summary(chosen)

# Returns Tukey's five number summary (minimum, lower-hinge, median, upper-hinge, maximum)
sapply(chosen, function(x) fivenum(x) )

# Plot the distribution of the v(168)

ggplot(dataset, aes(x=V_168)) + 
       geom_histogram(aes(y=..density..),
       binwidth=10000,
       colour="black", fill="white") +
       geom_density(alpha=.2, fill="#FF6666") + 
       scale_x_continuous(labels = comma) + 
       scale_y_continuous(labels = comma)

ggplot(dataset, aes(x=V_168)) +
       geom_histogram(binwidth=10000, colour="black", fill="white") +
       geom_vline(aes(xintercept=mean(V_168)),
                  color="red", linetype="dashed", size=1) +
       scale_x_continuous(labels = comma) + 
       scale_y_continuous(labels = comma)

# The distributon is very skewed, which is typical for data on virtual communities

# Lets make log transform on it

ggplot(dataset, aes(x=log10(V_168 + 1))) + 
  geom_histogram(aes(y=..density..),
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + 
  scale_x_continuous(labels = comma) + 
  scale_y_continuous(labels = comma)

ggplot(dataset, aes(x=log10(V_168 + 1))) +
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(log10(V_168 + 1))),
             color="red", linetype="dashed", size=1) +
  scale_x_continuous(labels = comma) + 
  scale_y_continuous(labels = comma)

# Remove outliers from the dataset

mean_v_168 <- mean(log10(dataset[,"V_168"]))
std_v_168 <- sd(log10(dataset[,"V_168"]))

dataset <- dataset[((mean_v_168 - 3*std_v_168 <= log10(dataset[,"V_168"])) & 
                     (log10(dataset[,"V_168"]) <= mean_v_168 + 3*std_v_168)), ]

# Compute correlation coefficients between the log-transformed v(n) for n = {1;2;...;24} and v(168)

df_cor_cols <- paste("V", seq(1, 24), sep="_")
df_cor_all <- c("V_168", df_cor_cols)
dfcor <- dataset %>% 
          mutate(V_1 = ifelse(V_1 == 0, 1, V_1)) %>%
          mutate_each_(funs(log10), df_cor_cols) %>% 
          select_(interp(~one_of(df_cor_all), var = as.name(df_cor_all)))

cormat <- round(cor(dfcor),2)
melted_cormat <- melt(cormat)
head(melted_cormat, 25)

library(polycor)
hetcor(dfcor)

# Randomly split the dataset into training and test sets (10% for testing, rest for training)

testIndex = createDataPartition(dataset$V_168, p = 0.9)[[1]]
testing = dataset[-testIndex,]
training = dataset[testIndex,]

cls_indx = which(names(training) %in% c("id"))

# Using training data, and linear regressor that minimizes Ordinary Least Squares (OLS)
# error function. It should take as the input v(n) and output v(168).

modelFit <- train(V_168 ~ ., method="lm", data=training[,-cls_indx])

# Extend the above linear regressor with multiple inputs,

incremental <- c()
results <- data.frame(V=NULL, mRSE=NULL, regression=NULL)

for (feature_name in df_cor_cols){
  incremental <- append(incremental, feature_name)
  formula <- as.formula(paste("V_168 ~", feature_name))
  formulaMulti <- as.formula(paste("V_168 ~", ifelse(length(incremental) < 2, 
                                                     ' V_1', 
                                                     paste(incremental, collapse = ' + '))))
  print(formula)
  print(formulaMulti)
  modelFit <- train(formula, method="lm", data=training[,-cls_indx])
  modelMultiFit <- train(formulaMulti, method="lm", data=training[,-cls_indx])
  print(modelFit)
  print(modelMultiFit)
  predicted <- predict(modelFit,testing[,-cls_indx])
  predictedMulti <- predict(modelMultiFit,testing[,-cls_indx])
  
  b <- data.frame(V = length(incremental), mRSE = calculateMRSE(predicted, testing), regression='single')
  m <- data.frame(V = length(incremental), mRSE = calculateMRSE(predictedMulti, testing), regression='multi')
  results <- bind_rows(results, b)
  results <- bind_rows(results, m)
}

# Plot that

ggplot(data=results, aes(x=V, y=mRSE, group=regression, shape=regression, colour=regression)) + 
  geom_line(size=1.5) + 
  geom_point(size=3, fill="white") +
  scale_shape_manual(values=c(22,21))

# Done!
