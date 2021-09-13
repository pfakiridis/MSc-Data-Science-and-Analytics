

library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)

library(factoextra)




##################   pca  #####
res.pca <- prcomp(dissertation_pca,  scale = TRUE)

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

fviz_pca_var(res.pca, col.var = "black",axes = c(1, 2),geom.var = c("text"),title='Biplot of PC1 and PC2')


fviz_pca_var(res.pca, col.var = "black",axes = c(1, 3),geom.var = c("'arrow","text"),title='Biplot of PC1 and PC3')



fviz_pca_biplot(res.pca, label = "var",axes = c(2, 3))


# inspect the attributes of the PCA object returned by prcomp
attributes(test)
# see value section of the help for the prcomp for more details
help(prcomp)


pc_covid_var <- res.pca$sdev^2
pc_covid_var
pc_covid_PEV <- pc_covid_var / sum(pc_covid_var)
pc_covid_PEV




require(ggplot2)
require(reshape)
require(corrgram)
require(RColorBrewer)
require(FactoMineR)
require(igraph)
require(Matrix)



##### cumulative PEV ##################

opar <- par()
plot(
  cumsum(pc_covid_PEV),
  ylim = c(0,1),
  xlab = 'PCs',
  ylab = 'PEV',
  pch = 20,
  col = 'black',
  main='Proportion of Explained Variance'
)
abline(h = 0.8, col = 'red', lty = 'dashed')
par(opar)


#### exploratory data analysis ## ########
### summary statistics  ############

library(reshape2)
library(ggplot2)
d <- melt(dissertation_pca)
theme_set(theme_gray(base_size = 8))
ggplot(d,aes(x = value)) + 
  facet_wrap(~variable,scales = "free") + 
  labs(title='Histograms of All Variables',y="", x = "")+
  geom_histogram()



ppn <-summary(dissertation_pca)
ppn <-psych::describe(dissertation_pca)
pnn


####### importing dta set with the categorical variables too  #######################

###boxplots####

boxplot = Mega_Dataset_Covid_clear_clear[c(12,13,14,15,16)]

ggplot(stack(boxplot), aes(x = ind, y = values)) +
  labs(title='Boxplots of Ethnic Groups',y="", x = "")+
  geom_boxplot()




###boxplots health####




health = Mega_Dataset_Covid_clear_clear[c(21:25)]

ggplot(stack(health), aes(x = ind, y = values)) +
  labs(title='Boxplots of Health Conditions',y="", x = "")+
  geom_boxplot()



###boxplots economic####




economic = Mega_Dataset_Covid_clear_clear[c(10,11,17)]

ggplot(stack(economic), aes(x = ind, y = values)) +
  labs(title='Boxplots of Social Factors',y="", x = "")+
  geom_boxplot()



#### scatter plot income  ##
names(Mega_Dataset_Covid_clear_clear)[27] <- "income"










############ linear regression  ############################

require(caTools)
set.seed(101) 
sample = sample.split(test$avg_deaths, SplitRatio = .7)
train = subset(test, sample == TRUE)
test  = subset(test, sample == FALSE)


############## Linear Regression with one of the variables  ##################

dataoriginal <- lm(deaths_per_thousand~proportion_at_risk_jobs,dissertation_pca)
summary(dataoriginal)



############## MSE  ##################
mean(dataoriginal$residuals^2)



############## RMSE  ##################
predictions <- dataoriginal %>% predict(test)


RMSE(predictions, test$over_70_prop)



############## confidence interval  ##################
confint(dataoriginal) 




############################## svm   ##########################

######feature engineering####

median(dissertation_pca$deaths_per_thousand)


test <-dissertation_pca %>% mutate(avg_deaths = case_when(deaths_per_thousand > 1.872659 ~ 'above', deaths_per_thousand <= 1.872659 ~ 'below'))



test$avg_deaths <- as.factor(test$avg_deaths)







###################### svm kernel with the radial function chosen.####################################

mymodel <- svm(avg_deaths ~ adjusted_income + over_70_prop + proportion_at_risk_jobs + insecure_proportion + all_bame_prop + Obesity + Diabetes + heart_disease + pakistani_bangla_prop + all_indian_prop + vaccination_rate  + adjusted_cases_per_thousand + child_poverty + all_black_prop  +Asthma  +covid_jabs              , data = test, kernel = "radial")


#### summary of the model and confusion matrix

summary(mymodel)

pred <- predict(mymodel, test)
tab <- table(Predicted = pred, Actual = test$avg_deaths)
tab
1-sum(diag(tab)/sum(tab))
sum(diag(tab)/sum(tab))





########  tuning and svm performance  #############################

set.seed(2018)
tmodel <- tune(svm,avg_deaths ~ adjusted_income + over_70_prop + proportion_at_risk_jobs + insecure_proportion + all_bame_prop + Obesity + Diabetes + heart_disease + pakistani_bangla_prop + all_indian_prop + vaccination_rate  + adjusted_cases_per_thousand + child_poverty + all_black_prop + covid_jabs   , data = test,ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:5)))


plot(tmodel)
summary(tmodel)
bestmodel <- tmodel$best.model
summary(bestmodel)
















