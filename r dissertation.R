

library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)

library(factoextra)


dissertation_pca <- Dataset_for_MSc_Data_Science_and_Analytics[,-c(1:3)]



##################   pca  #####
res.pca <- prcomp(dissertation_pca,  scale = TRUE)

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

fviz_pca_var(res.pca, col.var = "black",axes = c(1, 2),geom.var = c("text"),title='Biplot of PC1 and PC2')


fviz_pca_var(res.pca, col.var = "black",axes = c(1, 3),geom.var = c("'arrow","text"),title='Biplot of PC1 and PC3')



fviz_pca_biplot(res.pca, label = "var",axes = c(2, 3))



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
abline(h = 0.75, col = 'red', lty = 'dashed')
par(opar)


#### exploratory data analysis ## ########
### summary statistics  histograms ############

library(reshape2)
library(ggplot2)
d <- melt(dissertation_pca)
theme_set(theme_gray(base_size = 8))
ggplot(d,aes(x = value)) + 
  facet_wrap(~variable,scales = "free") + 
  labs(title='Histograms of All Variables',y="", x = "")+
  geom_histogram()

### summary statistics  table ############

ppn <-summary(dissertation_pca)
ppn <-psych::describe(dissertation_pca)
ppn


####### importing dta set with the categorical variables too  #######################

###boxplots####

boxplot = dissertation_pca[c(7:11)]

ggplot(stack(boxplot), aes(x = ind, y = values)) +
  labs(title='Boxplots of Ethnic Groups',y="", x = "")+
  geom_boxplot()




###boxplots health####




health = dissertation_pca[c(17:21)]

ggplot(stack(health), aes(x = ind, y = values)) +
  labs(title='Boxplots of Health Conditions',y="", x = "")+
  geom_boxplot()



###boxplots economic####




economic = dissertation_pca[c(5,6,12)]

ggplot(stack(economic), aes(x = ind, y = values)) +
  labs(title='Boxplots of Social Factors',y="", x = "")+
  geom_boxplot()






############## Linear Regression with one of the variables  ##################

dataoriginal <- lm(covid_19_deaths_per_thousand~proportion_at_risk_jobs,dissertation_pca)
summary(dataoriginal)



############## MSE  ##################
mean(dataoriginal$residuals^2)



############## RMSE  ##################
predictions <- dataoriginal %>% predict(dissertation_pca)


install.packages('MLmetrics')
library('MLmetrics')
RMSE(predictions, dissertation_pca$over_70_prop)



############## confidence interval  ##################
confint(dataoriginal) 




############################## svm   ##########################

######feature engineering####

median(dissertation_pca$covid_19_deaths_per_thousand)


dissertation_pca <-dissertation_pca %>% mutate(avg_deaths = case_when(covid_19_deaths_per_thousand > 1.872659 ~ 'above', covid_19_deaths_per_thousand <= 1.872659 ~ 'below'))

names(dissertation_pca)[names(dissertation_pca) == 'Net annual income after housing costs (£)'] <- "adjusted_income"
names(dissertation_pca)[names(dissertation_pca) == 'Obesity (18+)'] <- "Obesity"
names(dissertation_pca)[names(dissertation_pca) == 'Coronary heart disease'] <- "heart_disease"
names(dissertation_pca)[names(dissertation_pca) == 'pakistani_or_bangladeshi_prop'] <- "pakistani_bangla_prop"
names(dissertation_pca)[names(dissertation_pca) == 'covid jabs given up to 20/07/2021'] <- "covid_jabs"




require(caTools)
set.seed(101) 
sample = sample.split(dissertation_pca$avg_deaths, SplitRatio = .7)
train = subset(dissertation_pca, sample == TRUE)
test  = subset(dissertation_pca, sample == FALSE)


test$avg_deaths <- as.factor(test$avg_deaths)







###################### svm kernel with the radial function chosen.####################################
library(e1071)



mymodel <- svm(avg_deaths ~ adjusted_income + over_70_prop + proportion_at_risk_jobs + insecure_proportion + all_bame_prop + Obesity + Diabetes + heart_disease + pakistani_bangla_prop + all_indian_prop + vaccination_rate  + adjusted_cases_per_thousand + child_poverty + all_black_prop  +Asthma  +covid_jabs, data = test, kernel = "radial")


#### summary of the model and confusion matrix

summary(mymodel)

pred <- predict(mymodel, test)
tab <- table(Predicted = pred, Actual = test$avg_deaths)
tab
1-sum(diag(tab)/sum(tab))
sum(diag(tab)/sum(tab))





########  tuning and svm performance , play around with cost #############################

set.seed(2018)
tmodel <- tune(svm,avg_deaths ~ adjusted_income + over_70_prop + proportion_at_risk_jobs + insecure_proportion + all_bame_prop + Obesity + Diabetes + heart_disease + pakistani_bangla_prop + all_indian_prop + vaccination_rate  + adjusted_cases_per_thousand + child_poverty + all_black_prop + covid_jabs   , data = test,ranges = list(epsilon = seq(0,1,0.1), cost = 2^(1:4)))


plot(tmodel)
summary(tmodel)
bestmodel <- tmodel$best.model
summary(bestmodel)















