#install.packages("dslabs")
library(dslabs)
help(package = "dslabs")

#import the dataset
data("gapminder")
head(gapminder)

#make the dataset looks cleaner
#install.packages("dplyr")
library(dplyr)
example.gapminder <- gapminder %>% select(continent, life_expectancy, infant_mortality)

#omit the NA data
example.gapminder <- na.omit(example.gapminder)

#write the power function for further use
power.function <- function (n, effect.size, siglevel = 0.05){ 
  effect.size <- abs(effect.size)
  rz <- atanh(effect.size) + effect.size/(2 * (n - 1))
  quant <- qt(siglevel/2, df = n - 2, lower = FALSE)
  rc <- sqrt(quant^2/(quant^2 + n - 2))
  rcz <- atanh(rc)
  power <- pnorm((rz - rcz) * sqrt(n - 3)) + pnorm((-rz - rcz) * sqrt(n - 3))
  return(power)
}

#####Scenario 1 - Data collected from different continents
#calculate the mean and sd of infant mortality and life expectancy
#install.packages("dplyr")
library(dplyr)
scenario1.gapminder <- example.gapminder %>% group_by(continent) %>% 
  summarise(mean.infantmortality= mean(infant_mortality), sd.infantmortality = sd(infant_mortality), mean.lifeexpectancy = mean(life_expectancy), sd.lifeexpectancy = sd(life_expectancy))
head(scenario1.gapminder)

#convert it into dataframe
scenario1.gapminder.dataframe <- as.data.frame(scenario1.gapminder)
scenario1.gapminder.dataframe

#create a function using Monte Carlo method to simulate dataset
simulate.dataset1 <- function(m){
  N <- 1000
  sim_data1 <- matrix(data = NA, nrow = 100, ncol = 2) 
  colnames(sim_data1) <- c("infant.mortality", "life.expectancy")
  simulated_means_infantmortality1 <- rep (NA, N)
  simulated_means_lifeexpectancy1 <- rep (NA, N)
  for (i in 1:N){
    sim_data1[,1] <- rnorm(n = 100, mean = scenario1.gapminder.dataframe[m,2], sd = scenario1.gapminder.dataframe[m,3])
    sim_data1[,2] <- rnorm(n = 100, mean = scenario1.gapminder.dataframe[m,4], sd = scenario1.gapminder.dataframe[m,5])
    simulated_means_infantmortality1[i] <- mean(sim_data1[,1])
    simulated_means_lifeexpectancy1[i] <- mean(sim_data1[,2])
  }
  hist(simulated_means_infantmortality1)
  hist(simulated_means_lifeexpectancy1)
  return(sim_data1)
}

#simulate samples of five continents
set.seed(41131)
simulate.dataset.Africa <- as.data.frame(simulate.dataset1(1))
simulate.dataset.Americas <- as.data.frame(simulate.dataset1(2))
simulate.dataset.Asia <- as.data.frame(simulate.dataset1(3))
simulate.dataset.Europe <- as.data.frame(simulate.dataset1(4))
simulate.dataset.Oceania <- as.data.frame(simulate.dataset1(5))

#create matrix for Effect Size and Power
result_data1 <- matrix(data = NA, nrow = 5, ncol = 2) 
colnames(result_data1) <- c("Effect Size", "Power")
rownames(result_data1) <- c("Africa", "Americas", "Asia", "Europe", "Oceania")

###########AFRICA################
#Correlation Coefficient Test～～～～～～～～～～～～～～～～～
#install.packages("ggpubr")
library("ggpubr")
ggscatter(simulate.dataset.Africa, x = "infant.mortality", y = "life.expectancy",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Infant deaths per 1000", ylab = "Life expectancy in years")
#Shapiro-Wilk normality test for infant mortality
shapiro.test(simulate.dataset.Africa$infant.mortality)
#Shapiro-Wilk normality test for life expectancy
shapiro.test(simulate.dataset.Africa$life.expectancy)
#Visual inspection for infant mortality
ggqqplot(simulate.dataset.Africa$infant.mortality, ylab = "Infant Mortality")
#Visual inspection for life expectancy
ggqqplot(simulate.dataset.Africa$life.expectancy, ylab = "Life expectancy")
#Pearson Correlation Test
cor.test(simulate.dataset.Africa$infant.mortality, simulate.dataset.Africa$life.expectancy,
         method = "pearson")

#Spearman Rank Correlation - Nonparametric Test～～～～～～～～～～～～
cor.test(simulate.dataset.Africa$infant.mortality, simulate.dataset.Africa$life.expectancy,
         method = "spearman")

#Calculate Effect size and Power
result_data1[1,1] <- -0.06314464
result_data1[1,2] <- power.function(n = 100, effect.size = result_data1[1,1])

#############AMERICAS################
#Correlation Coefficient Test～～～～～～～～～～～～～～～～～
#install.packages("ggpubr")
library("ggpubr")
ggscatter(simulate.dataset.Americas, x = "infant.mortality", y = "life.expectancy",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Infant deaths per 1000", ylab = "Life expectancy in years")
#Shapiro-Wilk normality test 
shapiro.test(simulate.dataset.Americas$infant.mortality)
shapiro.test(simulate.dataset.Americas$life.expectancy)
#Visual inspection
ggqqplot(simulate.dataset.Americas$infant.mortality, ylab = "Infant Mortality")
ggqqplot(simulate.dataset.Americas$life.expectancy, ylab = "Life expectancy")
#Pearson Correlation Test
cor.test(simulate.dataset.Americas$infant.mortality, simulate.dataset.Americas$life.expectancy,
         method = "pearson")

#Spearman Rank Correlation - Nonparametric Test～～～～～～～～～～～～
cor.test(simulate.dataset.Americas$infant.mortality, simulate.dataset.Americas$life.expectancy,
         method = "spearman")

#Calculate Effect size and Power
result_data1[2,1] <- 0.04397091
result_data1[2,2] <- power.function(n = 100, effect.size = result_data1[2,1])

##################ASIA################
#Correlation Coefficient Test～～～～～～～～～～～～～～～～～
#install.packages("ggpubr")
library("ggpubr")
ggscatter(simulate.dataset.Asia, x = "infant.mortality", y = "life.expectancy",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Infant deaths per 1000", ylab = "Life expectancy in years")
#Shapiro-Wilk normality test
shapiro.test(simulate.dataset.Asia$infant.mortality)
shapiro.test(simulate.dataset.Asia$life.expectancy)
#Visual inspection
ggqqplot(simulate.dataset.Asia$infant.mortality, ylab = "Infant Mortality")
ggqqplot(simulate.dataset.Asia$life.expectancy, ylab = "Life expectancy")
#Pearson Correlation Test
cor.test(simulate.dataset.Asia$infant.mortality, simulate.dataset.Asia$life.expectancy,
         method = "pearson")

#Spearman Rank Correlation - Nonparametric Test～～～～～～～～～～～～
cor.test(simulate.dataset.Asia$infant.mortality, simulate.dataset.Asia$life.expectancy,
         method = "spearman")

#Calculate Effect size and Power
result_data1[3,1] <- -0.00665304
result_data1[3,2] <- power.function(n = 100, effect.size = result_data1[3,1])

##########EUROPE################
#Correlation Coefficient Test～～～～～～～～～～～～～～～～～
#install.packages("ggpubr")
library("ggpubr")
ggscatter(simulate.dataset.Europe, x = "infant.mortality", y = "life.expectancy",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Infant deaths per 1000", ylab = "Life expectancy in years")
#Shapiro-Wilk normality test
shapiro.test(simulate.dataset.Europe$infant.mortality)
shapiro.test(simulate.dataset.Europe$life.expectancy)
#Visual inspection
ggqqplot(simulate.dataset.Europe$infant.mortality, ylab = "Infant Mortality")
ggqqplot(simulate.dataset.Europe$life.expectancy, ylab = "Life expectancy")
#Pearson Correlation Test
cor.test(simulate.dataset.Europe$infant.mortality, simulate.dataset.Europe$life.expectancy,
         method = "pearson")

#Spearman Rank Correlation - Nonparametric Test～～～～～～～～～～～～
cor.test(simulate.dataset.Europe$infant.mortality, simulate.dataset.Europe$life.expectancy,
         method = "spearman")

#Calculate Effect size and Power
result_data1[4,1] <- -0.04719308
result_data1[4,2] <- power.function(n = 100, effect.size = result_data1[4,1])

##########OCEANIA################
#Correlation Coefficient Test～～～～～～～～～～～～～～～～～
#install.packages("ggpubr")
library("ggpubr")
ggscatter(simulate.dataset.Oceania, x = "infant.mortality", y = "life.expectancy",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Infant deaths per 1000", ylab = "Life expectancy in years")
#Shapiro-Wilk normality test
shapiro.test(simulate.dataset.Oceania$infant.mortality)
shapiro.test(simulate.dataset.Oceania$life.expectancy)
#Visual inspection
ggqqplot(simulate.dataset.Oceania$infant.mortality, ylab = "Infant Mortality")
ggqqplot(simulate.dataset.Oceania$life.expectancy, ylab = "Life expectancy")
#Pearson Correlation Test
cor.test(simulate.dataset.Oceania$infant.mortality, simulate.dataset.Oceania$life.expectancy,
         method = "pearson")

#Spearman Rank Correlation - Nonparametric Test～～～～～～～～～～～～
cor.test(simulate.dataset.Oceania$infant.mortality, simulate.dataset.Oceania$life.expectancy,
         method = "spearman")

#Calculate Effect size and Power
result_data1[5,1] <- 0.1905645
result_data1[5,2] <- power.function(n = 100, effect.size = result_data1[5,1])

#Read Effect size and Power of all five continents
result_data1


#Scenario 2 - Different sample size
#install.packages("dplyr")
library(dplyr)
#Calculate mean and sd of two variables 
scenario2.gapminder <- example.gapminder %>%
  summarise(mean.infantmortality= mean(infant_mortality), sd.infantmortality = sd(infant_mortality), mean.lifeexpectancy = mean(life_expectancy), sd.lifeexpectancy = sd(life_expectancy))
head(scenario2.gapminder)

#convert it into dataframe
scenario2.gapminder.dataframe <- as.data.frame(scenario2.gapminder)
scenario2.gapminder.dataframe

#create a function using Monte Carlo method to simulate dataset
simulate.dataset2 <- function(n){
  N <- 1000
  sim_data2 <- matrix(data = NA, nrow = n, ncol = 2) 
  colnames(sim_data2) <- c("infant.mortality", "life.expectancy")
  simulated_means_infantmortality2 <- rep (NA, N)
  simulated_means_lifeexpectancy2 <- rep (NA, N)
  for (i in 1:N){
    sim_data2[,1] <- rnorm(n, mean = scenario1.gapminder.dataframe[1,2], sd = scenario1.gapminder.dataframe[1,3])
    sim_data2[,2] <- rnorm(n, mean = scenario1.gapminder.dataframe[1,4], sd = scenario1.gapminder.dataframe[1,5])
    simulated_means_infantmortality2[i] <- mean(sim_data2[,1])
    simulated_means_lifeexpectancy2[i] <- mean(sim_data2[,2])
  }
  hist(simulated_means_infantmortality2)
  hist(simulated_means_lifeexpectancy2)
  return(sim_data2)
}

#Simulate dataset using different sample size
set.seed(41132)
simulate.dataset.10 <- as.data.frame(simulate.dataset2(10))
simulate.dataset.100 <- as.data.frame(simulate.dataset2(100))
simulate.dataset.1000 <- as.data.frame(simulate.dataset2(1000))

#Create matrix for Effect Size and Power
result_data2 <- matrix(data = NA, nrow = 3, ncol = 2) 
colnames(result_data2) <- c("Effect Size", "Power")
rownames(result_data2) <- c("Sample Size 10", "Sample Size 100", "Sample Size 1000")

############sample size 10###################
#Correlation Coefficient Test～～～～～～～～～～～～～～～～～
#install.packages("ggpubr")
library("ggpubr")
ggscatter(simulate.dataset.10, x = "infant.mortality", y = "life.expectancy",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Infant deaths per 1000", ylab = "Life expectancy in years")
#Shapiro-Wilk normality test
shapiro.test(simulate.dataset.10$infant.mortality)
shapiro.test(simulate.dataset.10$life.expectancy)
#Visual inspection
ggqqplot(simulate.dataset.10$infant.mortality, ylab = "Infant Mortality")
ggqqplot(simulate.dataset.10$life.expectancy, ylab = "Life expectancy")
#Pearson Correlation Test
cor.test(simulate.dataset.10$infant.mortality, simulate.dataset.10$life.expectancy,
         method = "pearson")

#Spearman Rank Correlation - Nonparametric Test～～～～～～～～～～～～
cor.test(simulate.dataset.10$infant.mortality, simulate.dataset.10$life.expectancy,
         method = "spearman")

#Calculate Effect Size and Power
result_data2[1,1] <- 0.1289003
result_data2[1,2] <- power.function(n = 10, effect.size = result_data2[1,1])

############sample size 100###################
#Correlation Coefficient Test～～～～～～～～～～～～～～～～～
#install.packages("ggpubr")
library("ggpubr")
ggscatter(simulate.dataset.100, x = "infant.mortality", y = "life.expectancy",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Infant deaths per 1000", ylab = "Life expectancy in years")
#Shapiro-Wilk normality test
shapiro.test(simulate.dataset.100$infant.mortality)
shapiro.test(simulate.dataset.100$life.expectancy)
#Visual inspection
ggqqplot(simulate.dataset.100$infant.mortality, ylab = "Infant Mortality")
ggqqplot(simulate.dataset.100$life.expectancy, ylab = "Life expectancy")
#Pearson Correlation Test
cor.test(simulate.dataset.100$infant.mortality, simulate.dataset.100$life.expectancy,
         method = "pearson")

#Spearman Rank Correlation - Nonparametric Test～～～～～～～～～～～～
cor.test(simulate.dataset.100$infant.mortality, simulate.dataset.100$life.expectancy,
         method = "spearman")

#Calculate Effect Size and Power
result_data2[2,1] <- 0.1441637
result_data2[2,2] <- power.function(n = 100, effect.size = result_data2[2,1])

############sample size 1000###################
#Correlation Coefficient Test～～～～～～～～～～～～～～～～～
#install.packages("ggpubr")
library("ggpubr")
ggscatter(simulate.dataset.1000, x = "infant.mortality", y = "life.expectancy",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Infant deaths per 1000", ylab = "Life expectancy in years")
#Shapiro-Wilk normality test
shapiro.test(simulate.dataset.1000$infant.mortality)
shapiro.test(simulate.dataset.1000$life.expectancy)
#Visual inspection
ggqqplot(simulate.dataset.1000$infant.mortality, ylab = "Infant Mortality")
ggqqplot(simulate.dataset.1000$life.expectancy, ylab = "Life expectancy")
#Pearson Correlation Test
cor.test(simulate.dataset.1000$infant.mortality, simulate.dataset.1000$life.expectancy,
         method = "pearson")

#Spearman Rank Correlation - Nonparametric Test～～～～～～～～～～～～
cor.test(simulate.dataset.1000$infant.mortality, simulate.dataset.1000$life.expectancy,
         method = "spearman")

#Calculate effect Size and Power
result_data2[3,1] <- -0.00538512
result_data2[3,2] <- power.function(n = 1000, effect.size = result_data2[3,1])

#Read Effect size and power of all sample sizes
result_data2


#Scenario 3 - Different Standard Deviation
#install.packages("dplyr")
library(dplyr)
#Only calculate the mean of two variables
scenario3.gapminder <- example.gapminder %>%
  summarise(mean.infantmortality= mean(infant_mortality), mean.lifeexpectancy = mean(life_expectancy))
head(scenario3.gapminder)

#convert it into dataframe
scenario3.gapminder.dataframe <- as.data.frame(scenario3.gapminder)
scenario3.gapminder.dataframe

#create a function using Monte Carlo method to simulate dataset
simulate.dataset3 <- function(sd){
  N <- 1000
  sim_data3 <- matrix(data = NA, nrow = 100, ncol = 2) 
  colnames(sim_data3) <- c("infant.mortality", "life.expectancy")
  simulated_means_infantmortality3 <- rep (NA, N)
  simulated_means_lifeexpectancy3 <- rep (NA, N)
  for (i in 1:N){
    sim_data3[,1] <- rnorm(n = 100, mean = scenario1.gapminder.dataframe[1,2], sd)
    sim_data3[,2] <- rnorm(n = 100, mean = scenario1.gapminder.dataframe[1,4], sd)
    simulated_means_infantmortality3[i] <- mean(sim_data3[,1])
    simulated_means_lifeexpectancy3[i] <- mean(sim_data3[,2])
  }
  hist(simulated_means_infantmortality3)
  hist(simulated_means_lifeexpectancy3)
  return(sim_data3)
}

#simulate dataset using different sd
set.seed(41133)
simulate.dataset.sd2 <- as.data.frame(simulate.dataset3(2))
simulate.dataset.sd5 <- as.data.frame(simulate.dataset3(5))
simulate.dataset.sd10 <- as.data.frame(simulate.dataset3(10))

#create matrix for Effect Size and Power
result_data3 <- matrix(data = NA, nrow = 3, ncol = 2) 
colnames(result_data3) <- c("Effect Size", "Power")
rownames(result_data3) <- c("sd 2", "sd 5", "sd 10")

############sd 2###################
#Correlation Coefficient Test～～～～～～～～～～～～～～～～～
#install.packages("ggpubr")
library("ggpubr")
ggscatter(simulate.dataset.sd2, x = "infant.mortality", y = "life.expectancy",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Infant deaths per 1000", ylab = "Life expectancy in years")
#Shapiro-Wilk normality test
shapiro.test(simulate.dataset.sd2$infant.mortality)
shapiro.test(simulate.dataset.sd2$life.expectancy)
#Visual inspection
ggqqplot(simulate.dataset.sd2$infant.mortality, ylab = "Infant Mortality")
ggqqplot(simulate.dataset.sd2$life.expectancy, ylab = "Life expectancy")
#Pearson Correlation Test
cor.test(simulate.dataset.sd2$infant.mortality, simulate.dataset.sd2$life.expectancy,
         method = "pearson")

#Spearman Rank Correlation - Nonparametric Test～～～～～～～～～～～～
cor.test(simulate.dataset.sd2$infant.mortality, simulate.dataset.sd2$life.expectancy,
         method = "spearman")

#Calculate effect Size and Power
result_data3[1,1] <- -0.01193447
result_data3[1,2] <- power.function(n = 100, effect.size = result_data3[1,1])

############sd 5###################
#Correlation Coefficient Test～～～～～～～～～～～～～～～～～
#install.packages("ggpubr")
library("ggpubr")
ggscatter(simulate.dataset.sd5, x = "infant.mortality", y = "life.expectancy",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Infant deaths per 1000", ylab = "Life expectancy in years")
#Shapiro-Wilk normality test
shapiro.test(simulate.dataset.sd5$infant.mortality)
shapiro.test(simulate.dataset.sd5$life.expectancy)
#Visual inspection
ggqqplot(simulate.dataset.sd5$infant.mortality, ylab = "Infant Mortality")
ggqqplot(simulate.dataset.sd5$life.expectancy, ylab = "Life expectancy")
#Pearson Correlation Test
cor.test(simulate.dataset.sd5$infant.mortality, simulate.dataset.sd5$life.expectancy,
         method = "pearson")

#Spearman Rank Correlation - Nonparametric Test～～～～～～～～～～～～
cor.test(simulate.dataset.sd5$infant.mortality, simulate.dataset.sd5$life.expectancy,
         method = "spearman")


#Calculate effect Size and Power
result_data3[2,1] <- -0.007452775
result_data3[2,2] <- power.function(n = 100, effect.size = result_data3[2,1])


############sd 10###################
#Correlation Coefficient Test～～～～～～～～～～～～～～～～～
#install.packages("ggpubr")
library("ggpubr")
ggscatter(simulate.dataset.sd10, x = "infant.mortality", y = "life.expectancy",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Infant deaths per 1000", ylab = "Life expectancy in years")
#Shapiro-Wilk normality test
shapiro.test(simulate.dataset.sd10$infant.mortality)
shapiro.test(simulate.dataset.sd10$life.expectancy)
#Visual inspection 
ggqqplot(simulate.dataset.sd10$infant.mortality, ylab = "Infant Mortality")
ggqqplot(simulate.dataset.sd10$life.expectancy, ylab = "Life expectancy")
#Pearson Correlation Test
cor.test(simulate.dataset.sd10$infant.mortality, simulate.dataset.sd10$life.expectancy,
         method = "pearson")

#Spearman Rank Correlation - Nonparametric Test～～～～～～～～～～～～
cor.test(simulate.dataset.sd10$infant.mortality, simulate.dataset.sd10$life.expectancy,
         method = "spearman")

#Calculate effect Size and Power
result_data3[3,1] <- 0.137056
result_data3[3,2] <- power.function(n = 100, effect.size = result_data3[3,1])

#read the result
result_data3

