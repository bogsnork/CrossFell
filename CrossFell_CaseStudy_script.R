#CrossFell_CaseStudy_script.R

#This is the script file which accompanies the Cross Fell Case Study: 

## How to do it (in R)

### Packages----
# Import the required packages into R
library(tidyverse) # Loads the following core packages: 
# ggplot2, for data visualisation
# dplyr, for data manipulation
# tidyr, for data tidying
# readr, for data import
# purrr, for functional programming
# tibble, for tibbles, a simpler type of data frame
library(knitr)    # turns rmarkdown document to html, pdf or word
library(ggpubr)   # helps make publication-ready graphs
library(broom)    # cleans up code output from common functions
library(nortest)  # functions for testing normality
library(lattice)  # for faceting graphs

### Load data----
# reads an excel file and writes it to a new object: ht.data
ht.data <- read.csv("ht.data.csv", header = TRUE)

### Inspect data----
# look at the data table
View(ht.data)
# inspect data classes
str(ht.data) # shows structure of data
# turn 'year' into a factor
ht.data$year <- as.factor(ht.data$year) 

### Explore data----
#summarise the data
summary(ht.data)
#graph the data
plot(ht.data) #R selects plot type (usually chooses well)

# Calculate means using package `dplyr`
ht.means <-       # create a new object
  ht.data %>%     # take the height data
  group_by(year) %>% # group it by year
  summarise(         # calculate summary statistics for each group
    mean = mean(height),     # its mean
    median = median(height), # its median
    sd = sd(height),         # the standard deviation
    n = length(height)       # the number of observations
  ) 
ht.means  #print the result

### Check assumptions for statistical tests----
#### Plot data

##### Boxplots

#Make a series of boxplot grouped by a factor
boxplot(height ~ year, data = ht.data) 

##### Histograms
# histograms of height by year in base R
par(mfrow=c(3,2))
hist(ht.data$height[which(ht.data$year == "2003")], main = "2003", xlab = "height")
hist(ht.data$height[which(ht.data$year == "2005")], main = "2005", xlab = "height")
hist(ht.data$height[which(ht.data$year == "2008")], main = "2008", xlab = "height")
hist(ht.data$height[which(ht.data$year == "2010")], main = "2010", xlab = "height")
hist(ht.data$height[which(ht.data$year == "2013")], main = "2013", xlab = "height")
par(mfrow=c(1,1))

# histograms of height by year - ggplot
ggplot(ht.data, aes(x= height)) +
  geom_histogram() +
  facet_wrap(~year)

# histograms of height by year - lattice
histogram(~height|year, data = ht.data, type = "count")

##### Density plots

#density plot of height by year - ggplot
ggplot(ht.data, aes(x= height)) +
  geom_density(aes(colour = year)) 

#density histogram and smoother of height by year - base R
par(mfrow=c(3,2))
plot(density(ht.data$height[which(ht.data$year == "2003")]),
     main = "2003", xlab = "height", xlim = c(0,10))
plot(density(ht.data$height[which(ht.data$year == "2005")]),
     main = "2005", xlab = "height", xlim = c(0,10))
plot(density(ht.data$height[which(ht.data$year == "2008")]), 
     main = "2008", xlab = "height", xlim = c(0,10))
plot(density(ht.data$height[which(ht.data$year == "2010")]), 
     main = "2010", xlab = "height", xlim = c(0,10))
plot(density(ht.data$height[which(ht.data$year == "2013")]), 
     main = "2013", xlab = "height", xlim = c(0,10))
par(mfrow=c(1,1))

#density histogram and smoother of height by year - ggplot
ggplot(ht.data, aes(x= height)) +
  geom_histogram(aes(y=..density..), fill = "grey") + 
  geom_density(colour = "red") +
  facet_wrap(~year)

#density plots and 'rug marks' of height by year - lattice
densityplot(~height|year, data = ht.data)

#### Test for normality

# Anderson Darling test for the composite hypothesis of normality (package: nortest)
ad.test(x = ht.data$height)
#if p > 0.05 then the data is normally distributed

#run Anderson Darling test for normality on each year in turn - base R
norm.2003 <- ad.test(ht.data$height[which(ht.data$year == "2003")])
norm.2003
norm.2005 <- ad.test(ht.data$height[which(ht.data$year == "2005")])
norm.2005
norm.2008 <- ad.test(ht.data$height[which(ht.data$year == "2008")])
norm.2008
norm.2010 <- ad.test(ht.data$height[which(ht.data$year == "2010")])
norm.2010
norm.2013 <- ad.test(ht.data$height[which(ht.data$year == "2013")])
norm.2013

# put results in a dataframe - base R
norm <- data.frame(year = c(2003, 2005, 2008, 2010, 2013), 
                   statistic = c(norm.2003$statistic, 
                                 norm.2005$statistic, 
                                 norm.2008$statistic, 
                                 norm.2010$statistic, 
                                 norm.2013$statistic), 
                   p.value = c(norm.2003$p.value,
                               norm.2005$p.value, 
                               norm.2008$p.value, 
                               norm.2010$p.value, 
                               norm.2013$p.value))

#run Anderson Darling test for normality on each year in turn - dplyr and broom
norm <-  # create a new object to hold our results
  ht.data %>% # start with the height data
  group_by(year) %>% # group it by year
  do(                # do the following functions on each group element
    tidy(            # extracts test results into a table
      ad.test(.$height) #Anderson Darling test
    )
  ) 
norm

##### Try exponential transform
#run Anderson Darling test for normality on each year in turn - dplyr and broom
norm <-  # create a new object to hold our results
  ht.data %>% # start with the height data
  group_by(year) %>% # group it by year
  do(                # do the following functions on each group element
    tidy(            # extracts test results into a table
      ad.test(exp(.$height)) #Anderson Darling test on log transformed data
    )
  ) 
norm


#### Test for homogeneity of variances

#run Bartlett test for homogeneity of variances 
bart <- bartlett.test(height ~ year, data = ht.data)
bart

### Compare means----

#### Kruskal-Wallis rank sum test

krusk <- kruskal.test(height ~ year, data = ht.data)
krusk

#### Pairwise Wilcoxson / Mann-Whitney U tests
pairwise.wilcox.test(x = ht.data$height, g = ht.data$year, paired = FALSE)

#We can graph this using the package `ggpubr`: 
 
#run test again using function ggpubr::compare_means() to prepare brackets for graphic output. 
wilc.pairw <- compare_means(formula = height ~ year, 
                            data = ht.data, 
                            method = "wilcox.test", paired = FALSE)
wilc.pairw <- filter(wilc.pairw, p < 0.05) #select only significant results.

#generate list of brackets
my_comparisons <- list() 
for (i in 1:nrow(wilc.pairw)) {
  my_comparisons[[i]] <- c(wilc.pairw$group1[i], wilc.pairw$group2[i])
}

#prepare plot
p <- ggplot(ht.data, aes(x = year, y = height)) +
  geom_boxplot()

# Add pairwise comparisons p-value to plot
p + stat_compare_means(comparisons = my_comparisons, 
                       label = "p.signif", 
                       hide.ns = FALSE)

