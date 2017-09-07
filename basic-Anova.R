# basic workflow for ANOVA
#forked from https://gist.github.com/benmarwick/6262134 

# some fake example data
x <- runif(100) # The measurement variable (100 random numbers)
y <- sample(rep(letters[1:5], each=20)) # The catagorical variable (five groups)
# make a data frame of the x and y vectors
df <- data.frame(measure = x, group = y)
# inspec the data frame to make sure it looks right
str(df)

# quick plot to see the data
plot(measure ~ group, data = df,
     ylab="group", main="Boxplots of example data")


# in looking at the plots we are hoping not to see
# 1. Outliers — these will be apparent as separated 
# points on the boxplots. The default is to extend the
# whiskers of the boxplot no more than one and half 
# times the interquartiles range from the quartiles.
# Any points further away than this are plotted separately.
# 2. Skewness — this will be apparent from an asymmetrical
# form for the boxes.
# 3. Unequal variance — this will be apparent from clearly 
# unequal box sizes. Some care is required
# because often there is very little data be 
# used in the construction of the boxplots and so even when the
# variances truly are equal in the groups, we can 
# expect a great deal of variability


# To proceed with the verification ANOVA, we must 
# first verify the homoskedasticity (ie test for 
# homogeneity of variances). The software R provides 
# two tests: the Bartlett test, and the Fligner-Killeen test.

# Bartlett Test of Homogeneity of Variances
bartlett.test(measure ~ group, data = df)
# if the p-value is < 0.05 then data not suitable for ANOVA

# The Fligner-Killeen test
with (df, fligner.test(measure, group))
# if the p-value is < 0.05 then data not suitable for ANOVA

# If the data fails these tests, try 

# Kruskal Wallis test of ranks
with(df, kruskal.test(measure ~ group))

# One-Way Permutation Test based on 9999 Monte-Carlo 
# resamplings. 
library(coin)
oneway_test(measure ~ group, data = df,
            distribution = approximate(B = 9999))

# Having verified the homoskedasticity of the
# groups, we can proceed with the ANOVA model.

# First organize the values, fitting the model:


fit = lm(formula = measure ~ group, data = df)


# Then we analyze the ANOVA model:


anova(fit)

# The output of the function is a classical ANOVA table with the following data:
#   Df = degree of freedom
#   Sum Sq = deviance (within groups, and residual)
#   Sq = variance (within groups, and residual)
#   F value = the value of the Fisher statistic test, so computed (variance within groups) / (variance residual)
#   Pr(>F) = p-value

# If p-value > 0.05, we accept the null hypothesis H0: 
# the group means are statistically equal. 

# If the group means are NOT equal, we can compute
# further to see which groups differ...

# plot the ANOVA
library(granovaGG)
granovagg.1w(df$measure, df$group)
?granovagg.1w # read the docs to deciper this plot...

# Tukey Honestly Significant Differences
# calculates post hoc comparisons on each factor in the model. 
aovs <- aov(measure ~ group, data = df)
TukeyHSD(aovs) # look for 'p adj' to be < 0.05
# plot
plot(TukeyHSD(aovs))

# pairwise t-test with bonferroni adjustment
with(df, pairwise.t.test(measure, group, p.adjust.method="bonferroni"))