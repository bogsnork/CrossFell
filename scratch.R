#scratch


# oneway function from pckg userfriendlyscience ----
library(userfriendlyscience)
oneway

function (y, x, posthoc = NULL, means = FALSE, fullDescribe = FALSE, 
          levene = FALSE, plot = FALSE, digits = 2, omegasq = TRUE, 
          etasq = TRUE, corrections = FALSE, pvalueDigits = 3, t = FALSE, 
          conf.level = 0.95, silent = FALSE) 
{
  res <- list(input = as.list(environment()))
  res$input$x.name <- extractVarName(deparse(substitute(x)))
  res$input$y.name <- extractVarName(deparse(substitute(y)))
  if (!is.numeric(y)) {
    stop("The y variable (", res$input$y.name, ") is not a numeric ", 
         "vector! Note that in analysis of variance, the 'y' variable ", 
         "must have at least the interval level of measurement!")
  }
  if (!is.factor(x)) {
    if (!silent) {
      warning("### Warning: the x variable (", res$input$x.name, 
              ") is not a ", "factor! Converting it myself - but note that variables in R have ", 
              "data types, and it's advisable to set these adequately (use for ", 
              "example 'as.factor'; see '?as.factor' for help)!")
    }
    res$input$x.raw <- x
    x <- as.factor(x)
    res$input$x <- x
  }
  assign(res$input$x.name, x)
  assign(res$input$y.name, y)
  res$intermediate <- list()
  res$intermediate$aov <- aov(formula(paste0(res$input$y.name, 
                                             " ~ ", res$input$x.name)))
  res$intermediate$Anova <- Anova(res$intermediate$aov, type = 3)
  if (!is.null(posthoc)) {
    if (tolower(posthoc) == "tukey") {
      res$intermediate$posthoc <- TukeyHSD(res$intermediate$aov)
    }
    else if (tolower(posthoc) == "games-howell") {
      res$intermediate$posthocTGH <- posthocTGH(y = y, 
                                                x = x, method = "Games-Howell")
      res$intermediate$posthoc <- res$intermediate$posthocTGH$output$games.howell
    }
    else {
      res$intermediate$posthoc <- pairwise.t.test(x = y, 
                                                  g = x, p.adjust.method = posthoc)
    }
  }
  if (means) {
    res$intermediate$means <- describeBy(y, x)
    tmpAttributes <- attributes(res$intermediate$means)
    res$intermediate$means <- lapply(res$intermediate$means, 
                                     function(x) {
                                       class(x) <- "data.frame"
                                       rownames(x)[1] <- " "
                                       return(x[, colnames(x) != "vars"])
                                     })
    if (!fullDescribe) {
      res$intermediate$means <- lapply(res$intermediate$means, 
                                       function(x) {
                                         return(x[, colnames(x) %in% c("n", "mean", 
                                                                       "sd", "se", "median")])
                                       })
    }
    if (t) {
      res$intermediate$means <- lapply(res$intermediate$means, 
                                       t)
    }
    attributes(res$intermediate$means) <- tmpAttributes
  }
  if (levene) {
    res$intermediate$leveneTest <- leveneTest(y, group = x, 
                                              center = mean)
  }
  res$intermediate$etasq <- computeEffectSize_etasq(var1 = x, 
                                                    var2 = y, conf.level = conf.level)
  res$intermediate$confIntOmegaSq <- confIntOmegaSq(var1 = x, 
                                                    var2 = y, conf.level = conf.level)
  res$output <- list(etasq = res$intermediate$Anova$`Sum Sq`[2]/sum(res$intermediate$Anova$`Sum Sq`[2:3]), 
                     etasq.ci = res$intermediate$etasq$ci, omegasq = res$intermediate$confIntOmegaSq$output$es, 
                     omegasq.ci = res$intermediate$confIntOmegaSq$output$ci)
  res$output$dat <- data.frame(SS = res$intermediate$Anova$`Sum Sq`[2:3], 
                               Df = res$intermediate$Anova$Df[2:3])
  res$output$dat$MS <- res$output$dat$SS/res$output$dat$Df
  res$output$dat[1, "F"] <- res$intermediate$Anova$F[2]
  res$output$dat[1, "p"] <- res$intermediate$Anova$`Pr(>F)`[2]
  row.names(res$output$dat) <- c("Between groups (error + effect)", 
                                 "Within groups (error only)")
  if (corrections) {
    res$intermediate$welch <- oneway.test(formula(paste0(res$input$y.name, 
                                                         " ~ ", res$input$x.name)))
    SSm <- res$output$dat["Between groups (error + effect)", 
                          "SS"]
    tmpDat <- na.omit(data.frame(x = x, y = y))
    groupVariances <- as.numeric(by(tmpDat$y, tmpDat$x, var))
    groupSizes <- as.numeric(by(tmpDat$y, tmpDat$x, length))
    denominator <- sum(groupVariances * (1 - (groupSizes/sum(groupSizes))))
    res$intermediate$brown.forsythe <- list()
    res$intermediate$brown.forsythe$F <- SSm/denominator
    res$intermediate$brown.forsythe$Df1 <- length(groupSizes) - 
      1
    cValues <- ((1 - (groupSizes/sum(groupSizes))) * groupVariances)/(sum((1 - 
                                                                             (groupSizes/sum(groupSizes))) * groupVariances))
    inverseDf2 <- sum(cValues^2/(groupSizes - 1))
    res$intermediate$brown.forsythe$Df2 <- 1/inverseDf2
    res$intermediate$brown.forsythe$p <- pf(res$intermediate$brown.forsythe$F, 
                                            res$intermediate$brown.forsythe$Df1, res$intermediate$brown.forsythe$Df2, 
                                            lower.tail = FALSE)
  }
  if (plot) {
    res$intermediate$dat <- data.frame(x, y)
    names(res$intermediate$dat) <- c(res$input$x.name, res$input$y.name)
    res$output$plot <- dlvPlot(res$intermediate$dat, x = res$input$x.name, 
                               y = res$input$y.name)$plot + ggtitle(paste0(res$input$x.name, 
                                                                           " and ", res$input$y.name))
  }
  class(res) <- "oneway"
  return(res)
}
<bytecode: 0x00000000202f3238>
  <environment: namespace:userfriendlyscience>
  
# run tests on multiple subsets
  
#all taxa
bartlett.test(height ~ year, data = ht.data)

#Sheep's fescue
bartlett.test(height ~ year, data = filter(ht.data, taxon == "Fo"))

#all graminoids
bartlett.test(height ~ year, data = filter(ht.data, taxon == "grm"))

#Mosses and lichen
bartlett.test(height ~ year, data = filter(ht.data, taxon == "moss"))

#bilberry
bartlett.test(height ~ year, data = filter(ht.data, taxon == "Vm"))


group_by(ht.data, taxon)  

bartlett.test(height ~ year, data = group_by(ht.data, taxon))
bartlett.test(height ~ year, data = group_by(ht.data, taxon))

library(purrr)
tmp.bart <- by(ht.data, ht.data$taxon, function(x) bartlett.test(height ~ year, data = x))
bart.df          <- data.frame(cbind(taxon = names(tmp.bart)))
bart.df$statistic  <- tmp.bart %>% map_dbl("statistic") 
bart.df$df       <- tmp.bart %>% map_dbl("parameter")
bart.df$p.value     <- tmp.bart %>% map_dbl("p.value") 
bart.df$sig.sym  <- symnum(bart.df$p.value, 
                             cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                             symbols = c("***", "**", "*", ""))
bart.df$sig.txt  <- if_else(condition = bart.df$p.value < 0.001, 
                            true = "p<0.001", 
                            false = paste0("p=", round(bart.df$p.value, digits=3)))
bart.df$method   <- tmp.bart %>% map_chr("method")
bart.df$formula   <- tmp.bart %>% map_chr("data.name")



