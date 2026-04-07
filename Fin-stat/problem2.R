### -------------------- Clear all Outputs and Terminal of R Studio ----------------
## This for safety and to avoid variables conflict
rm(list = ls())
graphics.off()
cat("\014")

### ---------------------- Installing Packages Required ---------------------------
options (repos = "https://cran.rstudio.com")
packages <- c ("ggplot2","readxl","plotly","stargazer", "regclass","leaps", "car")
installed_packages <- packages %in% rownames(installed.packages())

# Install packages, if not yet installed
if (any(installed_packages == FALSE)) {
  install.packages ( packages [!installed_packages])
}
invisible (lapply(packages,library,character.only = TRUE))   # Package Loading

### ------------------------------ Loading Data -----------------------------------
## To get current directory
getwd()
obs <- read_excel("Problem2set2.xlsx")

### Number of Columns
ncol(obs)
### Number of rows
n_obs=nrow(obs)

### Assigning Variables to Data
head(obs)
Y_obs <- obs[, 1]$'Yvariable'
factors <- obs[, 2:6]

FactorA <- factors[, 1]$'FactorA'
FactorB <- factors[, 2]$'FactorB'
FactorC <- factors[, 3]$'FactorC'
FactorD <- factors[, 4]$'FactorD'
FactorE <- factors[, 5]$'FactorE'

### -------------------------- Shapiro-Wilk Normality Test ------------------------
tests <- list(
  "Factor A" = shapiro.test(FactorA),
  "Factor B" = shapiro.test(FactorB),
  "Factor C" = shapiro.test(FactorC),
  "Factor D" = shapiro.test(FactorD),
  "Factor E" = shapiro.test(FactorE)
)
tests

### ---------------------------- Histogram Plots ---------------------------------

## Bins are choosed based on Freedman–Diaconis (FD) rule, for robustness towards to outliners

fig <- plot_ly(alpha = 0.6)
fig <- fig %>% add_histogram(x = FactorA, name = 'Factor A')
fig <- fig %>% add_histogram(x = FactorB, name = 'Factor B')
fig <- fig %>% add_histogram(x = FactorC, name = 'Factor C')
fig <- fig %>% add_histogram(x = FactorD, name = 'Factor D')
fig <- fig %>% add_histogram(x = FactorE, name = 'Factor E')
fig <- fig %>% layout(barmode = "overlay")
fig

### ------------------------------ Factor A Histogram ----------------------------
hist(FactorA,probability = TRUE,breaks = "FD",main = "Factor A",xlab = "Value")
lines(density(FactorA), lwd = 2)

x <- seq(min(FactorA), max(FactorA), length.out = 200)
lines(x,dnorm(x, mean(FactorA), sd(FactorA)),col = "red",lwd = 2)

legend("topright",legend = c("Kernel Density Estimate", "Normal Fit"),
       lwd = c(2, 2), col = c("black", "red"), bty = "n")

### ------------------------------ Factor B Histogram ----------------------------
hist(FactorB,probability = TRUE,breaks = "FD",main = "Factor B",xlab = "Value")
lines(density(FactorB), lwd = 2)

x <- seq(min(FactorB), max(FactorB), length.out = 200)
lines(x,dnorm(x, mean(FactorB), sd(FactorB)),col = "orange",lwd = 2)

legend("topright",legend = c("Kernel Density Estimate", "Normal Fit"),
       lwd = c(2, 2),col = c("black", "orange"),bty = "n")

### ------------------------------ Factor C Histogram ----------------------------
hist(FactorC,probability = TRUE,breaks = "FD",main = "Factor C",xlab = "Value")
lines(density(FactorC), lwd = 2)

x <- seq(min(FactorC), max(FactorC), length.out = 200)
lines(x,dnorm(x, mean(FactorC), sd(FactorC)),col = "forestgreen",lwd = 2)

legend("topright",legend = c("Kernel Density Estimate", "Normal Fit"),lwd = c(2, 2),
       col = c("black", "forestgreen"), bty = "n")

### ------------------------------ Factor D Histogram ----------------------------
hist(FactorD,probability = TRUE,breaks = "FD",main = "Factor D",xlab = "Value")
lines(density(FactorD), lwd = 2)

x <- seq(min(FactorD), max(FactorD), length.out = 200)
lines(x,dnorm(x, mean(FactorD), sd(FactorD)),col = "blue",lwd = 2)

legend("topright",legend = c("Kernel Density Estimate", "Normal Fit"),lwd = c(2, 2),
       col = c("black", "blue"),bty = "n")

### ------------------------------ Factor E Histogram ----------------------------
hist(FactorE,probability = TRUE,breaks = "FD",main = "Factor E",xlab = "Value")
lines(density(FactorE), lwd = 2)

x <- seq(min(FactorE), max(FactorE), length.out = 200)
lines(x,dnorm(x, mean(FactorE), sd(FactorE)),
      col = "purple",lwd = 2)

legend("topright",legend = c("Kernel Density Estimate", "Normal Fit"),
       lwd = c(2, 2),col = c("black", "purple"),bty = "n")

###---------------------------- Quantile-Quantile Plots --------------------------
qq_plot <- ggplot() +
  stat_qq(aes(sample = FactorA, color = "FactorA")) +
  stat_qq(aes(sample = FactorB, color = "FactorB")) +
  stat_qq(aes(sample = FactorC, color = "FactorC")) +
  stat_qq(aes(sample = FactorD, color = "FactorD")) +
  stat_qq(aes(sample = FactorE, color = "FactorE")) +
  stat_qq_line(aes(sample = FactorA, color = "FactorA")) +
  stat_qq_line(aes(sample = FactorB, color = "FactorB")) +
  stat_qq_line(aes(sample = FactorC, color = "FactorC")) +
  stat_qq_line(aes(sample = FactorD, color = "FactorD")) +
  stat_qq_line(aes(sample = FactorE, color = "FactorE")) +
  labs(
    x = "Theoretical Quantiles",
    y = "Sample Quantiles",
    color = "Factor",
    title = "Q-Q Plot of Factors"
  ) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  theme_light()
qq_plot

### ------------------------------ QQ Plot Factor A -------------------------------
qqplotA <- ggplot(data.frame(FactorA), aes(sample = FactorA)) + stat_qq() +
  stat_qq_line(color = "red", linewidth = 1) +
  labs(x = "Theoretical Quantiles",y = "Sample Quantiles",title = "Factor A") +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  theme_light()
qqplotA

### ------------------------------ QQ Plot Factor B -------------------------------
qqplotB <- ggplot(data.frame(FactorB), aes(sample = FactorB)) +stat_qq() +
  stat_qq_line(color = "orange", linewidth = 1) +
  labs(x = "Theoretical Quantiles",y = "Sample Quantiles",title = "Factor B") +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  theme_light()
qqplotB

### ------------------------------ QQ Plot Factor C -------------------------------
qqplotC <- ggplot(data.frame(FactorC), aes(sample = FactorC)) + stat_qq() +
  stat_qq_line(color = "forestgreen", linewidth = 1) +
  labs(x = "Theoretical Quantiles",y = "Sample Quantiles",title = "Factor C") +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  theme_light()
qqplotC

### ------------------------------ QQ Plot Factor D -------------------------------
qqplotD <- ggplot(data.frame(FactorD), aes(sample = FactorD)) + stat_qq() +
  stat_qq_line(color = "blue", linewidth = 1) +
  labs(x = "Theoretical Quantiles",y = "Sample Quantiles",title = "Factor D") +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  theme_light()
qqplotD

### ------------------------------ QQ Plot Factor E -------------------------------
qqplotE <- ggplot(data.frame(FactorE), aes(sample = FactorE)) +
  stat_qq() +
  stat_qq_line(color = "purple", linewidth = 1) +
  labs(x = "Theoretical Quantiles",y = "Sample Quantiles",title = "Factor E") +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  theme_light()
qqplotE


### ---------------------------- Simple Linear Regression Models ------------------

# Simple Linear Regression For Factor A
regA <- lm(Y_obs~FactorA)
summary(regA)

# Simple Linear Regression For Factor B
regB <- lm(Y_obs~FactorB)
summary(regB)

# Simple Linear Regression For Factor C
regC <- lm(Y_obs~FactorC)
summary(regC)

# Simple Linear Regression For Factor D
regD <- lm(Y_obs~FactorD)
summary(regD)

# Simple Linear Regression For Factor E
regE <- lm(Y_obs~FactorE)
summary(regE)


### ----------------------- Summary to LaTeX table ------------------------------

### Using stargazer package, we are getting the simple regression model summary as LaTeX table in the output. 
### **Un-comment** and Change the parameter regA for different factors.
### Regression summary to LaTeX table

# stargazer(
#   regE,
#   type = "latex",
#   title = "Regression Results on Factor E",
#   label = "tab:regE_summ",
#   single.row = TRUE,
#   dep.var.labels = "Yvariable",
#   covariate.labels = c("Factor E")
# )

### ------------------------ Multi-Linear Regression Model -----------------------

multireg <- lm(Y_obs~FactorA+FactorB+FactorC+FactorD+FactorE)
summary(multireg)
anova(multireg)

# stargazer(multireg, type = "latex",
#           title = "Multi Regression Results",
#           label = "tab:multireg_summ",
#           single.row = TRUE)

### ------------------------- Multi-Collinearity Test on Factors -----------------

vif(multireg)

### ------------------------- Bayes Information Criterion ------------------------

BIC(multireg)

#### Choosing the best model using, step function, a stepwise model selection f
#### or regression models. k=2 for Akaike Information Criterion. 
### direction= "both" meaning allowing both addition and removal of factors based on 
### the score of model

best_model <- step(multireg, direction="both", k=log(n_obs))
summary(best_model)
BIC(best_model)

# stargazer(best_model, type = "latex",
#           title = "Multi Regression Results of Best Model",
#           label = "tab:bestmodel_summ",
#           single.row = TRUE)


### ------------------- Best Model based on Adjusted R\^2- Squared ----------------

regfit <- regsubsets(
  Y_obs ~ FactorA + FactorB + FactorC + FactorD + FactorE, data=obs,nvmax = n_obs)
summary(regfit)


adjr2_plot <- plot(regfit, scale="adjr2", col=gray(seq(0, 0.9, length = 10)))
title(main="Adjusted R²", font.main=2)
BIC_plot <- plot(regfit, scale="bic")
title(main="BIC", font.main=2)


### ----------------- Prediction using the Multi-Linear Regression Model ----------

# Creating a data frame of new factors and calling the column "new_factors".
###This naming of the column is important whenn using predict afterwards. 
new_factors <- data.frame(
  FactorA = 34,
  FactorB = 76,
  FactorC = -25,
  FactorD = -55,
  FactorE = 42
)

# Predicting the new value of the Yvaribale
new_Y_obs <-predict(best_model,newdata=new_factors)
new_Y_obs

