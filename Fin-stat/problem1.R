
### ---------------- Clear all Outputs and Terminal of R Studio ------------------
## This for safety and to avoid variables conflict
rm(list = ls())
graphics.off()
cat("\014")

### --------------------- Installing Packages Required ---------------------------
options (repos = "https://cran.rstudio.com")
packages <- c ( "ggplot2","readxl","plotly")
installed_packages <- packages %in% rownames(installed.packages())

# Install packages,if  not yet installed
if (any(installed_packages == FALSE)) {
  install.packages ( packages [!installed_packages])
}
invisible (lapply(packages,library,character.only = TRUE))   # Package Loading


### ----------------- 1. Stocks prices and Log returns on Stock ------------------
### ------------------------------------------------------------------------------

### Loading Data
getwd()
prices <- read_excel("Problem1set2.xlsx")

head(prices)
print(prices[,1])
print(prices[,2])         # Print out the vector of  "prices" of stock A

### Nnumber of Columns
ncol(prices)
###number of rows (that is our n in the lectures)
n_obs = nrow(prices)

##### We use the function summary()
summary(prices$"A")
mean(prices$"A")
#### The adjusted empirical variance for stock A 
var(prices$"A")

###------------------------ Visualization using ggplot ---------------------------
p <- ggplot(prices, aes(x=Date, y=A)) +
  geom_area(fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2") +
  labs(x = "Consecutive Days",y = "Stock Prices",title = "Stock A daily prices") 
p   ## Displaying Plot

### ----------------------------- Returns on Stock -------------------------------
#We create a numeric vector of size n-1
returnsA<-vector(mode = "numeric", length = n_obs -1)
#then, we use a "for loop" in R
for(i in 1:n_obs-1) returnsA[i]<-(prices$`A`[i+1]-prices$`A`[i])/prices$`A`[i]
# Plot for returns
plot(returnsA, type='l', xlab="Days", ylab="Returns on Stock")

### ------------------------ Logarithm Returns on Stock A ------------------------
log_returns <- diff(log(prices$`A`), lag=1)

# Plot for Log returns
df <- data.frame(Date = 1:length(log_returns),LogReturn = log_returns)
q <- ggplot(df, aes(x=Date, y=log_returns)) + 
  geom_line(color="steelblue") +
  labs(x = "Days", y = "Log returns on Stock A",title = "Log Returns")
q 

### ----------------------------- Histogram --------------------------------------
hist(log_returns, probability = TRUE, breaks = "FD",
     main = "Log Returns", xlab = "Value", ylab = "Density")
lines(density(log_returns), lwd = 2)

x <- seq(min(log_returns), max(log_returns), length.out = 200)
lines(x, dnorm(x, mean(log_returns), sd(log_returns)), col = "forestgreen",lwd = 2)

legend("topright", legend = c("Kernel Density Estimate", "Normal Fit"),
       lwd = c(2, 2), col = c("black", "forestgreen"), bty = "n")

# Split plotting area, 1 row 2 columns
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))

first_half <- log_returns[1:floor(n_obs/2)]
second_half <- log_returns[(floor(n_obs/2)):n_obs-1]

# First Half
hist(first_half,probability = TRUE,breaks = 20,
     main = "Log Returns (First Half)", xlab = "Value", ylab = "Density",
     ylim = c(0, max(density(first_half)$y, dnorm(first_half, mean(first_half),
     sd(first_half)))))

lines(density(first_half), lwd = 2, col = "black")
xgrid1 <- seq(min(first_half), max(first_half), length.out = 200)
lines(xgrid1, dnorm(xgrid1, mean(first_half), sd(first_half)), col = "red", lwd = 2)

# Second half
hist(second_half,probability = TRUE,breaks = 20,
     main = "Log Returns (Second Half)",xlab = "Value",ylab = "Density",
     ylim = c(0, max(density(second_half)$y, dnorm(second_half, mean(second_half), 
     sd(second_half)))))

lines(density(second_half), lwd = 2, col = "black")
xgrid2 <- seq(min(second_half), max(second_half), length.out = 200)
lines(xgrid2, dnorm(xgrid2, mean(second_half), sd(second_half)), col = "red", lwd = 2)
legend("topright",legend = c("Kernel Density", "Normal Fit"),col = c("black", "red"),
       lwd = 2,bty = "n")

par(mfrow = c(1,1)) ## Restoring the plotting to normal

### ------------------ Kolmogorov-Sirmnov Test for Same distribution -------------

## To check if both the data sets are coming from same distribution
ks.test(first_half, second_half)

### -------------------------- Quantile-Quantile Plots Plot -----------------------

qqplot <- ggplot(data.frame(log_returns), aes(sample = log_returns)) +
  stat_qq(shape = "*", size = 6) +
  stat_qq_line(color = "red", linewidth = 0.75) +
  labs(x = "Theoretical Quantiles",y = "Sample Quantiles",
       title = "Q-Q Plot of Log Returns") +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10) +
  theme_light()
qqplot

### --------------------------------- Meucci Visual Check ------------------------
### The shatter plot should be circular, if the data is i.i.d random variables

lA1<-vector(mode = "numeric", length = n_obs-1)
for(i in 1:n_obs-1) lA1[i]<-log_returns[i]

lA2<-vector(mode = "numeric", length = n_obs-1)
for(i in 1:n_obs-1) lA2[i]<-log_returns[i+1]

plot(lA1, lA2, xlab="Log Returns", ylab="Log Returns Lagged", asp=1)

### --------------------------- Mean, Variance and Covariance --------------------

X_mean  <- mean(log_returns) ## Log returns mean
X_adjvar <- var(log_returns) ## Log returns Adjusted Variance
cov_mat <- cov(as.matrix(log_returns))
cov_mat_GBM <- diag(X_adjvar, length(log_returns))
X_mean
X_adjvar
cov_mat

### -------------- Shapiro-Wilk Normality, Box-Pierce and Box-Ljung Tests -------
## Normality test
shapiro.test(log_returns)

## Box-Pierce test on the log-returns
Box.test (log_returns, lag = 5, type = "Box-Pierce")

## Box-Ljung test on the log-returns
Box.test (log_returns, lag = 5, type = "Ljung-Box")

### ------------------------- Kolmogorov-Smirnov Test ----------------------------
## For Normal Distribution
ks.test(log_returns,pnorm,mean=mean(X_mean),sd=sd(log_returns))


### ----------------------------- Parameters -------------------------------------
Delta <- 1/252 # One trading day
mu_hat <- X_mean/Delta + X_adjvar/2 
sigma_hat <- sqrt (X_adjvar/Delta)

mu_hat ## parameter mu
sigma_hat ## parameter Sigma


### ------------------------ Confidence Intervals -------------------------------
### Confidence Interval for mu
alpha <- 0.05
k = length(log_returns) # total log returns data points
se_mu <- sigma_hat / sqrt(k * Delta)
mu_hat + c(-1,1) * qnorm(1 - alpha/2) * se_mu

### Confidence Interval for sigma
lower <- (k-1) * sigma_hat^2 / qchisq(1 - alpha/2, df = k)
upper <- (k-1) * sigma_hat^2 / qchisq(alpha/2, df = k)
sqrt(c(lower, upper))  # Confidence Interval for sigma


### -------------------- 2. Losses on the stock and Value at Risk ---------------
### -----------------------------------------------------------------------------

### ------------------ Historical Method: Value at Risk (vaR) -------------------
St <- as.numeric(tail(prices$'A', 1))

loss_op <- -St * (exp(log_returns) - 1) ## Loss operator
losses_sorted <- sort(loss_op)

alpha <- 0.95
k <- length(loss_op)
VaR_hist <- losses_sorted[floor(k * alpha)]
VaR_hist
VaR_hist/St ## VaR in percentage


### -------------------------- Monte Carlo Simulation ----------------------------

alpha <- 0.95 # confidence level
N <- 100000
set.seed(24)   # for reproducibility
St <- as.numeric(tail(prices$'A', 1)) # Current portfolio value

# Parameters
mu_hat <- mean(log_returns) / Delta + 0.5 * var(log_returns) / Delta
sigma_hat <- sd(log_returns) / sqrt(Delta)
sim_returns <- (mu_hat - 0.5 * sigma_hat^2) * Delta +
  sigma_hat * sqrt(Delta) * rnorm(N)

sim_losses <- - St * (exp(sim_returns) - 1)
sim_losses_sorted <- sort(sim_losses)
VaR_MC <- sim_losses_sorted[floor(N * alpha)]
VaR_MC
VaR_MC/St

### NOTE: change alpha values to 0.99 and 0.999 to get VaR simulation results for 99% 
### and 99.9% confidence levels respectively