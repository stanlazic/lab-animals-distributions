library(tidyverse)
library(gamlss)
library(ggplot2)
library(ggbeeswarm)
library(cowplot)


## sample size per group
N <- 10

## group indicator and labels
g <- rep(0:1, N)
group_labels <- rep(c("Control", "Drug"), N)


## ------------------------------------------------------------
## Poisson Distribution
## ------------------------------------------------------------

## coefficients
b0 <- 0.45
b1 <- 0.8

## generate data
set.seed(123)
y_pois <- rPO(N * 2, b0 + b1 * g)

## fit models
m_pois <- gamlss(y_pois ~ g, family = PO())    # Poisson distribution
m_pois_no <- gamlss(y_pois ~ g, family = NO()) # Normal distribution

## model summary and diagnostic plots
summary(m_pois)
summary(m_pois_no)
wilcox.test(y_pois ~ g)
## a t-test is equivalent to the m_pois_no model, but the p-values differ slightly
## due to the different algorithms used.
t.test(y_pois ~ g, var.equal = TRUE)

## confidence intervals
confint(m_pois)
confint(m_pois_no)

plot(m_pois)
plot(m_pois_no)

## test for normality of residuals
shapiro.test(m_pois$residuals)
shapiro.test(m_pois_no$residuals)

## test if predictions include negative values
poisson_predictions <- rNO(
  n = 100000,
  mu = unique(predict(m_pois_no, type = "response")),
  sigma = exp(m_pois_no$sigma.coefficients)
)

ifelse(min(poisson_predictions) < 0, "Impossible values!", "OK")


## ------------------------------------------------------------
## Student-t Distribution
## ------------------------------------------------------------

## coefficients
b0 <- 10
b1 <- 1
sigma <- 1

## generate data
set.seed(25)
y_t <- b0 + b1 * g + rt(N * 2, df = 2) * sigma

## fit models
m_t <- gamlss(y_t ~ g, family = TF())    # Student-t distribution
m_t_no <- gamlss(y_t ~ g, family = NO()) # Normal distribution

## model summary and diagnostic plots
summary(m_t)
summary(m_t_no)
wilcox.test(y_t ~ g)
## a t-test is equivalent to the m_t_no model, but the p-values differ slightly
## due to the different algorithms used.
t.test(y_t ~ g, var.equal = TRUE)

## confidence intervals
confint(m_t)
confint(m_t_no)

plot(m_t)
plot(m_t_no)

## test for normality of residuals
shapiro.test(m_t$residuals)
shapiro.test(m_t_no$residuals)



## ------------------------------------------------------------
## Gamma Distribution
## ------------------------------------------------------------

## coefficients
b0 <- 0.5
b1 <- 2
sigma <- 1

## generate data
set.seed(11)
y_gamma <- rGA(N * 2, b0 + b1 * g, sigma)

## fit models
m_gamma <- gamlss(y_gamma ~ g, family = GA(mu.link = "log")) # Gamma distribution
m_gamma_no <- gamlss(log(y_gamma) ~ g, family = NO())        # Normal distribution (log transformed data)

## try lognormal model for comparison
m_log <- gamlss(y_gamma ~ g, family = LOGNO()) # Lognormal distribution

## which is better based on AIC (lower is better)
m_gamma$aic # Gamma is much better
m_log$aic

## model summary and diagnostic plots
summary(m_gamma)
summary(m_gamma_no)
wilcox.test(y_gamma ~ g)
## a t-test is equivalent to the m_gamma_no model, but the p-values differ slightly
## due to the different algorithms used.
t.test(log(y_gamma) ~ g, var.equal = TRUE)

## confidence intervals
confint(m_gamma)
confint(m_gamma_no)

plot(m_gamma)
plot(m_gamma_no)

## test for normality of residuals
shapiro.test(m_gamma$residuals)
shapiro.test(m_gamma_no$residuals)



## ------------------------------------------------------------
## Beta Distribution
## ------------------------------------------------------------

## coefficients
b0 <- -1.5
b1 <- 1.75
sigma <- 0.8

## generate data
set.seed(11)
y_beta <- rBE(N * 2, plogis(b0 + b1 * g), sigma)

## fit models
m_beta <- gamlss(y_beta ~ g, family = BE(mu.link = "log")) # Beta distribution
m_beta_no <- gamlss(y_beta ~ g, family = NO())             # Normal distribution

## model summary and diagnostic plots
summary(m_beta) # note the model coef needs to be logged to put it on the probably scale: log(0.763)=0.27
summary(m_beta_no)
wilcox.test(y_beta ~ g)
## a t-test is equivalent to the m_beta_no model, but the p-values differ slightly
## due to the different algorithms used.
t.test(y_beta ~ g, var.equal = TRUE)

## confidence intervals
confint(m_beta_no)


plot(m_beta)
plot(m_beta_no)

## test for normality of residuals
shapiro.test(m_beta$residuals)
shapiro.test(m_beta_no$residuals)


## test if predictions include values outside of [0,1]
beta_predictions <- rNO(
  n = 100000,
  mu = unique(predict(m_beta_no, type = "response")),
  sigma = exp(m_beta_no$sigma.coefficients)
)

ifelse(min(beta_predictions) < 0 || max(beta_predictions) > 1, "Impossible values!", "OK")



## ------------------------------------------------------------
## Plots for manuscript
## ------------------------------------------------------------


## Define custom theme for plots
my_theme <- theme_cowplot() +
  theme(
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13),
    title = element_text(size = 11),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 1.5
    )
  )

# Plot 1
p1 <- ggplot() +
  geom_beeswarm(aes(x = group_labels, y = y_pois),
    pch = 21, fill = "grey", cex = 3.5, size = 2, method = "center"
  ) +
  labs(x = "", y = "Outcome", title = "Poisson:\nCounts") +
  ylim(0, 4) +
  my_theme

# Plot 2
p2 <- ggplot() +
  geom_beeswarm(aes(x = group_labels, y = y_t),
    pch = 21, fill = "grey", cex = 3.5, size = 2, method = "center"
  ) +
  labs(x = "", y = "Outcome", title = "Student-t:\nOutliers") +
  ylim(0, 15) +
  theme_bw() +
  my_theme

# Plot 3
p3 <- ggplot() +
  geom_beeswarm(aes(x = group_labels, y = y_gamma),
    pch = 21, fill = "grey", cex = 3.5, size = 2, method = "center"
  ) +
  labs(x = "", y = "Outcome", title = "Gamma:\nSkewed and positive") +
  ylim(0, 10) +
  theme_bw() +
  my_theme

# Plot 4
p4 <- ggplot() +
  geom_beeswarm(aes(x = group_labels, y = y_beta),
    pch = 21, fill = "grey", cex = 3.5, size = 2, method = "center"
  ) +
  labs(x = "", y = "Outcome", title = "Beta:\nProportions (0 to 1)") +
  ylim(0, 1) +
  theme_bw() +
  my_theme


## Arrange the plots in a 2x2 grid
all_plots <- plot_grid(p1, p2, p3, p4,
  nrow = 2,
  labels = LETTERS[1:4], align = "v"
)


## combine plots and table
pdf("fig1.pdf", height = 6, width = 5.5)
plot_grid(all_plots)
dev.off()
