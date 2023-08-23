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
y_pois <- rpois(N * 2, b0 + b1 * g)

## fit models
m_pois <- gamlss(y_pois ~ g, family = PO())
m_pois_no <- gamlss(y_pois ~ g, family = NO())

## model summary and diagnostic plots
summary(m_pois)
summary(m_pois_no)
wilcox.test(y_pois ~ g)

plot(m_pois)
plot(m_pois_no)

## test for normality of residuals
shapiro.test(m_pois$residuals)
shapiro.test(m_pois_no$residuals)



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
m_t <- gamlss(y_t ~ g, family = TF())
m_t_no <- gamlss(y_t ~ g, family = NO())

## model summary and diagnostic plots
summary(m_t)
summary(m_t_no)
wilcox.test(y_t ~ g)

plot(m_t)
plot(m_t_no)

## test for normality of residuals
shapiro.test(m_t$residuals)
shapiro.test(m_t_no$residuals)


## ------------------------------------------------------------
## Log-normal Distribution
## ------------------------------------------------------------

## coefficients
b0 <- 0.5
b1 <- 1
sigma <- 1

## generate data
set.seed(11)
y_lognormal <- rLOGNO(N * 2, b0 + b1 * g, sigma)

## fit models
m_ln <- gamlss(y_lognormal ~ g, family = LOGNO())
m_ln_no <- gamlss(y_lognormal ~ g, family = NO())

## model summary and diagnostic plots
summary(m_ln)
summary(m_ln_no)
wilcox.test(y_lognormal ~ g)

plot(m_ln)
plot(m_ln_no)

## test for normality of residuals
shapiro.test(m_ln$residuals)
shapiro.test(m_ln_no$residuals)


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
m_beta <- gamlss(y_beta ~ g, family = BE())
m_beta_no <- gamlss(y_beta ~ g, family = NO())

## model summary and diagnostic plots
summary(m_beta)
summary(m_beta_no)
wilcox.test(y_beta ~ g)

plot(m_beta)
plot(m_beta_no)

## test for normality of residuals
shapiro.test(m_beta$residuals)
shapiro.test(m_beta_no$residuals)




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
    pch = 16, cex = 3.5, size = 2, method = "center"
  ) +
  labs(x = "", y = "Outcome", title = "Poisson:\nCounts") +
  ylim(0, 4) +
  my_theme

# Plot 2
p2 <- ggplot() +
  geom_beeswarm(aes(x = group_labels, y = y_t),
    pch = 16, cex = 3.5, size = 2, method = "center"
  ) +
  labs(x = "", y = "Outcome", title = "Student-t:\nOutliers") +
  ylim(0, 15) +
  theme_bw() +
  my_theme

# Plot 3
p3 <- ggplot() +
  geom_beeswarm(aes(x = group_labels, y = y_lognormal),
    pch = 16, cex = 3.5, size = 2, method = "center"
  ) +
  labs(x = "", y = "Outcome", title = "Log-normal:\nSkewed and positive") +
  ylim(0, 12) +
  theme_bw() +
  my_theme

# Plot 4
p4 <- ggplot() +
  geom_beeswarm(aes(x = group_labels, y = y_beta),
    pch = 16, cex = 3.5, size = 2, method = "center"
  ) +
  labs(x = "", y = "Outcome", title = "Beta:\nProportions or percents") +
  ylim(0, 1) +
  theme_bw() +
  my_theme


## Arrange the plots in a 2x2 grid
all_plots <- plot_grid(p1, p2, p3, p4,
  nrow = 2,
  labels = LETTERS[1:4], align = "v"
)


## table of p-values for the various analyses
df <- tibble(
  `Poisson` = c("0.027", "0.017", "0.025"),
  `Student-t` = c("0.018", "0.870", "0.075"),
  `Log-normal` = c("0.008", "0.053", "0.015"), 
  `Beta` = c("0.087", "0.100", "0.089")
)
rownames(df) <- c("Best", "Normal", "Wilcoxon")

## create a ggtable
p5 <- ggtexttable(df, theme = ttheme("mBlue"))


## combine plots and table
pdf("fig1.pdf", height = 8, width = 6)
plot_grid(all_plots, p5,
  ncol = 1, rel_heights = c(4, 1),
  labels = c("", "E")
)
dev.off()
