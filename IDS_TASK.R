

library(tidyverse)   # dplyr, ggplot2, tidyr
library(e1071)       # skewness(), kurtosis()
library(patchwork)   # arrange plots
# library(psych)    # describe() - optional
# library(naniar)   # vis_miss() - optional for missingness visualization

data("faithful")     # load dataset

#UNIVARIATE ANALYSIS

dim(faithful)        # number of rows and columns
head(faithful)       # first 6 rows
str(faithful)        # structure & datatypes
names(faithful)      # column names

sapply(faithful, class)      # class of each column
sapply(faithful, is.numeric) # TRUE if numeric

sum(is.na(faithful))         # total NA in dataset
colSums(is.na(faithful))     # NA count per column

summary(faithful)   # min, 1Q, median, mean, 3Q, max for each column

# detailed stats (mean, sd, median, IQR, skewness, kurtosis)
num_summary <- faithful %>%
  summarise(across(everything(), list(
    n = ~sum(!is.na(.)),
    mean = ~mean(., na.rm=TRUE),
    median = ~median(., na.rm=TRUE),
    sd = ~sd(., na.rm=TRUE),
    IQR = ~IQR(., na.rm=TRUE),
    min = ~min(., na.rm=TRUE),
    max = ~max(., na.rm=TRUE),
    skew = ~e1071::skewness(., na.rm=TRUE),
    kurt = ~e1071::kurtosis(., na.rm=TRUE)
  ), .names = "{.col}_{.fn}"))

# make it easier to view
num_summary %>% pivot_longer(everything(), names_to="metric", values_to="value")

# 1) Histograms with density overlay
p1 <- ggplot(faithful, aes(x = eruptions)) +
  geom_histogram(bins = 25, aes(y = ..density..), color="black", fill=NA) +
  geom_density(alpha=0.2) +
  labs(title="Eruption durations (min)", x="eruptions")
  p1

p2 <- ggplot(faithful, aes(x = waiting)) +
  geom_histogram(bins = 25, aes(y = ..density..), color="black", fill=NA) +
  geom_density(alpha=0.2) +
  labs(title="Waiting times (min)", x="waiting")
  print(p2)

# 2) Boxplots
p3 <- ggplot(faithful, aes(y = eruptions)) + geom_boxplot() + labs(title="Boxplot: eruptions")
p3
p4 <- ggplot(faithful, aes(y = waiting))   + geom_boxplot() + labs(title="Boxplot: waiting")
p4


# 3) QQ-plots
qq1 <- ggplot(faithful, aes(sample = eruptions)) + stat_qq() + stat_qq_line() + labs(title="Q-Q: eruptions")
qq1                                                                                     
qq2 <- ggplot(faithful, aes(sample = waiting))   + stat_qq() + stat_qq_line() + labs(title="Q-Q: waiting")
qq2
# Arrange plots
(p1 + p2) / (p3 + p4)    # histogram row / boxplot row
qq1 + qq2                # qq plots side-by-side

outlier_info <- map_dfr(names(faithful), function(colname){
  x <- faithful[[colname]]
  q1 <- quantile(x, 0.25, na.rm=TRUE)
  q3 <- quantile(x, 0.75, na.rm=TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  tibble(variable = colname,
         q1 = q1, q3 = q3, IQR = iqr,
         lower = lower, upper = upper,
         n_outliers = sum(x < lower | x > upper, na.rm = TRUE))
})
outlier_info
# show rows that are outliers for eruptions
faithful %>% mutate(row = row_number()) %>%
  filter(eruptions < outlier_info$lower[outlier_info$variable=="eruptions"] |
           eruptions > outlier_info$upper[outlier_info$variable=="eruptions"])

#BIVARIATE ANALYSIS
cor(faithful$eruptions, faithful$waiting) 
cor(faithful)

library(ggplot2)
#SCATTER PLOT

ggplot(faithful, aes(x = eruptions, y = waiting)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relationship between Eruptions and Waiting Time",
       x = "Eruption Duration (min)",
       y = "Waiting Time (min)")

model <- lm(waiting ~ eruptions, data = faithful)
summary(model)


#MULTIVARIATE ANALYSIS
model <- lm(waiting ~ eruptions, data = faithful)
summary(model)


ggplot(faithful, aes(x = eruptions, y = waiting)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Linear Regression between Eruptions and Waiting Time",
       x = "Eruption Duration (min)",
       y = "Waiting Time (min)")

#Residual Analysis (to check model fit)

faithful$residuals <- resid(model)

ggplot(faithful, aes(x = eruptions, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residual Plot", x = "Eruption Duration", y = "Residuals")

#Density Contours (joint distribution)

ggplot(faithful, aes(x = eruptions, y = waiting)) +
  geom_point(alpha = 0.5) +
  geom_density_2d(color = "red") +
  labs(title = "2D Density of Eruptions vs Waiting Time")

#CLUSTERING
set.seed(123)
faithful_clusters <- kmeans(faithful, centers = 2)
faithful$cluster <- as.factor(faithful_clusters$cluster)

ggplot(faithful, aes(x = eruptions, y = waiting, color = cluster)) +
  geom_point(size = 2) +
  labs(title = "Clustering of Faithful Data (K-Means)")

#HEAT MAP

ggplot(faithful, aes(x = eruptions, y = waiting)) +
  geom_bin2d(bins = 25) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Heatmap: Frequency of Eruptions vs Waiting Time",
       x = "Eruption Duration (minutes)",
       y = "Waiting Time (minutes)",
       fill = "Count")



