# --------------------
# Imports
# --------------------
library(ggplot2)
library(dplyr)
library(readr)
library(stats)

# --------------------
# Reading data
# --------------------
df_white <- read_delim("data/winequality-white.csv", delim = ";")
df_white$wine_color <- "white"
cat("White shape:", dim(df_white), "\n")

df_red <- read_delim("data/winequality-red.csv", delim = ";")
df_red$wine_color <- "red"
cat("Red shape:", dim(df_red), "\n")

df_wine <- bind_rows(df_white, df_red)
cat("Summary shape:", dim(df_wine), "\n")

# --------------------
# Exercise 1.1a
# --------------------
bins <- as.integer(max(df_wine$alcohol) - min(df_wine$alcohol))

ggplot(df_wine, aes(x = alcohol)) +
  geom_histogram(bins = bins, fill = "blue") +
  ggtitle("Distribution of alcohol in wine") +
  xlab("Alcohol [%]") +
  ylab("Frequency")

# --------------------
# Exercise 1.1b
# --------------------
ggplot(df_wine, aes(x = alcohol, fill = wine_color)) +
  geom_histogram(data = df_wine[df_wine$wine_color == "red",], alpha = 0.5, bins = bins, color = "black", linetype = "solid") +
  geom_histogram(data = df_wine[df_wine$wine_color == "white",], alpha = 0.5, bins = bins, color = "black", linetype = "solid") +
  ggtitle("Distribution of alcohol in wine depends on the color") +
  xlab("Alcohol [%]") +
  ylab("Frequency") +
  scale_fill_manual(values = c("red" = "red", "white" = "blue")) +
  theme_minimal() +
  labs(fill = "Wine Color") +
  guides(fill = guide_legend(title = "Wine Color"))

# --------------------
# Exercise 1.1c
# --------------------
pearson <- cor(df_wine$quality, df_wine$alcohol, method = "pearson")
cat("Pearson Correlation: ", pearson, "\n")

spearman <- cor(df_wine$quality, df_wine$alcohol, method = "spearman")
cat("Spearman Correlation:", spearman, "\n")

quality_unique <- unique(df_wine$quality)
wine_color_unique <- unique(df_wine$wine_color)

data_for_plot <- expand.grid(wine_color = wine_color_unique, quality = quality_unique) %>%
  left_join(df_wine, by = c("wine_color" = "wine_color", "quality" = "quality"))

ggplot(data_for_plot, aes(x = factor(quality), y = alcohol, fill = wine_color)) +
  geom_boxplot() +
  ggtitle("Alcohol percentage depending on the quality and color of the wine") +
  xlab("Wine Quality") +
  ylab("Alcohol [%]") +
  scale_fill_manual(values = c("red" = "red", "white" = "blue")) +
  theme_minimal() +
  labs(fill = "Wine Color")

# --------------------
# Exercise 1.1d
# --------------------
pearson_density <- cor(df_wine$density, df_wine$alcohol, method = "pearson")
cat("Pearson Correlation (Density): ", pearson_density, "\n")

spearman_density <- cor(df_wine$density, df_wine$alcohol, method = "spearman")
cat("Spearman Correlation (Density):", spearman_density, "\n")

params <- lm(density ~ alcohol, data = df_wine)

ggplot(df_wine, aes(x = alcohol, y = density)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  ggtitle("Wine density depending on the alcohol percentage") +
  xlab("Alcohol [%]") +
  ylab("Density") +
  theme_minimal() +
  labs(color = "Legend")
