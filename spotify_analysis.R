
# Spotify Data Analysis Project
# Dataset source:
# https://www.kaggle.com/datasets/nelgiriyewithana/most-streamed-spotify-songs-2024/data


spotify_data <- read.csv("spotify_2024.csv", header = TRUE, sep = ",")
# Load libraries

library(ggplot2)

# Load dataset
spotify_data <- read.csv("spotify_2024.csv")


# Create log-transformed streams to reduce skew
spotify_data$log_streams <- log10(spotify_data$spotify_streams + 1)


# 1. Proportion of Explicit Tracks

explicit_track <- spotify_data$explicit_track
p_hat <- mean(explicit_track)
n <- length(explicit_track)
p0 <- 1/3

# Z-test for proportion
z_stat <- (p_hat - p0) / sqrt(p0 * (1 - p0) / n)
p_value_prop <- 2 * (1 - pnorm(abs(z_stat)))

# Confidence interval (99%)
z_99 <- 2.576
se_prop <- sqrt(0.25 / n)
ci_prop <- c(0.5 - z_99 * se_prop, 0.5 + z_99 * se_prop)

# 2. Mean Track Score Analysis

scores <- spotify_data$track_score

xbar <- mean(scores)
s <- sd(scores)
n_scores <- length(scores)

# Hypothesis test (mean = 40)
t_stat <- (xbar - 40) / (s / sqrt(n_scores))
p_value_mean <- 2 * (1 - pnorm(abs(t_stat)))

# Confidence interval (90%)
z_90 <- 1.645
se_mean <- s / sqrt(n_scores)
ci_mean <- c(xbar - z_90 * se_mean, xbar + z_90 * se_mean)

# 3. Correlation Analysis

correlation_test <- cor.test(
  spotify_data$track_score,
  spotify_data$log_streams
)

# 4. Regression Model

model <- lm(log_streams ~ track_score, data = spotify_data)
model_summary <- summary(model)

# 5. Visualizations

# Scatter plot
ggplot(spotify_data, aes(x = track_score, y = log_streams)) +
  geom_point(alpha = 0.5, color = "purple") +
  labs(
    title = "Track Score vs Log(Spotify Streams)",
    x = "Track Score",
    y = "Log(Streams)"
  )

# Histogram
ggplot(spotify_data, aes(x = log_streams)) +
  geom_histogram(bins = 50, fill = "pink") +
  labs(
    title = "Distribution of Log(Spotify Streams)",
    x = "Log(Streams)"
  )

# 6. High Score Proportion

high_score <- ifelse(spotify_data$track_score >= 70, 1, 0)

p_hat_high <- mean(high_score)
n_high <- length(high_score)
p0_high <- 0.20

z_high <- (p_hat_high - p0_high) / sqrt(p0_high * (1 - p0_high) / n_high)
p_value_high <- 2 * (1 - pnorm(abs(z_high)))

# 95% CI
se_high <- sqrt(p_hat_high * (1 - p_hat_high) / n_high)
ci_high <- c(
  p_hat_high - qnorm(0.975) * se_high,
  p_hat_high + qnorm(0.975) * se_high
)

print(model_summary)
print(correlation_test)
