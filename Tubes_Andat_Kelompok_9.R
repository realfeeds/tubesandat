df <- movies_metadata

num_cols <- c("budget", "revenue", "popularity", "runtime", "vote_average")
for (col in num_cols) {
  df[[col]] <- as.numeric(df[[col]])
}

df$release_year <- as.numeric(substr(df$release_date, 1, 4))


df$log_budget     <- log(df$budget + 1)
df$log_revenue    <- log(df$revenue + 1)
df$log_popularity <- log(df$popularity + 1)

model_df <- df[, c("vote_average",
                   "log_budget", "log_revenue", "log_popularity",
                   "runtime", "release_year")]

model_df <- model_df[complete.cases(model_df), ]

# regresi linear
model <- lm(vote_average ~ ., data = model_df)
summary(model)
(r2 <- summary(model)$r.squared)

coef_vals <- coef(model)[-1]
barplot(coef_vals,
        horiz = TRUE,
        las = 1,
        main = "Feature Importance (Log-Scale Linear Model)",
        xlab = "Coefficient Value")

# distribusi rating
ratings <- model_df$vote_average
mean_rating   <- mean(ratings)
median_rating <- median(ratings)
iqr_rating    <- IQR(ratings)
mean_rating
median_rating
iqr_rating

hist(model_df$vote_average,
     breaks = 30,
     main = "Distribution of Movie Ratings",
     xlab = "TMDB Rating",
     col = "lightgray",
     border = "white")

# scatter matrix
pairs(
  model_df[, c("vote_average",
               "log_budget",
               "log_popularity",
               "runtime")],
  main = "Scatter Matrix of Key Variables",
  pch = 20,
  col = rgb(0, 0, 0, 0.2)
)
