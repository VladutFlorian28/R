
library(tidyverse)   
library(broom)       

set.seed(123)


df <- readr::read_csv("library_books_read.csv")


glimpse(df)
summary(df)


na_counts <- colSums(is.na(df))
print(na_counts)


if(any(na_counts > 0)){
  message("Sunt valori lipsă — se vor elimina rândurile incomplete (poți schimba strategia).")
  df <- df %>% drop_na(member_age, membership_years, books_read)
}


df <- df %>%
  mutate(
    member_age = as.numeric(member_age),
    membership_years = as.numeric(membership_years),
    books_read = as.numeric(books_read)
  )


p1 <- ggplot(df, aes(x = member_age, y = books_read)) +
  geom_jitter(width = 0.6, height = 0.6, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "books_read ~ member_age",
       x = "Vârsta membru (ani)", y = "Număr cărți citite") +
  theme_minimal()


p2 <- ggplot(df, aes(x = membership_years, y = books_read)) +
  geom_jitter(width = 0.2, height = 0.6, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "books_read ~ membership_years",
       x = "Ani de membru", y = "Număr cărți citite") +
  theme_minimal()

ggsave("books_vs_age.png", p1, width = 7, height = 5)
ggsave("books_vs_membership_years.png", p2, width = 7, height = 5)


cor_age <- cor(df$books_read, df$member_age, method = "pearson")
cor_membership <- cor(df$books_read, df$membership_years, method = "pearson")
cat("Correlație books_read vs member_age:", round(cor_age, 3), "\n")
cat("Correlație books_read vs membership_years:", round(cor_membership, 3), "\n")


lm_books <- lm(books_read ~ member_age + membership_years, data = df)
print(summary(lm_books))


tidy_lm <- tidy(lm_books)
print(tidy_lm)


df <- df %>% mutate(
  books_pred = predict(lm_books, newdata = df)
)


MAE <- mean(abs(df$books_pred - df$books_read))
RMSE <- sqrt(mean((df$books_pred - df$books_read)^2))
cat("MAE (regresie):", round(MAE, 3), "\n")
cat("RMSE (regresie):", round(RMSE, 3), "\n")


p3 <- ggplot(df, aes(x = books_read, y = books_pred)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Valori reale vs prezise (regresie)", x = "books_read (real)", y = "books_pred (prezis)") +
  theme_minimal()
ggsave("real_vs_predicted.png", p3, width = 6, height = 6)


new_members <- tibble(
  member_age = c(18, 25, 40, 60),
  membership_years = c(1, 3, 5, 7)
)
new_members <- new_members %>%
  mutate(books_pred = predict(lm_books, newdata = new_members),
         books_pred_rounded = round(books_pred))

print(new_members)
readr::write_csv(new_members, "predictions_regression.csv")
cat("Predicțiile pentru membri noi au fost salvate în predictions_regression.csv\n")


threshold <- 25

df <- df %>%
  mutate(read_many_books = if_else(books_read > threshold, 1L, 0L))


table(df$read_many_books)


glm_reader <- glm(read_many_books ~ member_age + membership_years, data = df, family = binomial)
print(summary(glm_reader))


df <- df %>%
  mutate(prob_many = predict(glm_reader, type = "response"),
         pred_many = if_else(prob_many > 0.5, 1L, 0L))


conf_mat <- table(Real = df$read_many_books, Pred = df$pred_many)
print(conf_mat)


accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
precision <- ifelse(sum(conf_mat[,"1"]) == 0, NA, conf_mat["1","1"] / sum(conf_mat[,"1"]))
recall <- ifelse(sum(conf_mat["1",]) == 0, NA, conf_mat["1","1"] / sum(conf_mat["1",]))
cat("Accuracy (clasificare):", round(accuracy, 3), "\n")
cat("Precision (clasificare):", ifelse(is.na(precision), "NA", round(precision,3)), "\n")
cat("Recall (clasificare):", ifelse(is.na(recall), "NA", round(recall,3)), "\n")


df_class_preds <- df %>% select(member_age, membership_years, books_read, read_many_books, prob_many, pred_many)
readr::write_csv(df_class_preds, "predictions_classification.csv")
cat("Predicțiile de clasificare au fost salvate în predictions_classification.csv\n")


p4 <- ggplot(df, aes(x = member_age, y = membership_years, color = prob_many)) +
  geom_point(alpha = 0.8, size = 2.5) +
  scale_color_viridis_c(option = "C") +
  labs(title = paste0("Probabilitatea de a fi " , threshold, "+ cărți (logistic)"),
       color = "Probabilitate") +
  theme_minimal()
ggsave("probability_scatter.png", p4, width = 7, height = 5)


saveRDS(lm_books, file = "lm_books_model.rds")
saveRDS(glm_reader, file = "glm_reader_model.rds")
cat("Modelele au fost salvate ca lm_books_model.rds și glm_reader_model.rds\n")


coef_lm <- tidy_lm %>% filter(term != "(Intercept)") %>% arrange(desc(abs(estimate)))
cat("Ordinea predictorilor (regresie) după impact absolut estimat:\n")
print(coef_lm)

coef_glm <- broom::tidy(glm_reader) %>% filter(term != "(Intercept)") %>% arrange(desc(abs(estimate)))
cat("Ordinea predictorilor (logistic) după impact absolut estimat:\n")
print(coef_glm)

cat("\nSfaturi:")
cat("\n - Verifică semnificația (p-value) în summary() pentru a decide ce variabile sunt relevante.")
cat("\n - Dacă modelul nu explică suficient (R^2 mic / acuratețe mică), încearcă caracteristici noi sau date adiționale.")


cat("Script rulat complet. Vezi fișierele generate: books_vs_age.png, books_vs_membership_years.png, real_vs_predicted.png, probability_scatter.png, predictions_regression.csv, predictions_classification.csv, lm_books_model.rds, glm_reader_model.rds\n")
