library(tidyverse)
library(lubridate)

books <- read_csv("books_2.0.csv")
users <- read_csv("users.csv")
rentals <- read_csv("rentals.csv")

books <- books %>%
  rename(book_id = id) %>%
  select(book_id, title, author, published, genre)

users <- users %>%
  rename(user_id = user_id)

rentals_tidy <- rentals %>%
  pivot_longer(
    cols = matches("book_id_\\d+"),
    names_to = "book_col",
    values_to = "book_id"
  ) %>%
  mutate(index = parse_number(book_col)) %>%
  select(-book_col) %>%
  left_join(
    rentals %>%
      pivot_longer(
        cols = matches("return_date_\\d+"),
        names_to = "date_col",
        values_to = "return_date"
      ) %>%
      mutate(index = parse_number(date_col)) %>%
      select(-date_col),
    by = c(names(rentals)[1], "index")
  )

rentals_users_df <- rentals_tidy %>%
  left_join(users, by = "user_id")

full_data_df <- rentals_users_df %>%
  left_join(books, by = "book_id")

genuri_populare <- full_data_df %>%
  filter(!is.na(genre)) %>%
  group_by(user_id, genre) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))

sezonalitate <- full_data_df %>%
  mutate(return_date = dmy(return_date),
         luna = month(return_date, label = TRUE, abbr = FALSE)) %>%
  group_by(luna) %>%
  summarise(n = n(), .groups = "drop")

revin_gen <- full_data_df %>%
  filter(!is.na(genre)) %>%
  group_by(user_id, genre) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)

fantome <- full_data_df %>%
  group_by(user_id) %>%
  summarise(total = n(), .groups = "drop") %>%
  filter(total == 1)

top_tari <- full_data_df %>%
  filter(!is.na(country)) %>%
  group_by(country) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))

pivot_user_gen <- full_data_df %>%
  filter(!is.na(genre)) %>%
  group_by(user_id, genre) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = genre, values_from = n, values_fill = 0)

write_csv(genuri_populare, "genuri_populare.csv")
write_csv(sezonalitate, "sezonalitate.csv")
write_csv(revin_gen, "utilizatori_revin_gen.csv")
write_csv(fantome, "utilizatori_fantoma.csv")
write_csv(top_tari, "top_tari.csv")
write_csv(pivot_user_gen, "pivot_user_gen.csv")
