library(tidyverse)
library(lubridate)

# 1. Încărcare fișiere
books <- read_csv("books_2.0.csv")
users <- read_csv("users.csv")
rentals <- read_csv("rentals.csv")

# 2. Pregătire books
# Redenumim coloana id -> book_id și păstrăm doar coloanele relevante
books <- books %>%
  rename(book_id = id) %>%
  select(book_id, title, author, published, genre)

# 3. Pregătire users
# Redenumim coloana id -> user_id
users <- users %>%
  rename(user_id = user_id)

# 4. Pregătire rentals
# Identificăm coloanele book_id_X și return_date_X
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
    by = c(names(rentals)[1], "index") # prima coloană este user_id sau rental_id
  )

# 5. Legare rentals cu users
rentals_users_df <- rentals_tidy %>%
  left_join(users, by = "user_id")

# 6. Legare cu books
full_data_df <- rentals_users_df %>%
  left_join(books, by = "book_id")

# 7. Întrebări de business

## 7.1. Cele mai populare genuri și pentru cine
genuri_populare <- full_data_df %>%
  filter(!is.na(genre)) %>%
  group_by(user_id, genre) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))

## 7.2. Sezonalitate (număr de cărți împrumutate pe lună)
sezonalitate <- full_data_df %>%
  mutate(return_date = dmy(return_date),
         luna = month(return_date, label = TRUE, abbr = FALSE)) %>%
  group_by(luna) %>%
  summarise(n = n(), .groups = "drop")

## 7.3. Utilizatori care revin la același gen
revin_gen <- full_data_df %>%
  filter(!is.na(genre)) %>%
  group_by(user_id, genre) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)

## 7.4. Utilizatori fantomă
fantome <- full_data_df %>%
  group_by(user_id) %>%
  summarise(total = n(), .groups = "drop") %>%
  filter(total == 1)

## 7.5. Top țări după vizite
top_tari <- full_data_df %>%
  filter(!is.na(country)) %>%
  group_by(country) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))

## 7.6. Tabel pivot utilizator x gen
pivot_user_gen <- full_data_df %>%
  filter(!is.na(genre)) %>%
  group_by(user_id, genre) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = genre, values_from = n, values_fill = 0)

# 8. Salvare rezultate
write_csv(genuri_populare, "genuri_populare.csv")
write_csv(sezonalitate, "sezonalitate.csv")
write_csv(revin_gen, "utilizatori_revin_gen.csv")
write_csv(fantome, "utilizatori_fantoma.csv")
write_csv(top_tari, "top_tari.csv")
write_csv(pivot_user_gen, "pivot_user_gen.csv")

#In urma analizei efectuate din fisierele rezultate filtrarii datelor reiese ca : Utilizatorii care au revenit , au revenit pentru genurile Science si non-fiction
#Nu exista utilizatori fantoma
#Cel mai des biblioteca a fost vizitata de locuitorii urmatoarelor tari : USA - 52 , Serbia - 49 , Germany -
#Sezonalitatea exista  , apetitul pentru lectura creste pe toata perioada verii si in luna ianuarie
#Desi sunt multe genuri populare  , cele mai cele sunt Science si Non-Fiction