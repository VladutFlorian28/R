

library(readr)
library(readxl)
library(DBI)
library(RMySQL)
library(dplyr)
library(lubridate)

setwd("C:/Users/vladu/Documents")



books_df <- read_csv(
  "books_3.csv",
  na = c("", "N/A")
)


col_an_publicare <- names(books_df)[
  grepl("year|pub|date", names(books_df), ignore.case = TRUE)
][1]

if (!is.na(col_an_publicare)) {
  books_df <- books_df %>%
    rename(publication_year = !!sym(col_an_publicare)) %>%
    mutate(publication_year = as.integer(publication_year))
} else {
  warning("Nu am găsit o coloană care să conțină anul publicării!")
}

glimpse(books_df)


rentals_df <- read_excel(
  "users_rentals_3.xlsx",
  na = c("", "N/A"),
  col_types = c("numeric", "text", "text", "text", "text", "numeric", "date")
)

glimpse(rentals_df)


con <- dbConnect(
  RMySQL::MySQL(),
  host = "localhost",
  user = "root",
  password = "Alin20032003",
  dbname = "library"
)

# Verificăm numele tabelelor
print(dbListTables(con))

# Preluăm tabelele în R
book_db   <- dbReadTable(con, "book")
user_db   <- dbReadTable(con, "user")
rental_db <- dbReadTable(con, "rental")

# Verificăm structura tabelelor pentru nume coloane
print(names(book_db))
print(names(user_db))
print(names(rental_db))

dbDisconnect(con)


book_id_col <- if ("book_id" %in% names(book_db)) "book_id" else "id"
user_id_col <- if ("user_id" %in% names(user_db)) "user_id" else if ("id" %in% names(user_db)) "id" else NA


nr_books_csv <- nrow(books_df)
nr_books_db  <- nrow(book_db)


min_year <- if ("publication_year" %in% names(books_df)) {
  min(books_df$publication_year, na.rm = TRUE)
} else { NA }

max_year <- if ("publication_year" %in% names(books_df)) {
  max(books_df$publication_year, na.rm = TRUE)
} else { NA }


total_rentals <- nrow(rentals_df)


distinct_books_excel <- length(unique(rentals_df$rental_book_id))
distinct_books_db <- length(unique(rental_db$book_id))


available_books <- if ("status" %in% names(rentals_df)) {
  rentals_df %>%
    filter(status == "available") %>%
    distinct(rental_book_id)
} else {
  tibble() 
}


users_east_albert <- if (all(c("city") %in% names(user_db)) && all(c("user_id", "returned") %in% names(rental_db))) {
  user_db %>%
    filter(city == "East Albert") %>%
    anti_join(
      rental_db %>% filter(returned == FALSE),
      by = setNames("user_id", user_id_col)
    )
} else {
  tibble()
}


top_books <- rental_db %>%
  group_by(book_id) %>%
  summarise(total_rentals = n()) %>%
  arrange(desc(total_rentals)) %>%
  slice_head(n = 10) %>%
  left_join(book_db, by = setNames(book_id_col, "book_id"))

top_users <- rental_db %>%
  group_by(user_id) %>%
  summarise(total_rentals = n()) %>%
  arrange(desc(total_rentals)) %>%
  slice_head(n = 10) %>%
  left_join(user_db, by = setNames(user_id_col, "user_id"))



write_csv(available_books, "Vladut_Florian_Rezultate_Task3_AvailableBooks.csv")
write_csv(users_east_albert, "Vladut_Florian_Rezultate_Task3_UsersNoRentals.csv")
write_csv(top_books, "Vladut_Florian_Rezultate_Task3_TopBooks.csv")
write_csv(top_users, "Vladut_Florian_Rezultate_Task3_TopUsers.csv")

###############################################################

