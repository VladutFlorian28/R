
library(tidyverse)


country_distribution <- read_csv("country_distribution.csv")
genre_by_user <- read_csv("genre_by_user.csv")
ghost_users <- read_csv("ghost_users.csv")
monthly_rentals <- read_csv("monthly_rentals.csv")


glimpse(country_distribution)
glimpse(genre_by_user)
glimpse(ghost_users)
glimpse(monthly_rentals)


ggplot(genre_by_user, aes(x = genre, y = count, color = genre)) +
  geom_jitter(width = 0.2, alpha = 0.7) +
  labs(
    title = "Număr împrumuturi per gen și utilizator",
    x = "Gen literar",
    y = "Număr cărți împrumutate"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


ggplot(monthly_rentals, aes(x = month, y = rental_count)) +
  geom_line(color = "#2E86AB", size = 1) +
  geom_point(color = "#2E86AB", size = 2) +
  scale_x_continuous(breaks = 1:12) +
  labs(
    title = "Evoluția împrumuturilor lunare",
    x = "Lună",
    y = "Număr împrumuturi"
  ) +
  theme_minimal()


ggplot(country_distribution, aes(x = reorder(country, -user_count), y = user_count, fill = country)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Număr utilizatori pe țară",
    x = "Țară",
    y = "Număr utilizatori"
  ) +
  theme_minimal()


ggplot(ghost_users, aes(x = rental_count)) +
  geom_histogram(binwidth = 5, fill = "#E27D60", color = "white", alpha = 0.8) +
  labs(
    title = "Distribuția utilizatorilor după numărul de împrumuturi",
    x = "Număr împrumuturi/an",
    y = "Număr utilizatori"
  ) +
  theme_minimal()


ggplot(genre_by_user, aes(x = factor(user_id), y = count, fill = genre)) +
  geom_col() +
  labs(
    title = "Preferințe de lectură per utilizator",
    x = "Utilizator",
    y = "Număr împrumuturi",
    fill = "Gen literar"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank())
