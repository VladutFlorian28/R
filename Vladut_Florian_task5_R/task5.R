#############################################
# Prenume_Nume_Task5.R
# Exemplu: Andrei_Popescu_Task5.R
#############################################

# =========================
# 1. Introducere
# =========================
# Acest script creează vizualizări clare și relevante pe baza seturilor de date:
# country_distribution.csv, genre_by_user.csv, ghost_users.csv, monthly_rentals.csv
# Scop: să răspundă vizual la întrebările cerute în sarcină, folosind tipul de grafic optim pentru fiecare situație.

# =========================
# 2. Încărcarea și pregătirea datelor
# =========================
library(tidyverse)

# Importul datelor (presupunem că fișierele sunt în directorul de lucru)
country_distribution <- read_csv("country_distribution.csv")
genre_by_user <- read_csv("genre_by_user.csv")
ghost_users <- read_csv("ghost_users.csv")
monthly_rentals <- read_csv("monthly_rentals.csv")

# Verificări de bază
glimpse(country_distribution)
glimpse(genre_by_user)
glimpse(ghost_users)
glimpse(monthly_rentals)

# =========================
# 3. Vizualizări
# =========================

# ---- 3.1 Tabel utilizator–gen cu numărul de împrumuturi ----
# Mesaj: câte genuri diferite citește fiecare utilizator și câte cărți a citit din fiecare gen.
# Alegere: scatter plot -> ușor de observat variația în funcție de gen și user_id
ggplot(genre_by_user, aes(x = genre, y = count, color = genre)) +
  geom_jitter(width = 0.2, alpha = 0.7) +
  labs(
    title = "Număr împrumuturi per gen și utilizator",
    x = "Gen literar",
    y = "Număr cărți împrumutate"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# ---- 3.2 Numărul de cărți împrumutate de-a lungul lunilor ----
# Mesaj: identificăm trendul și posibilele oscilații sezoniere
# Alegere: line plot -> clar pentru serii temporale
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

# ---- 3.3 Țara cu cei mai mulți utilizatori ----
# Mesaj: evidențierea clară a țării dominante
# Alegere: barplot -> compară direct frecvențele pe țări
ggplot(country_distribution, aes(x = reorder(country, -user_count), y = user_count, fill = country)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Număr utilizatori pe țară",
    x = "Țară",
    y = "Număr utilizatori"
  ) +
  theme_minimal()

# ---- 3.4 Distribuția utilizatorilor după numărul anual de cărți împrumutate ----
# Mesaj: câți sunt „fantome” și câți sunt cititori fideli
# Alegere: histogramă -> arată distribuția naturală
ggplot(ghost_users, aes(x = rental_count)) +
  geom_histogram(binwidth = 5, fill = "#E27D60", color = "white", alpha = 0.8) +
  labs(
    title = "Distribuția utilizatorilor după numărul de împrumuturi",
    x = "Număr împrumuturi/an",
    y = "Număr utilizatori"
  ) +
  theme_minimal()

# ---- 3.5 Genurile cele mai populare – și pentru cine ----
# Mesaj: preferințele pe gen în funcție de utilizatori
# Alegere: barplot stacked -> arată proporția genurilor per utilizator
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

# =========================
# 4. Concluzie
# =========================
# Graficele prezentate răspund clar întrebărilor propuse:
# - Scatter plot-ul arată diversitatea pe gen a fiecărui utilizator.
# - Linia evidențiază evoluția lunară și eventuale trenduri sezoniere.
# - Barplot-ul pentru țări scoate în evidență liderul la număr de utilizatori.
# - Histograma arată distribuția și extremele (ghost users vs heavy readers).
# - Barplot-ul stacked indică preferințele de lectură în funcție de utilizator.
