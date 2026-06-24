preturi_A <- c(19.99, 24.50, 18.75, 22.00, 20.30)
cantitati_A <- c(5, 8 , 4 ,7,6)

preturi_B <- c(15.00 , 16.50 , 14.75 , 17.00, 15.80)
cantitati_B <-c(10,9,12,8,11)

produs_A <- list(
  nume = "Produs A",
  pret = preturi_A,
  cantitate = cantitati_A
)
produs_B <- list(
  nume = "Produs B",
  pret = preturi_B,
  cantitate = cantitati_B
)
shop_data <- list(
  produs_A = produs_A,
  produs_B = produs_B
)
venit_A = preturi_A * cantitati_A
venit_B = preturi_B * cantitati_B

shop_data$venituri_totale <- list(
  A = sum(venit_A),
  B = sum(venit_B)
)

#ziua cu venitul maxim pentru fiecare produs
zi_max_A <- which.max(venit_A)
zi_max_B <- which.max(venit_B)

#vanzare medie zilnica
cant_medie_A <- mean(cantitati_A)
cant_medie_B <- mean(cantitati_B)

#Data frame pentru fiecare produs
df_A <- data.frame(
  produs = "Produs A",
  zi = 1:5,
  pret = preturi_A,
  cantitate = cantitati_A,
  venit = venit_A
)
df_B <- data.frame(
  produs = "Produs B",
  zi = 1:5,
  pret = preturi_B,
  cantitate = cantitati_B,
  venit = venit_B
  
)
df_sales <- rbind(df_A , df_B)
venit_total_per_produs <- aggregate(venit ~ produs , df_sales , sum)
pret_mediu_per_produs <- aggregate(pret ~ produs , df_sales , mean)
cantitate_totala_per_produs <- aggregate(cantitate ~ produs , df_sales , sum)

produs_top_venit <- venit_total_per_produs$produs[which.max(venit_total_per_produs$venit)]

produs_top_pret <- pret_mediu_per_produs$produs[which.max(pret_mediu_per_produs$pret)]

produs_top_vanzari <- cantitate_totala_per_produs$produs[which.max(cantitate_totala_per_produs$cantitate)]

print("=== Rezumat metrici ===")
print(venit_total_per_produs)
print(pret_mediu_per_produs)
print(cantitate_totala_per_produs)


cat("\nProdusul cu cel mai mare venit total: ", produs_top_venit)
cat("\nProdusul cu preț mediu mai mare: ", produs_top_pret)
cat("\nProdusul cu cele mai multe unități vândute: ", produs_top_vanzari, "\n")



