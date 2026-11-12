data_toko_awal <- data.frame(
  Tahun  = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
  Gula   = c(50, 70, 100, 120, 110, 100, 120, 135, 140, 150, 145),
  Minyak = c(80, 85, 90, 100, 120, 125, 135, 140, 150, 190, 195),
  Sabun  = c(55, 70, 80, 95, 80, 100, 105, 110, 120, 100, 130)
)

write.csv(data_toko_awal, "datatoko.csv", row.names = FALSE)
cat("File datatoko.csv berhasil dibuat!\n\n")


# ------------------------------------------------------------------------------
# SOAL 1: BACA TABEL 1 KE DALAM R
# ------------------------------------------------------------------------------
datatoko <- read.csv("datatoko.csv")

cat("=== HASIL BACA DATASET (SOAL 1) ===\n")
print(datatoko)
str(datatoko)


# ------------------------------------------------------------------------------
# SOAL 2: ANALISIS STATISTIK SEDERHANA (SUMMARY)
# ------------------------------------------------------------------------------
cat("\n=== RINGKASAN STATISTIK DESKRIPTIF (SOAL 2) ===\n")
summary_stats <- summary(datatoko[, c("Gula", "Minyak", "Sabun")])
print(summary_stats)

# Standar Deviasi & Total Akumulasi
sd_gula   <- sd(datatoko$Gula)
sd_minyak <- sd(datatoko$Minyak)
sd_sabun  <- sd(datatoko$Sabun)

total_gula   <- sum(datatoko$Gula)
total_minyak <- sum(datatoko$Minyak)
total_sabun  <- sum(datatoko$Sabun)

cat("\n--- STANDAR DEVIASI ---\n")
cat("SD Gula   :", round(sd_gula, 2), "\n")
cat("SD Minyak :", round(sd_minyak, 2), "\n")
cat("SD Sabun  :", round(sd_sabun, 2), "\n")

cat("\n--- TOTAL PENJUALAN (UNIT) ---\n")
cat("Total Gula   :", total_gula, "unit\n")
cat("Total Minyak :", total_minyak, "unit\n")
cat("Total Sabun  :", total_sabun, "unit\n")


# ------------------------------------------------------------------------------
# SOAL 3: PLOT MASING-MASING BARANG DAN PLOT GABUNGAN
# ------------------------------------------------------------------------------
# Mengatur tampilan grid 2x2 untuk 4 grafik
par(mfrow = c(2, 2))

# 1. Plot Penjualan Gula
plot(datatoko$Tahun, datatoko$Gula, type = "o", col = "blue", pch = 16, lwd = 2,
     xlab = "Tahun", ylab = "Jumlah Terjual (Unit)", main = "Penjualan Gula (2010-2020)")
grid()

# 2. Plot Penjualan Minyak
plot(datatoko$Tahun, datatoko$Minyak, type = "o", col = "red", pch = 17, lwd = 2,
     xlab = "Tahun", ylab = "Jumlah Terjual (Unit)", main = "Penjualan Minyak (2010-2020)")
grid()

# 3. Plot Penjualan Sabun
plot(datatoko$Tahun, datatoko$Sabun, type = "o", col = "darkgreen", pch = 18, lwd = 2,
     xlab = "Tahun", ylab = "Jumlah Terjual (Unit)", main = "Penjualan Sabun (2010-2020)")
grid()

# 4. Plot Gabungan Ketiga Barang
plot(datatoko$Tahun, datatoko$Minyak, type = "o", col = "red", pch = 17, lwd = 2,
     ylim = c(40, 210), xlab = "Tahun", ylab = "Jumlah Terjual (Unit)", main = "Gabungan Penjualan Toko A")
lines(datatoko$Tahun, datatoko$Gula, type = "o", col = "blue", pch = 16, lwd = 2)
lines(datatoko$Tahun, datatoko$Sabun, type = "o", col = "darkgreen", pch = 18, lwd = 2)
legend("topleft", legend = c("Minyak", "Gula", "Sabun"),
       col = c("red", "blue", "darkgreen"), pch = c(17, 16, 18), lty = 1, lwd = 2, bty = "n")
grid()

# Mengembalikan layout ke default
par(mfrow = c(1, 1))
