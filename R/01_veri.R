# Veri okuma, keşifsel analiz, temizlik ve eğitim/test bölmesi.
# Tüm modeller aynı temizlenmiş veriyi kullanır.

BEKLENEN_SUTUNLAR <- c(
  "RecordID", "AQI", "PM10", "PM2_5", "NO2", "SO2", "O3", "Temperature",
  "Humidity", "WindSpeed", "RespiratoryCases", "CardiovascularCases",
  "HospitalAdmissions", "HealthImpactScore", "HealthImpactClass"
)

veri_oku <- function(yol) {
  if (!file.exists(yol)) {
    stop("Veri dosyası bulunamadı: ", yol, "\n",
         "Veri seti depoda yok. Kaggle'dan indirin:\n  ", KAGGLE_ADRESI, "\n",
         "ve '", VERI_DOSYA_ADI, "' dosyasını veri/ klasörüne koyun\n",
         "ya da yolu şöyle verin: Rscript main.R --veri=/yol/", VERI_DOSYA_ADI,
         call. = FALSE)
  }
  ham <- utils::read.csv(yol)
  eksik <- setdiff(BEKLENEN_SUTUNLAR, names(ham))
  if (length(eksik) > 0) {
    stop("CSV dosyasında beklenen sütunlar yok: ", paste(eksik, collapse = ", "), call. = FALSE)
  }
  ham
}

# IQR kuralı: çeyrekler arası açıklığın 1.5 katı dışındaki değerleri NA yapar.
aykiri_degerleri_isaretle <- function(x) {
  Q1 <- stats::quantile(x, 0.25, na.rm = TRUE)
  Q3 <- stats::quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  x[x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR)] <- NA
  x
}

# Aykırı değer temizliği YALNIZ bağımsız değişkenlere uygulanır.
# Hedef değişken 100'de tavan yapar (satırların ~%74'ü tam 100); IQR kuralı
# hedefe de uygulanırsa 95.5'in altındaki bütün skorlar silinir ve model
# neredeyse sabit bir değeri tahmin etmeye çalışır.
veri_hazirla <- function(ham) {
  baslik("VERİ")
  str(ham)
  print(summary(ham))
  bilgi("\nEksik değer sayıları:")
  print(colSums(is.na(ham)))

  grafik("01_aykiri_deger_kontrolu", function() {
    graphics::boxplot(dplyr::select(ham, dplyr::where(is.numeric)), las = 2,
                      main = "Outlier Kontrolü")
  }, genislik = 11)

  veri <- ham %>% dplyr::select(-RecordID, -HealthImpactClass)
  ozellikler <- setdiff(names(veri), HEDEF)
  veri_temiz <- veri %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(ozellikler), aykiri_degerleri_isaretle)) %>%
    stats::na.omit()
  veri_temiz <- as.data.frame(veri_temiz)
  rownames(veri_temiz) <- NULL
  attr(veri_temiz, "na.action") <- NULL

  bilgi(sprintf("\nHam satır sayısı            : %d", nrow(ham)))
  bilgi(sprintf("Aykırı değer temizliği sonrası: %d (%d satır çıkarıldı)",
                nrow(veri_temiz), nrow(ham) - nrow(veri_temiz)))
  bilgi(sprintf("Hedef skoru tam 100 olan satır: %d (%%%.1f)",
                sum(veri_temiz[[HEDEF]] == 100), 100 * mean(veri_temiz[[HEDEF]] == 100)))

  grafik("01_korelasyon_matrisi", function() {
    cor_matrix <- stats::cor(dplyr::select(veri_temiz, dplyr::where(is.numeric)))
    corrplot::corrplot(cor_matrix, method = "color", tl.cex = 0.7)
  }, genislik = 8, yukseklik = 8)

  veri_temiz
}

# 80/20 eğitim/test bölmesi. Her model kendi bölmesini bu fonksiyonla alır;
# tohum her çağrıda yeniden ayarlandığı için aynı yöntem her zaman aynı
# satırları verir ve modelin sonraki rastgele adımları (çapraz doğrulama,
# rastgele orman vb.) özgün analizle aynı rastgele sayı dizisini kullanır.
#   "cdp"    : caret::createDataPartition (hedefe göre tabakalı). Lasso/Ridge
#              dışındaki tüm modeller.
#   "sample" : base::sample. Lasso ve Ridge (tezdeki sonuçlar bu bölmeyle üretildi).
veri_bol <- function(veri, yontem = c("cdp", "sample")) {
  yontem <- match.arg(yontem)
  set.seed(TOHUM)
  if (yontem == "cdp") {
    indeks <- caret::createDataPartition(veri[[HEDEF]], p = 0.8, list = FALSE)
  } else {
    indeks <- sample(seq_len(nrow(veri)), 0.8 * nrow(veri))
  }
  list(egitim = veri[indeks, , drop = FALSE], test = veri[-indeks, , drop = FALSE])
}
