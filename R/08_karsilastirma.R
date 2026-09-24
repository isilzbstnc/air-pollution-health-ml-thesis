# Tüm modellerin test sonuçlarını tek tabloda toplar, sıralar ve kaydeder.
# Not: Lasso ve Ridge kendi rastgele bölmesiyle (base::sample) değerlendirilir;
# diğer beş model aynı createDataPartition test setini kullanır.

karsilastir <- function(sonuclar) {
  final_summary <- data.frame(
    Model = vapply(sonuclar, function(s) s$ad, character(1)),
    R_Kare = vapply(sonuclar, function(s) s$metrikler[["R2"]], numeric(1)),
    RMSE = vapply(sonuclar, function(s) s$metrikler[["RMSE"]], numeric(1)),
    MAE = vapply(sonuclar, function(s) s$metrikler[["MAE"]], numeric(1)),
    Test_N = vapply(sonuclar, function(s) length(s$gercek), integer(1)),
    Bolme = vapply(sonuclar, function(s) s$bolme, character(1)),
    row.names = NULL
  )
  final_summary <- final_summary[order(-final_summary$R_Kare), ]
  rownames(final_summary) <- NULL

  bilgi("--- MODEL PERFORMANS SIRALAMASI ---")
  print(final_summary, digits = 4)
  bilgi("(Bolme: cdp = createDataPartition, sample = Lasso/Ridge'in kendi rastgele bölmesi)")

  csv <- file.path(CALISMA$cikti_klasoru, "metrikler.csv")
  utils::write.csv(final_summary, csv, row.names = FALSE)
  bilgi("Metrikler kaydedildi: ", csv)

  grafik("08_model_r2_siralamasi", function() {
    ggplot(final_summary, aes(x = stats::reorder(Model, R_Kare), y = R_Kare, fill = R_Kare)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(R_Kare, 4)), hjust = -0.2) +
      coord_flip() +
      scale_y_continuous(limits = c(0, 1.1)) +
      scale_fill_gradient(low = "skyblue", high = "darkblue") +
      labs(title = "Hava Kalitesi ve Sağlık Etkisi: Model Performans Kıyaslaması",
           subtitle = "R-Kare (Belirleme Katsayısı) Değerlerine Göre Sıralama",
           x = "Kullanılan Algoritmalar", y = "Açıklayıcılık Oranı (R-Kare)") +
      theme_minimal() +
      theme(legend.position = "none")
  })

  grafik("08_model_performans_matrisi", function() {
    ggplot(final_summary, aes(x = R_Kare, y = RMSE, label = Model, color = Model)) +
      geom_point(size = 5) +
      geom_text(vjust = -1.5, size = 3.5, fontface = "bold") +
      labs(title = "Model Performans Matrisi",
           subtitle = "R-Kare (Yüksek Daha İyi) vs. RMSE (Düşük Daha İyi)",
           x = "R-Kare (Açıklayıcılık Oranı)", y = "RMSE (Hata Oranı)") +
      theme_minimal() +
      theme(legend.position = "none")
  })

  # Aynı test setini paylaşan modellerin tahmin dağılımları
  ortak <- Filter(function(s) s$bolme == "cdp", sonuclar)
  secilen <- intersect(c("XGBoost", "KNN", "SVR"), names(ortak))
  if (length(secilen) > 0) {
    gercek <- ortak[[secilen[1]]]$gercek
    tahminler <- do.call(rbind, lapply(secilen, function(ad) {
      data.frame(Model = ad, Tahmin = ortak[[ad]]$tahmin)
    }))
    grafik("08_tahmin_dagilimlari", function() {
      ggplot() +
        geom_density(aes(x = gercek, fill = "Gerçek Veri"), alpha = 0.2) +
        geom_density(data = tahminler, aes(x = Tahmin, color = Model), linewidth = 1) +
        scale_fill_manual(values = c("Gerçek Veri" = "gray40"), name = NULL) +
        labs(title = "Model Tahminlerinin Orijinal Veri Dağılımı ile Kıyaslanması",
             x = "Sağlık Skoru", y = "Yoğunluk") +
        theme_classic()
    })
  }

  grafik("08_model_mae_kiyaslamasi", function() {
    ggplot(final_summary, aes(x = stats::reorder(Model, MAE), y = MAE, fill = Model)) +
      geom_bar(stat = "identity") +
      labs(title = "Modellerin Ortalama Mutlak Hata (MAE) Kıyaslaması",
           x = "Model", y = "Hata Miktarı") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })

  invisible(final_summary)
}
