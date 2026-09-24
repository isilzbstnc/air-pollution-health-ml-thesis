# K-en yakın komşu regresyonu (girdiler standartlaştırılır, k 5-katlı çapraz
# doğrulamayla seçilir), PM10 senaryosu ve risk kategorisi başarısı.

skoru_kategorilere_ayir <- function(score) {
  ifelse(score < 25, "Düşük",
         ifelse(score < 50, "Orta",
                ifelse(score < 75, "Yüksek", "Çok Yüksek")))
}

skoru_dar_kategorilere_ayir <- function(score) {
  cut(score, breaks = seq(0, 100, by = 10), include.lowest = TRUE,
      labels = paste0("Grup-", 1:10))
}

model_knn <- function(veri, ayarlar) {
  b <- veri_bol(veri, "cdp")
  train_set <- b$egitim
  test_set <- b$test

  control <- caret::trainControl(method = "cv", number = 5)
  knn_model <- caret::train(HealthImpactScore ~ ., data = train_set, method = "knn",
                            trControl = control, preProcess = c("center", "scale"),
                            tuneLength = 10)
  bilgi("Seçilen komşu sayısı:")
  print(knn_model$bestTune)

  pred_knn <- stats::predict(knn_model, test_set)
  sonuc <- model_sonucu("KNN", test_set$HealthImpactScore, pred_knn, "cdp")

  grafik("04_knn_k_secimi", function() {
    plot(knn_model, main = "k Komşuluk Sayısına Göre RMSE Değişimi")
  })

  results_knn <- data.frame(Gercek = test_set$HealthImpactScore, Tahmin = pred_knn)
  grafik("04_knn_gercek_vs_tahmin", function() {
    ggplot(results_knn, aes(x = Gercek, y = Tahmin)) +
      geom_point(color = "purple", alpha = 0.5) +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      labs(title = "KNN: Gerçek vs Tahmin Değerleri",
           x = "Gerçek Sağlık Skoru", y = "Tahmin Edilen Sağlık Skoru") +
      theme_minimal()
  })

  test_set_sim_knn <- test_set
  test_set_sim_knn$PM10 <- test_set_sim_knn$PM10 * 1.10
  pred_sim_knn <- stats::predict(knn_model, test_set_sim_knn)
  diff_percent_knn <- ((mean(pred_sim_knn) - mean(pred_knn)) / mean(pred_knn)) * 100
  bilgi("\n--- KNN Senaryo Analizi: PM10 %10 Artışı ---")
  bilgi("Sağlık Skoru Üzerindeki Etki (%): ", format(diff_percent_knn))

  knn_imp <- caret::varImp(knn_model, scale = FALSE)
  grafik("04_knn_degisken_onemi", function() {
    plot(knn_imp, main = "KNN Modelinde Değişkenlerin Etki Düzeyi")
  })

  res_knn <- test_set$HealthImpactScore - pred_knn
  grafik("04_knn_hata_dagilimi", function() {
    ggplot(data.frame(Hata = res_knn), aes(x = Hata)) +
      geom_histogram(bins = 25, fill = "purple", color = "white", alpha = 0.7) +
      geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
      labs(title = "KNN Tahmin Hataları Dağılımı", x = "Hata Miktarı", y = "Frekans") +
      theme_minimal()
  })

  grafik("04_knn_dagilim_kiyaslamasi", function() {
    ggplot() +
      geom_density(aes(x = test_set$HealthImpactScore, fill = "Gerçek"), alpha = 0.4) +
      geom_density(aes(x = pred_knn, fill = "KNN Tahmin"), alpha = 0.4) +
      labs(title = "Gerçek ve KNN Tahmin Dağılımı Kıyaslaması", x = "Sağlık Skoru") +
      scale_fill_manual(values = c("Gerçek" = "gray", "KNN Tahmin" = "purple")) +
      theme_classic()
  })

  mape_knn <- mean(abs((test_set$HealthImpactScore - pred_knn) / test_set$HealthImpactScore)) * 100
  bilgi("KNN Ortalama Yüzde Hata: %", format(mape_knn))

  seviyeler <- c("Düşük", "Orta", "Yüksek", "Çok Yüksek")
  results_knn <- results_knn %>%
    dplyr::mutate(Gercek_Kat = factor(skoru_kategorilere_ayir(Gercek), levels = seviyeler),
                  Tahmin_Kat = factor(skoru_kategorilere_ayir(Tahmin), levels = seviyeler))
  conf_matrix <- caret::confusionMatrix(results_knn$Tahmin_Kat, results_knn$Gercek_Kat)
  print(conf_matrix)

  plt_table <- as.data.frame(conf_matrix$table)
  grafik("04_knn_risk_kategorisi_basarisi", function() {
    ggplot(plt_table, aes(Prediction, Reference, fill = Freq)) +
      geom_tile() +
      geom_text(aes(label = Freq), color = "white") +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(title = "KNN Risk Kategorisi Tahmin Başarısı",
           x = "Tahmin Edilen Sınıf", y = "Gerçek Sınıf") +
      theme_minimal()
  })

  error_rate <- mean(results_knn$Gercek_Kat != results_knn$Tahmin_Kat)
  bilgi("Kategorik Tahmin Hata Oranı: %", format(error_rate * 100))

  results_knn <- results_knn %>%
    dplyr::mutate(Gercek_Strict = skoru_dar_kategorilere_ayir(Gercek),
                  Tahmin_Strict = skoru_dar_kategorilere_ayir(Tahmin))
  strict_error <- mean(results_knn$Gercek_Strict != results_knn$Tahmin_Strict)
  bilgi("Daha Dar Kategorilerle Hata Oranı: %", format(strict_error * 100))

  sonuc
}
