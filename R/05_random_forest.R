# Random Forest regresyonu (500 ağaç) ve ek olarak tek karar ağacı.

model_random_forest <- function(veri, ayarlar) {
  b <- veri_bol(veri, "cdp")
  train_data <- b$egitim
  test_data <- b$test

  rf_reg_model <- randomForest::randomForest(HealthImpactScore ~ ., data = train_data,
                                             importance = TRUE, ntree = 500)
  print(rf_reg_model)

  predictions <- stats::predict(rf_reg_model, newdata = test_data)
  sonuc <- model_sonucu("Random Forest", test_data$HealthImpactScore, predictions, "cdp")

  grafik("05_rf_degisken_onemi", function() {
    randomForest::varImpPlot(rf_reg_model, main = "Değişkenlerin Sağlık Skoru Üzerindeki Etkisi")
  }, genislik = 11)

  plot_data <- data.frame(Gercek = test_data$HealthImpactScore, Tahmin = predictions)
  grafik("05_rf_gercek_vs_tahmin", function() {
    ggplot(plot_data, aes(x = Gercek, y = Tahmin)) +
      geom_point(alpha = 0.5, color = "blue") +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
      labs(title = "Gerçek vs. Tahmin Edilen Sağlık Skoru",
           x = "Gerçek Değerler", y = "Tahmin Edilen Değerler") +
      theme_minimal()
  })

  residuals_rf <- test_data$HealthImpactScore - predictions
  grafik("05_rf_hata_dagilimi", function() {
    graphics::hist(residuals_rf, breaks = 30, main = "Hataların Dağılımı (Residuals)",
                   xlab = "Hata Miktarı", col = "skyblue")
  })

  grafik("05_rf_kismi_bagimlilik_cardiovascular", function() {
    randomForest::partialPlot(rf_reg_model, pred.data = train_data,
                              x.var = "CardiovascularCases",
                              main = "CardiovascularCases Değişkeninin Sağlık Skoru Üzerindeki Etkisi")
  })

  grafik("05_rf_agac_sayisi_hata", function() {
    plot(rf_reg_model, main = "Ağaç Sayısına Göre Hata Oranı")
    graphics::legend("topright", legend = "Hata (MSE)", col = 1, lty = 1, cex = 0.8)
  })

  imp_df <- as.data.frame(randomForest::importance(rf_reg_model))
  imp_df$Variable <- rownames(imp_df)
  grafik("05_rf_degisken_onemi_detay", function() {
    ggplot(imp_df, aes(x = stats::reorder(Variable, `%IncMSE`), y = `%IncMSE`)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = "Değişkenlerin Sağlık Skoru Üzerindeki Etkisi",
           subtitle = "Random Forest - Değişken Önem Düzeyi", x = "Değişkenler",
           y = "% Artış MSE (Daha yüksek daha önemli)") +
      theme_minimal()
  })

  grafik("05_rf_dagilim_kiyaslamasi", function() {
    ggplot() +
      geom_density(aes(x = test_data$HealthImpactScore, fill = "Gerçek"), alpha = 0.4) +
      geom_density(aes(x = predictions, fill = "Tahmin"), alpha = 0.4) +
      scale_fill_manual(values = c("Gerçek" = "blue", "Tahmin" = "red")) +
      labs(title = "Gerçek vs Tahmin Edilen Değerlerin Dağılımı",
           x = "Health Impact Score", y = "Yoğunluk", fill = "Grup") +
      theme_classic()
  })

  # Ek model: tek karar ağacı
  tek_agac <- rpart::rpart(HealthImpactScore ~ ., data = train_data, method = "anova")
  print(tek_agac)
  grafik("05_karar_agaci", function() {
    rpart.plot::rpart.plot(tek_agac, type = 3, clip.right.labs = FALSE, branch = .3,
                           under = TRUE, box.palette = "BuGn",
                           main = "Sağlık Skoru Tahmin Dallanması")
  }, genislik = 12, yukseklik = 8)

  sonuc
}
