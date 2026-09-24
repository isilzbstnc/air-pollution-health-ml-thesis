# Çoklu lineer regresyon: katsayılar, VIF, artık analizi, güven aralıkları,
# standartlaştırılmış katsayılar.

model_lineer_regresyon <- function(veri, ayarlar) {
  b <- veri_bol(veri, "cdp")
  train_data <- b$egitim
  test_data <- b$test

  lm_model <- stats::lm(HealthImpactScore ~ ., data = train_data)
  summary_info <- summary(lm_model)
  print(summary_info)

  lm_summary <- summary_info$coefficients
  significant_vars <- lm_summary[lm_summary[, 4] < 0.05, , drop = FALSE]
  bilgi("\n--- İstatistiksel Olarak Anlamlı Değişkenler ---")
  print(significant_vars)

  pred_lm <- stats::predict(lm_model, test_data)
  sonuc <- model_sonucu("Lineer Regresyon", test_data$HealthImpactScore, pred_lm, "cdp")

  vif_values <- car::vif(lm_model)
  bilgi("\nVIF Değerleri:")
  print(vif_values)

  grafik("02_lm_vif", function() {
    graphics::barplot(vif_values, main = "VIF Değerleri (Multicollinearity Kontrolü)",
                      col = "steelblue", las = 2, cex.names = 0.7)
    graphics::abline(h = 5, col = "red", lty = 2)
  })

  grafik("02_lm_tani_grafikleri", function() {
    graphics::par(mfrow = c(2, 2))
    plot(lm_model)
  }, genislik = 10, yukseklik = 8)

  bilgi("\n--- Katsayılar İçin %95 Güven Aralıkları ---")
  print(stats::confint(lm_model))

  grafik("02_lm_gercek_vs_tahmin", function() {
    plot(test_data$HealthImpactScore, pred_lm,
         xlab = "Gerçek Değerler", ylab = "Tahmin Edilen Değerler",
         main = "Gerçek vs Tahmin (Linear Regression)", col = "blue", pch = 16)
    graphics::abline(0, 1, col = "red", lwd = 2)
  })

  grafik("02_lm_artik_histogrami", function() {
    graphics::hist(stats::residuals(lm_model), main = "Residual Histogram",
                   col = "lightblue", breaks = 30)
  })

  grafik("02_lm_artik_vs_tahmin", function() {
    plot(stats::fitted(lm_model), stats::residuals(lm_model),
         xlab = "Tahmin Edilen", ylab = "Residual", main = "Residual vs Fitted",
         col = "darkgreen", pch = 16)
    graphics::abline(h = 0, col = "red")
  })

  lm_beta <- lm.beta::lm.beta(lm_model)
  grafik("02_lm_degisken_onemi", function() {
    graphics::barplot(sort(abs(lm_beta$standardized.coefficients), decreasing = TRUE),
                      main = "Değişken Önem Sıralaması", col = "orange", las = 2)
  })

  grafik("02_lm_egitim_korelasyonu", function() {
    cor_matrix <- stats::cor(train_data[, vapply(train_data, is.numeric, logical(1))])
    corrplot::corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7)
  }, genislik = 8, yukseklik = 8)

  sonuc
}
