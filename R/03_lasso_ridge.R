# Lasso ve Ridge regresyonu (ceza parametresi k-katlı çapraz doğrulamayla seçilir).
# Tezdeki sonuçlarla birebir aynı olması için bu iki model kendi rastgele
# 80/20 bölmesini (base::sample) kullanır; diğer modeller createDataPartition kullanır.
# Sıra önemlidir: Lasso'nun çapraz doğrulaması Ridge'inkinden önce koşmalıdır.

model_lasso_ridge <- function(veri, ayarlar) {
  b <- veri_bol(veri, "sample")
  train_data <- b$egitim
  test_data <- b$test

  x_train <- as.matrix(train_data[, names(train_data) != HEDEF])
  y_train <- train_data[[HEDEF]]
  x_test <- as.matrix(test_data[, names(test_data) != HEDEF])
  y_test <- test_data[[HEDEF]]

  # --- LASSO ---
  cv_lasso <- glmnet::cv.glmnet(x_train, y_train, alpha = 1)
  lasso_model <- glmnet::glmnet(x_train, y_train, alpha = 1, lambda = cv_lasso$lambda.min)
  pred_lasso_vec <- as.numeric(stats::predict(lasso_model, s = cv_lasso$lambda.min, newx = x_test))
  lasso_sonuc <- model_sonucu("Lasso Regresyon", y_test, pred_lasso_vec, "sample")

  lasso_coefs <- as.matrix(stats::coef(lasso_model))
  lasso_coefs_df <- data.frame(Variable = rownames(lasso_coefs),
                               Coefficient = as.numeric(lasso_coefs))
  lasso_coefs_df <- lasso_coefs_df[order(-abs(lasso_coefs_df$Coefficient)), ]
  bilgi("\nLasso - Önem Sırasına Göre Katsayılar:")
  print(lasso_coefs_df)

  full_lasso <- glmnet::glmnet(x_train, y_train, alpha = 1)
  grafik("03_lasso_cv_ve_katsayi_yollari", function() {
    graphics::par(mfrow = c(1, 2))
    plot(cv_lasso)
    graphics::title("Lasso CV: Lambda vs Hata", line = 3)
    plot(full_lasso, xvar = "lambda", label = TRUE)
    graphics::abline(v = log(cv_lasso$lambda.min), col = "red", lty = 2)
    graphics::title("Katsayı Yolları ve İdeal Lambda", line = 3)
  }, genislik = 12)

  grafik("03_lasso_gercek_vs_tahmin", function() {
    ggplot(data.frame(Actual = y_test, Predicted = pred_lasso_vec), aes(x = Actual, y = Predicted)) +
      geom_point(alpha = 0.5, color = "darkblue") +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      labs(title = "Lasso: Gerçek vs. Tahmin Edilen Değerler",
           x = "Gerçek Sağlık Etki Skoru", y = "Tahmin Edilen Skor") +
      theme_minimal()
  })

  importance_df <- lasso_coefs_df[lasso_coefs_df$Variable != "(Intercept)" &
                                    lasso_coefs_df$Coefficient != 0, ]
  grafik("03_lasso_etki_gucu", function() {
    ggplot(importance_df, aes(x = stats::reorder(Variable, Coefficient), y = Coefficient,
                              fill = Coefficient > 0)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_manual(values = c("FALSE" = "firebrick", "TRUE" = "forestgreen"),
                        labels = c("FALSE" = "Azaltan", "TRUE" = "Artıran")) +
      labs(title = "Hava Kalitesi Parametrelerinin Etki Gücü", x = "Değişkenler",
           y = "Lasso Katsayı Değeri", fill = "Etki Yönü") +
      theme_minimal()
  })

  grafik("03_lasso_artik_dagilimi", function() {
    ggplot(data.frame(Residuals = y_test - pred_lasso_vec), aes(x = Residuals)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "white") +
      labs(title = "Hata (Residual) Dağılımı", x = "Hata Miktarı", y = "Frekans") +
      theme_minimal()
  })

  # --- RIDGE ---
  cv_ridge <- glmnet::cv.glmnet(x_train, y_train, alpha = 0)
  ridge_model <- glmnet::glmnet(x_train, y_train, alpha = 0, lambda = cv_ridge$lambda.min)
  pred_ridge_vec <- as.numeric(stats::predict(ridge_model, s = cv_ridge$lambda.min, newx = x_test))
  ridge_sonuc <- model_sonucu("Ridge Regresyon", y_test, pred_ridge_vec, "sample")

  ridge_coefs <- as.matrix(stats::coef(ridge_model))
  ridge_coefs_df <- data.frame(Variable = rownames(ridge_coefs),
                               Coefficient = as.numeric(ridge_coefs))
  ridge_coefs_df <- ridge_coefs_df[order(-abs(ridge_coefs_df$Coefficient)), ]
  bilgi("\nRidge Katsayıları (Hiçbiri tam sıfır değildir):")
  print(ridge_coefs_df)

  full_ridge <- glmnet::glmnet(x_train, y_train, alpha = 0)
  grafik("03_ridge_cv", function() {
    plot(cv_ridge)
    graphics::title("Ridge CV: Lambda vs Hata", line = 3)
  })

  grafik("03_ridge_katsayi_yollari", function() {
    plot(full_ridge, xvar = "lambda", label = TRUE)
    graphics::abline(v = log(cv_ridge$lambda.min), col = "blue", lty = 2)
    graphics::title("Ridge Katsayı Yolları (Erimeler)", line = 3)
  })

  l2_norm <- apply(full_ridge$beta, 2, function(x) sqrt(sum(x^2)))
  grafik("03_ridge_l2_normu", function() {
    plot(log(full_ridge$lambda), l2_norm, type = "l", col = "blue", lwd = 2,
         xlab = "Log(Lambda)", ylab = "Katsayıların L2 Normu",
         main = "Ridge Cezalandırmasının Katsayı Büyüklüğüne Etkisi")
    graphics::grid()
  })

  ridge_path_df <- as.data.frame(as.matrix(full_ridge$beta))
  ridge_path_df$Variable <- rownames(ridge_path_df)
  ridge_path_long <- tidyr::pivot_longer(ridge_path_df, cols = -Variable,
                                         names_to = "Lambda_Index", values_to = "Coefficient")
  lambda_values <- data.frame(Lambda_Index = paste0("s", 0:(length(full_ridge$lambda) - 1)),
                              Lambda = full_ridge$lambda)
  ridge_path_long <- merge(ridge_path_long, lambda_values)
  grafik("03_ridge_lambda_katsayi_degisimi", function() {
    ggplot(ridge_path_long, aes(x = log(Lambda), y = Coefficient, color = Variable)) +
      geom_line() +
      geom_vline(xintercept = log(cv_ridge$lambda.min), linetype = "dashed", color = "red") +
      theme_minimal() +
      labs(title = "Ridge Regresyonu: Lambda'ya Göre Katsayı Değişimi",
           x = "Log(Lambda)", y = "Katsayı Değeri") +
      theme(legend.position = "bottom")
  })

  ols_model <- stats::lm(HealthImpactScore ~ ., data = train_data)
  ols_coefs <- stats::coef(ols_model)[-1]
  ridge_coefs_final <- as.numeric(stats::coef(ridge_model))[-1]
  comparison_df <- data.frame(Variable = names(ols_coefs), OLS = as.numeric(ols_coefs),
                              Ridge = ridge_coefs_final) %>%
    tidyr::pivot_longer(cols = c(OLS, Ridge), names_to = "Model", values_to = "Value")
  grafik("03_ols_vs_ridge_katsayilari", function() {
    ggplot(comparison_df, aes(x = Variable, y = Value, fill = Model)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      labs(title = "OLS vs. Ridge Katsayı Kıyaslaması",
           subtitle = "Ridge'in katsayıları nasıl stabilize ettiğini görün",
           x = "Değişkenler", y = "Katsayı Değeri") +
      theme_minimal()
  })

  res_df <- data.frame(Model = rep(c("Ridge", "Lasso"), each = length(y_test)),
                       Residuals = c(y_test - pred_ridge_vec, y_test - pred_lasso_vec))
  grafik("03_lasso_ridge_hata_kutu_grafigi", function() {
    ggplot(res_df, aes(x = Model, y = Residuals, fill = Model)) +
      geom_boxplot() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = "Model Hata Dağılımları Karşılaştırması", y = "Hata (Gerçek - Tahmin)") +
      theme_minimal()
  })

  lasso_m <- lasso_sonuc$metrikler
  ridge_m <- ridge_sonuc$metrikler
  performance_comparison <- data.frame(
    Model = c("Lasso", "Ridge"),
    R_Squared = c(lasso_m[["R2"]], ridge_m[["R2"]]),
    RMSE = c(lasso_m[["RMSE"]], ridge_m[["RMSE"]]),
    MAE = c(lasso_m[["MAE"]], ridge_m[["MAE"]]),
    Degisken_Sayisi = c(sum(lasso_coefs != 0) - 1, sum(ridge_coefs != 0) - 1)
  )
  bilgi("\nLasso'nun seçtiği değişken sayısı: ", performance_comparison$Degisken_Sayisi[1])
  bilgi("Ridge'in seçtiği değişken sayısı: ", performance_comparison$Degisken_Sayisi[2])
  print(performance_comparison)

  grafik("03_lasso_ridge_gercek_vs_tahmin", function() {
    p1 <- ggplot(data.frame(Actual = y_test, Predicted = pred_lasso_vec), aes(x = Actual, y = Predicted)) +
      geom_point(color = "coral", alpha = 0.5) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      labs(title = "Lasso: Gerçek vs Tahmin", x = "Gerçek Skor", y = "Tahmin") +
      theme_minimal()
    p2 <- ggplot(data.frame(Actual = y_test, Predicted = pred_ridge_vec), aes(x = Actual, y = Predicted)) +
      geom_point(color = "steelblue", alpha = 0.5) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      labs(title = "Ridge: Gerçek vs Tahmin", x = "Gerçek Skor", y = "Tahmin") +
      theme_minimal()
    patchwork::wrap_plots(p1, p2)
  }, genislik = 12)

  comp_coefs <- data.frame(Feature = rownames(lasso_coefs), Lasso = as.numeric(lasso_coefs),
                           Ridge = as.numeric(ridge_coefs))
  comp_coefs <- comp_coefs[comp_coefs$Feature != "(Intercept)", ]
  comp_long <- tidyr::pivot_longer(comp_coefs, cols = c(Lasso, Ridge),
                                   names_to = "Model", values_to = "Coefficient")
  grafik("03_lasso_ridge_katsayi_siddeti", function() {
    ggplot(comp_long, aes(x = Feature, y = Coefficient, fill = Model)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      labs(title = "Katsayı Şiddeti: Lasso vs Ridge",
           subtitle = "Lasso'nun hangi değişkenleri sıfırladığını inceleyin",
           x = "Değişkenler", y = "Katsayı Değeri") +
      theme_minimal()
  })

  hata_df <- data.frame(Error = c(y_test - pred_lasso_vec, y_test - pred_ridge_vec),
                        Model = rep(c("Lasso", "Ridge"), each = length(y_test)))
  grafik("03_lasso_ridge_hata_yogunlugu", function() {
    ggplot(hata_df, aes(x = Error, fill = Model)) +
      geom_density(alpha = 0.4) +
      geom_vline(xintercept = 0, linetype = "dotted") +
      labs(title = "Hata Yoğunluk Grafiği", x = "Tahmin Hatası", y = "Yoğunluk") +
      theme_minimal()
  })

  results_df <- data.frame(Predicted = pred_ridge_vec, Residuals = y_test - pred_ridge_vec)
  grafik("03_ridge_artik_analizi", function() {
    p1_ridge <- ggplot(results_df, aes(x = Predicted, y = Residuals)) +
      geom_point(alpha = 0.5, color = "steelblue") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      geom_smooth(method = "loess", formula = y ~ x, color = "darkblue", se = FALSE) +
      labs(title = "Ridge: Hata vs. Tahmin Edilen Değerler",
           subtitle = "Sıfır etrafında rastgele dağılım 'iyi model' demektir",
           x = "Tahmin Edilen Sağlık Etki Skoru", y = "Hata (Artıklar)") +
      theme_minimal()
    p2_ridge <- ggplot(results_df, aes(x = Residuals)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
      labs(title = "Ridge: Hata Dağılımı (Histogram)", x = "Hata Miktarı", y = "Frekans") +
      theme_minimal()
    patchwork::wrap_plots(p1_ridge, p2_ridge)
  }, genislik = 12)

  list(lasso_sonuc, ridge_sonuc)
}
