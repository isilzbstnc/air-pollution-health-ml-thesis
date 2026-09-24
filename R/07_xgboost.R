# XGBoost regresyonu, değişken önemi, PM10 senaryosu, SHAP değerleri,
# öğrenme eğrisi ve kısmi bağımlılık grafiği.
# xgboost 1.7.x ve 3.x sürümlerinin ikisiyle de çalışır (parametre adları farklı).

xgb_egit <- function(params, dtrain, dtest) {
  izleme <- list(train = dtrain, val = dtest)
  args <- list(params = params, data = dtrain, nrounds = 1000,
               early_stopping_rounds = 50, print_every_n = 50)
  if ("evals" %in% names(formals(xgboost::xgb.train))) args$evals <- izleme else args$watchlist <- izleme
  do.call(xgboost::xgb.train, args)
}

xgb_ogrenme_kaydi <- function(model) {
  kayit <- attributes(model)$evaluation_log   # xgboost 3.x
  if (is.null(kayit)) kayit <- model$evaluation_log  # xgboost 1.7.x
  as.data.frame(kayit)
}

xgb_agac_ciz <- function(model) {
  if ("tree_idx" %in% names(formals(xgboost::xgb.plot.tree))) {
    xgboost::xgb.plot.tree(model = model, tree_idx = 1)
  } else {
    xgboost::xgb.plot.tree(model = model, trees = 0, show_node_id = TRUE)
  }
}

model_xgboost <- function(veri, ayarlar) {
  b <- veri_bol(veri, "cdp")
  train_set <- b$egitim
  test_set <- b$test

  train_x <- stats::model.matrix(HealthImpactScore ~ . - 1, data = train_set)
  train_y <- train_set$HealthImpactScore
  test_x <- stats::model.matrix(HealthImpactScore ~ . - 1, data = test_set)
  test_y <- test_set$HealthImpactScore

  dtrain <- xgboost::xgb.DMatrix(data = train_x, label = train_y)
  dtest <- xgboost::xgb.DMatrix(data = test_x, label = test_y)

  params <- list(
    booster = "gbtree",
    objective = "reg:squarederror",
    eta = 0.05,             # öğrenme hızı
    max_depth = 6,          # dallanma derinliği
    subsample = 0.8,        # her ağaçta verinin %80'i kullanılır
    colsample_bytree = 0.8  # her ağaçta değişkenlerin %80'i kullanılır
  )
  # Not: erken durdurma, tezdeki gibi test setini doğrulama seti olarak izler.
  xgb_model <- xgb_egit(params, dtrain, dtest)

  pred_xgb <- stats::predict(xgb_model, dtest)
  sonuc <- model_sonucu("XGBoost", test_y, pred_xgb, "cdp")

  importance_matrix <- xgboost::xgb.importance(feature_names = colnames(train_x), model = xgb_model)
  grafik("07_xgb_degisken_onemi", function() {
    graphics::par(mar = c(5, 5, 4, 2) + 0.1)
    xgboost::xgb.plot.importance(importance_matrix, main = "XGBoost Değişken Önem Sıralaması")
  })

  test_x_simulated <- test_x
  test_x_simulated[, "PM10"] <- test_x_simulated[, "PM10"] * 1.10
  pred_simulated <- stats::predict(xgb_model, test_x_simulated)
  avg_original <- mean(pred_xgb)
  avg_simulated <- mean(pred_simulated)
  diff_percent <- ((avg_simulated - avg_original) / avg_original) * 100
  bilgi("\n--- Senaryo Analizi: PM10 %10 Artışı ---")
  bilgi("Orijinal Tahmin Ortalaması: ", format(avg_original))
  bilgi("Simüle Edilen Ortalama: ", format(avg_simulated))
  bilgi("Sağlık Skoru Üzerindeki Etki (%): ", format(diff_percent))

  # İlk ağacın çizimi etkileşimli bir HTML dosyasıdır (PNG değil).
  agac_dosyasi <- file.path(CALISMA$grafik_klasoru, "07_xgb_ilk_agac.html")
  tryCatch({
    htmlwidgets::saveWidget(xgb_agac_ciz(xgb_model), agac_dosyasi, selfcontained = FALSE)
    CALISMA$grafik_sayisi <- CALISMA$grafik_sayisi + 1
  }, error = function(e) {
    CALISMA$grafik_hatalari <- c(CALISMA$grafik_hatalari,
                                 paste0("07_xgb_ilk_agac: ", conditionMessage(e)))
    bilgi("UYARI: XGBoost ağaç çizimi kaydedilemedi: ", conditionMessage(e))
  })

  # SHAPforxgboost grafik etiketleri için paketin 'new_labels' veri nesnesini
  # kullanır; bu nesne yalnız paket library() ile yüklendiğinde görünür.
  suppressPackageStartupMessages(library(SHAPforxgboost))
  colnames(train_x) <- make.names(colnames(train_x))
  shap_values <- SHAPforxgboost::shap.values(xgb_model = xgb_model, X_train = train_x)
  bilgi("\nOrtalama mutlak SHAP değerleri:")
  print(shap_values$mean_shap_score)
  shap_long <- SHAPforxgboost::shap.prep(xgb_model = xgb_model, X_train = train_x)
  grafik("07_xgb_shap_ozeti", function() SHAPforxgboost::shap.plot.summary(shap_long))

  eval_log <- xgb_ogrenme_kaydi(xgb_model)
  grafik("07_xgb_ogrenme_egrisi", function() {
    ggplot(eval_log, aes(x = iter)) +
      geom_line(aes(y = train_rmse, color = "Eğitim Hatası")) +
      geom_line(aes(y = val_rmse, color = "Doğrulama Hatası")) +
      labs(title = "Hata Oranının İterasyonlara Göre Değişimi", y = "RMSE", x = "Ağaç Sayısı") +
      theme_minimal()
  })

  results <- data.frame(Gercek = test_y, Tahmin = pred_xgb)
  grafik("07_xgb_yogunluk_dagilimi", function() {
    ggplot(results) +
      geom_density(aes(x = Gercek, fill = "Gerçek"), alpha = 0.5) +
      geom_density(aes(x = Tahmin, fill = "Tahmin"), alpha = 0.5) +
      labs(title = "Gerçek ve Tahmin Edilen Değerlerin Yoğunluk Dağılımı") +
      theme_classic()
  })

  grafik("07_xgb_yakinsama_analizi", function() {
    plot(eval_log$iter, eval_log$train_rmse, type = "l", col = "blue",
         xlab = "Ağaç Sayısı", ylab = "RMSE", main = "Model Yakınsama Analizi")
    graphics::lines(eval_log$iter, eval_log$val_rmse, col = "red")
    graphics::legend("topright", legend = c("Eğitim", "Doğrulama"), col = c("blue", "red"), lty = 1)
  })

  grafik("07_xgb_yogunluk_kiyaslamasi", function() {
    plot(stats::density(test_y), main = "Gerçek vs Tahmin Yoğunluk Kıyaslaması",
         col = "black", lwd = 2)
    graphics::lines(stats::density(pred_xgb), col = "red", lwd = 2, lty = 2)
    graphics::legend("topright", legend = c("Gerçek", "Tahmin"), col = c("black", "red"), lty = c(1, 2))
  })

  grafik("07_xgb_dagilim_kiyaslamasi", function() {
    ggplot(results) +
      geom_density(aes(x = Gercek, fill = "Gerçek Veri"), alpha = 0.4) +
      geom_density(aes(x = Tahmin, fill = "Tahmin Edilen"), alpha = 0.4) +
      labs(title = "Gerçek ve Tahmin Edilen Değerlerin Dağılım Kıyaslaması") +
      theme_classic()
  })

  mape_val <- mean(abs((test_y - pred_xgb) / test_y)) * 100
  bilgi("Ortalama Mutlak Hata (MAE): ", format(mean(abs(test_y - pred_xgb))))
  bilgi("Ortalama Yüzdesel Hata (MAPE): %", format(mape_val))

  pdp_cardio <- pdp::partial(xgb_model, pred.var = "CardiovascularCases", train = train_x,
                             type = "regression")
  grafik("07_xgb_kismi_bagimlilik_cardiovascular", function() {
    pdp::plotPartial(pdp_cardio,
                     main = "CardiovascularCases Değişiminin Sağlık Skoru Üzerindeki Marjinal Etkisi",
                     xlab = "CardiovascularCases Değeri",
                     ylab = "Tahmin Edilen Health Impact Score",
                     col = "darkblue", lwd = 2)
  })

  sonuc
}
