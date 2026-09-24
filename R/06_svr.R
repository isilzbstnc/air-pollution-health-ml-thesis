# Destek vektör regresyonu (radyal çekirdek, cost = 10, epsilon = 0.1).
# İsteğe bağlı: --svr-ayar ile tune.svm parametre araması da çalışır.

model_svr <- function(veri, ayarlar) {
  b <- veri_bol(veri, "cdp")
  train_set <- b$egitim
  test_set <- b$test

  svr_model <- e1071::svm(HealthImpactScore ~ ., data = train_set, type = "eps-regression",
                          kernel = "radial", cost = 10, epsilon = 0.1)
  pred_svr <- stats::predict(svr_model, test_set)
  sonuc <- model_sonucu("SVR", test_set$HealthImpactScore, pred_svr, "cdp")

  results_svr <- data.frame(Gercek = test_set$HealthImpactScore, Tahmin = pred_svr)
  grafik("06_svr_gercek_vs_tahmin", function() {
    ggplot(results_svr, aes(x = Gercek, y = Tahmin)) +
      geom_point(color = "darkgreen", alpha = 0.5) +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      labs(title = "SVR: Gerçek vs Tahmin Değerleri",
           x = "Gerçek Health Impact Score", y = "Tahmin Edilen Health Impact Score") +
      theme_minimal()
  })

  test_set_simulated <- test_set
  test_set_simulated$PM10 <- test_set_simulated$PM10 * 1.10
  pred_simulated_svr <- stats::predict(svr_model, test_set_simulated)
  avg_orig_svr <- mean(pred_svr)
  avg_sim_svr <- mean(pred_simulated_svr)
  diff_percent_svr <- ((avg_sim_svr - avg_orig_svr) / avg_orig_svr) * 100
  bilgi("\n--- SVR Senaryo Analizi: PM10 %10 Artışı ---")
  bilgi("Orijinal Tahmin Ortalaması: ", format(avg_orig_svr))
  bilgi("Simüle Edilen Ortalama: ", format(avg_sim_svr))
  bilgi("Sağlık Skoru Üzerindeki Etki (%): ", format(diff_percent_svr))

  residuals_svr <- test_set$HealthImpactScore - pred_svr
  grafik("06_svr_artik_analizi", function() {
    ggplot(data.frame(Tahmin = pred_svr, Artik = residuals_svr), aes(x = Tahmin, y = Artik)) +
      geom_point(color = "steelblue", alpha = 0.6) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "SVR Artık (Residual) Analizi", x = "Tahmin Edilen Değerler",
           y = "Hatalar (Artıklar)") +
      theme_minimal()
  })

  grafik("06_svr_hata_dagilimi", function() {
    ggplot(data.frame(Artik = residuals_svr), aes(x = Artik)) +
      geom_histogram(bins = 30, fill = "darkgreen", color = "white", alpha = 0.7) +
      labs(title = "SVR Hata Dağılımı", x = "Hata Miktarı", y = "Frekans") +
      theme_minimal()
  })

  bilgi("Toplam Destek Vektörü Sayısı: ", svr_model$tot.nSV)

  grafik("06_svr_dagilim_kiyaslamasi", function() {
    ggplot() +
      geom_density(aes(x = test_set$HealthImpactScore, fill = "Gerçek"), alpha = 0.4) +
      geom_density(aes(x = pred_svr, fill = "SVR Tahmin"), alpha = 0.4) +
      labs(title = "Gerçek vs SVR Tahmin Dağılımı", x = "Sağlık Skoru", y = "Yoğunluk") +
      scale_fill_manual(values = c("Gerçek" = "gray", "SVR Tahmin" = "green")) +
      theme_classic()
  })

  relative_error <- abs(test_set$HealthImpactScore - pred_svr) / test_set$HealthImpactScore
  bilgi("SVR Ortalama Yüzde Hata: %", format(mean(relative_error) * 100))

  # Parametre araması: eğitim verisinin 2/3'ü eğitim, 1/3'ü doğrulama.
  # Özgün kodda sampling/fix doğrudan verildiği için yok sayılıyor ve 10-katlı
  # çapraz doğrulama yapılıyordu (~250 sn); doğru yeri tunecontrol'dür.
  # Bulunan parametreler raporlanır; yukarıdaki model tezdeki gibi sabit kalır.
  if (isTRUE(ayarlar$svr_ayar)) {
    bilgi("\n--- SVR Parametre Araması (tune.svm) ---")
    obj <- e1071::tune.svm(HealthImpactScore ~ ., data = train_set,
                           cost = c(0.1, 1, 10, 100), epsilon = c(0, 0.1, 0.5),
                           tunecontrol = e1071::tune.control(sampling = "fix", fix = 2 / 3))
    print(summary(obj))
    grafik("06_svr_parametre_aramasi", function() plot(obj))
  } else {
    bilgi("\n(SVR parametre araması kapalı; açmak için: Rscript main.R --svr-ayar)")
  }

  sonuc
}
