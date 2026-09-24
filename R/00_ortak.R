# Ortak ayarlar, paket kontrolü ve yardımcı fonksiyonlar.
# Bu dosya main.R tarafından yüklenir; tek başına çalıştırılmaz.

TOHUM <- 100
HEDEF <- "HealthImpactScore"
VERI_DOSYA_ADI <- "air_quality_health_impact_data.csv"
KAGGLE_ADRESI <- "https://www.kaggle.com/datasets/rabieelkharoua/air-quality-and-health-impact-dataset"

GEREKLI_PAKETLER <- c(
  "dplyr", "tidyr", "ggplot2", "patchwork", "caret", "corrplot", "car",
  "lm.beta", "glmnet", "randomForest", "rpart", "rpart.plot", "e1071",
  "xgboost", "SHAPforxgboost", "pdp", "DiagrammeR", "htmlwidgets"
)

# Çalışma boyunca paylaşılan durum (çıktı klasörü, grafik sayacı, hatalar).
CALISMA <- new.env()

bilgi <- function(...) cat(..., "\n", sep = "")

baslik <- function(metin) {
  cat("\n", strrep("=", 70), "\n", metin, "\n", strrep("=", 70), "\n", sep = "")
}

# --- Paketler ---------------------------------------------------------------

eksik_paketler <- function(paketler = GEREKLI_PAKETLER) {
  paketler[!vapply(paketler, requireNamespace, logical(1), quietly = TRUE)]
}

paketleri_hazirla <- function(kur = TRUE) {
  eksik <- eksik_paketler()
  if (length(eksik) == 0) return(invisible(TRUE))

  bilgi("Eksik paketler: ", paste(eksik, collapse = ", "))
  komut <- paste0("install.packages(c(", paste0('"', eksik, '"', collapse = ", "), "))")
  if (!kur) {
    stop("Eksik paketler var. R'da şu komutla kurabilirsiniz:\n  ", komut, call. = FALSE)
  }

  # Rscript'te CRAN aynası tanımlı olmayabilir ("@CRAN@"); gerekirse eklenir.
  depo <- getOption("repos")
  cran <- if (!is.null(depo) && "CRAN" %in% names(depo)) depo[["CRAN"]] else ""
  if (!nzchar(cran) || identical(cran, "@CRAN@")) {
    depo <- depo[names(depo) != "CRAN"]
    options(repos = c(depo, CRAN = "https://cloud.r-project.org"))
  }

  # Sistem kütüphanesine yazma izni yoksa kullanıcı kütüphanesine kurulur.
  hedef <- .libPaths()[1]
  if (file.access(hedef, 2) != 0) {
    kullanici <- path.expand(Sys.getenv("R_LIBS_USER"))
    if (nzchar(kullanici)) {
      dir.create(kullanici, recursive = TRUE, showWarnings = FALSE)
      .libPaths(c(kullanici, .libPaths()))
      hedef <- kullanici
    }
  }

  bilgi("Eksik paketler CRAN'dan kuruluyor (yalnız ilk çalıştırmada)...")
  utils::install.packages(eksik, lib = hedef)

  hala_eksik <- eksik_paketler(eksik)
  if (length(hala_eksik) > 0) {
    stop("Şu paketler kurulamadı: ", paste(hala_eksik, collapse = ", "),
         "\nİnternet bağlantısını kontrol edip elle deneyin:\n  ", komut, call. = FALSE)
  }
  invisible(TRUE)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# --- Metrikler ----------------------------------------------------------------
# Tüm modellerde aynı tanımlar kullanılır (özgün dosyalardaki gibi R² = korelasyonun karesi).

metrikler <- function(gercek, tahmin) {
  gercek <- as.numeric(gercek)
  tahmin <- as.numeric(tahmin)
  c(
    R2 = stats::cor(gercek, tahmin)^2,
    RMSE = sqrt(mean((gercek - tahmin)^2)),
    MAE = mean(abs(gercek - tahmin))
  )
}

model_sonucu <- function(ad, gercek, tahmin, bolme) {
  m <- metrikler(gercek, tahmin)
  bilgi(sprintf("\n--- %s: test sonuçları (n = %d) ---", ad, length(gercek)))
  bilgi(sprintf("R-Kare : %.4f", m[["R2"]]))
  bilgi(sprintf("RMSE   : %.4f", m[["RMSE"]]))
  bilgi(sprintf("MAE    : %.4f", m[["MAE"]]))
  list(ad = ad, gercek = as.numeric(gercek), tahmin = as.numeric(tahmin),
       metrikler = m, bolme = bolme)
}

# --- Grafikler ----------------------------------------------------------------
# Her grafik ciktilar/grafikler/ altına PNG olarak kaydedilir. Bir grafik
# çizilemezse analiz durmaz; hata kaydedilir ve çalışmanın sonunda listelenir.

png_ac <- function(dosya, genislik, yukseklik) {
  tip <- if (Sys.info()[["sysname"]] == "Linux" && isTRUE(capabilities("cairo"))) "cairo" else NULL
  args <- list(filename = dosya, width = genislik, height = yukseklik, units = "in", res = 150)
  if (!is.null(tip)) args$type <- tip
  do.call(grDevices::png, args)
  grDevices::dev.cur()
}

grafik <- function(ad, ciz, genislik = 9, yukseklik = 6) {
  dosya <- file.path(CALISMA$grafik_klasoru, paste0(ad, ".png"))
  cihaz <- NULL
  basarili <- tryCatch({
    cihaz <- png_ac(dosya, genislik, yukseklik)
    nesne <- ciz()
    if (inherits(nesne, c("ggplot", "gg", "patchwork", "trellis"))) print(nesne)
    TRUE
  }, error = function(e) {
    CALISMA$grafik_hatalari <- c(CALISMA$grafik_hatalari,
                                 paste0(ad, ": ", conditionMessage(e)))
    bilgi("UYARI: '", ad, "' grafiği çizilemedi: ", conditionMessage(e))
    FALSE
  })
  if (!is.null(cihaz) && cihaz %in% grDevices::dev.list()) grDevices::dev.off(cihaz)
  if (basarili) {
    CALISMA$grafik_sayisi <- CALISMA$grafik_sayisi + 1
  } else if (file.exists(dosya)) {
    unlink(dosya)
  }
  invisible(basarili)
}

# --- Çalıştırma ---------------------------------------------------------------

arguman_oku <- function(argumanlar, ayarlar) {
  for (a in argumanlar) {
    if (startsWith(a, "--veri=")) {
      ayarlar$veri <- sub("^--veri=", "", a)
    } else if (a == "--svr-ayar") {
      ayarlar$svr_ayar <- TRUE
    } else if (a == "--kurma") {
      ayarlar$paket_kur <- FALSE
    } else if (a %in% c("-h", "--yardim", "--help")) {
      ayarlar$yardim <- TRUE
    } else {
      stop("Bilinmeyen argüman: ", a, "\nKullanım için: Rscript main.R --yardim", call. = FALSE)
    }
  }
  ayarlar
}

YARDIM_METNI <- "
Kullanım:
  Rscript main.R                    veri/air_quality_health_impact_data.csv dosyasını kullanır
  Rscript main.R --veri=YOL         başka bir CSV yolu verir
  Rscript main.R --svr-ayar         SVR için parametre aramasını (tune.svm) da çalıştırır
  Rscript main.R --kurma            eksik paketleri otomatik kurmaz, yalnız listeler

Çıktılar ciktilar/ klasörüne yazılır (grafikler, metrikler.csv, rapor.txt).
"

MODEL_ADIMLARI <- list(
  list(ad = "Lineer Regresyon", fonk = "model_lineer_regresyon"),
  list(ad = "Lasso ve Ridge",   fonk = "model_lasso_ridge"),
  list(ad = "KNN",              fonk = "model_knn"),
  list(ad = "Random Forest",    fonk = "model_random_forest"),
  list(ad = "SVR",              fonk = "model_svr"),
  list(ad = "XGBoost",          fonk = "model_xgboost")
)

analizi_calistir <- function(proje_koku, ayarlar, argumanlar = character(0)) {
  ayarlar <- arguman_oku(argumanlar, ayarlar)
  if (isTRUE(ayarlar$yardim)) {
    cat(YARDIM_METNI)
    return(invisible(NULL))
  }

  paketleri_hazirla(kur = isTRUE(ayarlar$paket_kur))
  suppressPackageStartupMessages({
    library(dplyr)
    library(ggplot2)
  })

  for (dosya in c("01_veri.R", "02_lineer_regresyon.R", "03_lasso_ridge.R", "04_knn.R",
                  "05_random_forest.R", "06_svr.R", "07_xgboost.R", "08_karsilastirma.R")) {
    kaynak_yukle(file.path(proje_koku, "R", dosya))
  }

  veri_yolu <- ayarlar$veri %||% file.path(proje_koku, "veri", VERI_DOSYA_ADI)
  ham <- veri_oku(veri_yolu)

  CALISMA$cikti_klasoru <- file.path(proje_koku, "ciktilar")
  CALISMA$grafik_klasoru <- file.path(CALISMA$cikti_klasoru, "grafikler")
  unlink(CALISMA$grafik_klasoru, recursive = TRUE)
  dir.create(CALISMA$grafik_klasoru, recursive = TRUE, showWarnings = FALSE)
  CALISMA$grafik_sayisi <- 0
  CALISMA$grafik_hatalari <- character(0)

  rapor <- file.path(CALISMA$cikti_klasoru, "rapor.txt")
  sink(rapor, split = TRUE)
  on.exit(sink(), add = TRUE)
  baslangic <- Sys.time()

  baslik("HAVA KİRLİLİĞİ VE SAĞLIK ETKİSİ: MODEL ANALİZİ")
  bilgi("Veri dosyası : ", normalizePath(veri_yolu))
  bilgi("R sürümü     : ", R.version.string)
  bilgi("xgboost      : ", as.character(utils::packageVersion("xgboost")))
  bilgi("SVR ayarı    : ", if (isTRUE(ayarlar$svr_ayar)) "açık (tune.svm çalışacak)" else "kapalı")

  veri <- veri_hazirla(ham)

  sonuclar <- list()
  model_hatalari <- character(0)
  for (adim in MODEL_ADIMLARI) {
    baslik(toupper(adim$ad))
    t0 <- Sys.time()
    sonuc <- tryCatch(
      get(adim$fonk)(veri, ayarlar),
      error = function(e) {
        bilgi("HATA: ", adim$ad, " çalışmadı: ", conditionMessage(e))
        model_hatalari <<- c(model_hatalari, paste0(adim$ad, ": ", conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(sonuc)) {
      if (!is.null(sonuc$ad)) sonuc <- list(sonuc)
      for (s in sonuc) sonuclar[[s$ad]] <- s
    }
    bilgi(sprintf("(%s süresi: %.1f sn)", adim$ad, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }

  baslik("MODEL KARŞILAŞTIRMASI")
  if (length(sonuclar) > 0) {
    karsilastir(sonuclar)
  } else {
    bilgi("Hiçbir model sonuç üretmedi; karşılaştırma atlandı.")
  }

  baslik("ÖZET")
  bilgi(sprintf("Toplam süre      : %.1f sn", as.numeric(difftime(Sys.time(), baslangic, units = "secs"))))
  bilgi("Çalışan modeller : ", length(sonuclar), " / 7")
  bilgi("Kaydedilen grafik: ", CALISMA$grafik_sayisi, " (", CALISMA$grafik_klasoru, ")")
  bilgi("Rapor            : ", rapor)
  if (length(CALISMA$grafik_hatalari) > 0) {
    bilgi("\nUYARI: ", length(CALISMA$grafik_hatalari), " grafik çizilemedi:")
    for (h in CALISMA$grafik_hatalari) bilgi("  - ", h)
  }
  if (length(model_hatalari) > 0) {
    bilgi("\nHATA: ", length(model_hatalari), " model adımı çalışmadı:")
    for (h in model_hatalari) bilgi("  - ", h)
    sink()
    on.exit(NULL)
    if (!interactive()) quit(status = 1, save = "no")
    stop("Bazı model adımları çalışmadı (ayrıntı yukarıda).", call. = FALSE)
  }
  bilgi("\nTüm adımlar tamamlandı.")
  invisible(sonuclar)
}
