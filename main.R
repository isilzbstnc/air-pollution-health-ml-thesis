# Air pollution and health impact: runs the whole analysis with one command.
# Hava kirliligi ve saglik etkisi: tum analizi tek komutla calistirir.
#
# Terminal:
#   Rscript main.R                     veri/air_quality_health_impact_data.csv
#   Rscript main.R --veri=YOL          baska bir CSV yolu
#   Rscript main.R --svr-ayar          SVR parametre aramasini (tune.svm) da calistir
#   Rscript main.R --kurma             eksik paketleri otomatik kurma, yalniz listele
#   Rscript main.R --yardim
#
# RStudio: bu dosyayi acip "Source" dugmesine basin. Ayarlari asagidan degistirebilirsiniz.
# Ciktilar: ciktilar/ klasoru (grafikler, metrikler.csv, rapor.txt).

AYARLAR <- list(
  veri = NULL,        # NULL ise veri/air_quality_health_impact_data.csv kullanilir
  svr_ayar = FALSE,   # TRUE: SVR icin tune.svm aramasi da calisir
  paket_kur = TRUE    # TRUE: eksik paketler CRAN'dan otomatik kurulur
)

proje_koku <- local({
  argumanlar <- commandArgs(trailingOnly = FALSE)
  dosya <- sub("^--file=", "", argumanlar[startsWith(argumanlar, "--file=")])
  if (length(dosya) == 1) {
    dirname(normalizePath(dosya))
  } else {
    kaynak <- NULL
    for (i in rev(seq_len(sys.nframe()))) {
      of <- sys.frame(i)$ofile
      if (!is.null(of)) { kaynak <- of; break }
    }
    if (!is.null(kaynak)) dirname(normalizePath(kaynak)) else getwd()
  }
})

if (!file.exists(file.path(proje_koku, "R", "00_ortak.R"))) {
  stop("R/ klasoru bulunamadi (", proje_koku, "). main.R'yi proje klasorunden calistirin.",
       call. = FALSE)
}

# Kod dosyalari Turkce karakter iceren UTF-8 metinlerdir. Sistem dil ayari UTF-8
# degilse (or. Linux'ta C/POSIX) once UTF-8'e gecilir; dosyalar da ayar ne olursa
# olsun UTF-8 olarak okunur.
if (!isTRUE(l10n_info()[["UTF-8"]]) && .Platform$OS.type != "windows") {
  for (yerel in c("C.UTF-8", "C.utf8", "en_US.UTF-8", "tr_TR.UTF-8")) {
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", yerel)))) break
  }
}

kaynak_yukle <- function(dosya) {
  satirlar <- readLines(dosya, encoding = "UTF-8", warn = FALSE)
  eval(parse(text = satirlar, encoding = "UTF-8", keep.source = FALSE), envir = globalenv())
  invisible(NULL)
}

kaynak_yukle(file.path(proje_koku, "R", "00_ortak.R"))
analizi_calistir(proje_koku, AYARLAR, commandArgs(trailingOnly = TRUE))
