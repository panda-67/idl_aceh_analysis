# Analisis Imunisasi Dasar Lengkap (IDL)

Proyek ini menganalisis data imunisasi dari berbagai kecamatan untuk
mengevaluasi cakupan vaksin, tren tahunan, dan distribusi per wilayah.

## Struktur Folder

- `data/` — Dataset ODS mentah
- `output/plots/` — Gambar hasil visualisasi
- `output/tables/` — Hasil analisis (CSV, tabel)
- `reports/` - Hasil analisis
- `scripts/` — Skrip utama analisis
- `R/` — Setup dan fungsi pendukung

## Cara Menjalankan

1. Buka file `.Rproj` ini di RStudio.
2. Dibagian console, Jalankan skrip:

```r
source("scripts/main_analysis.R")
```

3. Lalu, lihat hasil analisis di dalam folder reports, dapat berupa halaman
   html dan pdf file pada foldernya masing-masing.
