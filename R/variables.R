extra_dates <- "Tanggal Lahir Anak"

sheets <- list(
  jaya_baru       = "Copy of Jaya Baru (verified)",
  baiturrahman    = "Copy of Baiturrahman (verified)",
  darussalam      = "Copy of Darussalam (verified)",
  mesjid_raya     = "Copy of Mesjid Raya (verified)",
  meuraxa         = "Copy of Meuraxa (verified)",
  kuta_malaka     = "Copy of Kuta Malaka (verified)",
  kbj             = "Copy of KBJ (Verified)"
)

# JE => "JE"
vaccine_groups <- list(
  HB0 = "HB-0",
  BCG = "BCG",
  OPV = c("OPV 1", "OPV 2", "OPV 3", "OPV 4"),
  DTP = c(
    "DTP-HB-Hib 1", "DTP-HB-Hib 2", "DTP-HB-Hib 3", "DTP-HB-Hib 4"
  ),
  MR = c("MR 1", "MR 2"),
  IPV = c("IPV 1", "IPV 2"),
  PCV = c("PCV 1", "PCV 2", "PCV 3"),
  RV = c("RV 1", "RV 2", "RV 3")
)

vaccine_order <- c(
  "HB-0", "BCG", "OPV 1", "DTP-HB-Hib 1", "OPV 2", "PCV 1", "RV 1",
  "DTP-HB-Hib 2", "OPV 3", "PCV 2", "RV 2", "DTP-HB-Hib 3", "OPV 4", "PCV 3",
  "RV 3", "MR 1", "IPV 1", "IPV 2", "DTP-HB-Hib 4", "MR 2"
)

required_vaccines <- c(
  "HB-0", "BCG", "OPV 1", "DTP-HB-Hib 1", "OPV 2",
  "DTP-HB-Hib 2", "OPV 3", "DTP-HB-Hib 3", "OPV 4",
  "MR 1", "IPV 1"
)

required_cols <- c(
  "Nama Anak", "Kecamatan", "dob",
  as.character(unlist(vaccine_groups)),
  "HB0_doses", "BCG_doses", "OPV_doses", "IPV_doses",
  "DTP_doses", "MR_doses", "PCV_doses", "RV_doses",
  "region", "district", "treatment_duration", "kecamatan",
  "IDL_year", "idl_percent", "idl_status"
)

region_data <- tibble(
  kecamatan = c(
    "montasik", # 1
    "kuta_alam", # 2
    "lueng_bata", # 3
    "meuraxa", # 4
    "kuta_malaka", # 5
    "darussalam", # 6
    "kbj", # 7
    "kuta_raja", # 8
    "peukan_bada", # 9
    "jaya_baru", # 10
    "baiturrahman", # 11
    "mesjid_raya", # 12
    "darul_imarah", # 13
    "darul_kamal", # 14
    "blang_bintang", # 15
    "syiah_kuala", # 16
    "ulee_kareng", # 17
    "banda_raya" # 18
  ),
  district = c(
    "aceh_besar", # montasik
    "banda_aceh", # kuta_alam
    "banda_aceh", # lueng_bata
    "banda_aceh", # meuraxa
    "aceh_besar", # kuta_malaka
    "aceh_besar", # darussalam
    "aceh_besar", # kbj
    "banda_aceh", # kuta_raja
    "aceh_besar", # peukan_bada
    "banda_aceh", # jaya_baru
    "banda_aceh", # baiturrahman
    "aceh_besar", # mesjid_raya
    "aceh_besar", # darul_imarah
    "aceh_besar", # darul_kamal
    "aceh_besar", # blang_bintang
    "banda_aceh", # syiah_kuala
    "banda_aceh", # ulee_kareng
    "banda_aceh" # banda_raya
  ),
  treatment_group = c(
    "RC_one_year", # montasik
    "RC_one_year", # kuta_alam
    "RC_one_year", # lueng_bata
    "RC_two_year", # meuraxa
    "RC_two_year", # kuta_malaka
    "RC_two_year", # darussalam
    "VIV_one_year", # kbj
    "VIV_one_year", # kuta_raja
    "VIV_one_year", # peukan_bada
    "VIV_two_year", # jaya_baru
    "VIV_two_year", # baiturrahman
    "VIV_two_year", # mesjid_raya
    "control", # darul_imarah
    "control", # darul_kamal
    "control", # blang_bintang
    "control", # syiah_kuala
    "control", # ulee_kareng
    "control" # banda_raya
  )
)

region_groups <- list(
  RC = list(
    one_year = region_data$kecamatan[
      region_data$treatment_group == "RC_one_year"
    ],
    two_year = region_data$kecamatan[
      region_data$treatment_group == "RC_two_year"
    ]
  ),
  VIV = list(
    one_year = region_data$kecamatan[
      region_data$treatment_group == "VIV_one_year"
    ],
    two_year = region_data$kecamatan[
      region_data$treatment_group == "VIV_two_year"
    ]
  ),
  control = region_data$kecamatan[region_data$treatment_group == "control"],
  banda_aceh = region_data$kecamatan[region_data$district == "banda_aceh"],
  aceh_besar = region_data$kecamatan[region_data$district == "aceh_besar"]
)
