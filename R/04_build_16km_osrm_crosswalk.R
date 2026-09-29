# ============================================================
# 04_build_16km_osrm_crosswalk.R
# Public consolidated version of the row-preserving 16-km x OSRM crosswalk.
# ============================================================

# Public repository configuration ---------------------------------------------
config_file <- Sys.getenv("HOMECARE_CONFIG", unset = file.path("config", "config.R"))
if (!file.exists(config_file)) {
  stop("Configuration file not found: ", config_file,
       "\nRun scripts from the repository root or set HOMECARE_CONFIG.")
}
source(config_file)
work_dir <- cfg$work_dir
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
setwd(work_dir)

# ============================================================
# 16km rule × OSRM travel time cross-analysis v4
# 行数保持版：
#   - OSRM完成ファイル側の行数を必ず保持する
#   - nrow(dat) が nrow(dosrm) = 466,792 になることを強制確認する
#   - mesh_idの重複で unique() して行を落とさない
#   - 原則として mesh_id + centroid_lon + centroid_lat の複合キーで結合
#   - 複合キーで結合できない場合、両ファイルが同じ行順・同じ件数なら row_id で結合
#   - 30分・60分の4象限分類、人口加重集計、都道府県別集計を作成
# ============================================================


# ============================================================
# 0. Packages
# ============================================================

required_packages <- c("data.table")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop("Install required R packages before running: ", paste(missing_packages, collapse = ", "))
}

library(data.table)


# ============================================================
# 1. User settings
# ============================================================

work_dir <- cfg$work_dir
setwd(work_dir)

file_16km <- cfg$distance_16km_file
file_osrm <- cfg$osrm_arrival_file

thresholds <- cfg$osrm_thresholds_extended

out_master <- cfg$crosswalk_master_file
out_mesh_summary <- "policy_crosswalk_16km_osrm_mesh_summary_v4.csv"
out_pwa_summary <- "policy_crosswalk_16km_osrm_population_weighted_summary_v4.csv"
out_pwa_by_pref <- "policy_crosswalk_16km_osrm_population_weighted_by_pref_v4.csv"
out_mismatch <- "policy_crosswalk_16km_osrm_mismatch_cases_v4.csv"

# 座標キー作成時の丸め桁数
# 500mメッシュcentroidなので7桁で十分厳密。
coord_digits <- cfg$coordinate_key_digits


# ============================================================
# 2. Read data
# ============================================================

d16 <- fread(file_16km)
dosrm <- fread(file_osrm)

cat("16km file rows:", nrow(d16), "\n")
cat("OSRM file rows:", nrow(dosrm), "\n")

expected_n <- nrow(dosrm)


# ============================================================
# 3. Required column checks
# ============================================================

required_16 <- c(
  "mesh_id",
  "shicode",
  "centroid_lon",
  "centroid_lat",
  "nearest_distance_km",
  "within_16km"
)

required_osrm <- c(
  "mesh_id",
  "shicode",
  "centroid_lon",
  "centroid_lat",
  "min_duration_min"
)

missing_16 <- setdiff(required_16, names(d16))
missing_osrm <- setdiff(required_osrm, names(dosrm))

if (length(missing_16) > 0) {
  stop("16km file is missing columns: ", paste(missing_16, collapse = ", "))
}

if (length(missing_osrm) > 0) {
  stop("OSRM file is missing columns: ", paste(missing_osrm, collapse = ", "))
}


# ============================================================
# 4. Key formatting
# ============================================================

# 行順を保存
d16[, row_id_16km := .I]
dosrm[, row_id_osrm := .I]

# 型を統一
d16[, mesh_id := as.character(mesh_id)]
dosrm[, mesh_id := as.character(mesh_id)]

d16[, shicode := as.character(shicode)]
dosrm[, shicode := as.character(shicode)]

# within_16km を logical に統一
if (!is.logical(d16$within_16km)) {
  d16[, within_16km := tolower(as.character(within_16km)) %in% c("true", "t", "1", "yes")]
}

# 座標をnumericに統一
d16[, centroid_lon := as.numeric(centroid_lon)]
d16[, centroid_lat := as.numeric(centroid_lat)]
dosrm[, centroid_lon := as.numeric(centroid_lon)]
dosrm[, centroid_lat := as.numeric(centroid_lat)]

# 複合キー作成
d16[
  ,
  join_key := paste0(
    mesh_id, "_",
    sprintf(paste0("%.", coord_digits, "f"), centroid_lon), "_",
    sprintf(paste0("%.", coord_digits, "f"), centroid_lat)
  )
]

dosrm[
  ,
  join_key := paste0(
    mesh_id, "_",
    sprintf(paste0("%.", coord_digits, "f"), centroid_lon), "_",
    sprintf(paste0("%.", coord_digits, "f"), centroid_lat)
  )
]


# ============================================================
# 5. Duplicate and matching diagnostics
# ============================================================

dup16_key <- d16[, .N, by = join_key][N > 1]
duposrm_key <- dosrm[, .N, by = join_key][N > 1]

dup16_mesh <- d16[, .N, by = mesh_id][N > 1]
duposrm_mesh <- dosrm[, .N, by = mesh_id][N > 1]

cat("duplicated join_key in 16km file:", nrow(dup16_key), "\n")
cat("duplicated join_key in OSRM file:", nrow(duposrm_key), "\n")
cat("duplicated mesh_id in 16km file:", nrow(dup16_mesh), "\n")
cat("duplicated mesh_id in OSRM file:", nrow(duposrm_mesh), "\n")

if (nrow(dup16_key) > 0) fwrite(dup16_key, "check_duplicated_join_key_in_16km_file_v4.csv")
if (nrow(duposrm_key) > 0) fwrite(duposrm_key, "check_duplicated_join_key_in_osrm_file_v4.csv")
if (nrow(dup16_mesh) > 0) fwrite(dup16_mesh, "check_duplicated_mesh_id_in_16km_file_v4.csv")
if (nrow(duposrm_mesh) > 0) fwrite(duposrm_mesh, "check_duplicated_mesh_id_in_osrm_file_v4.csv")


# ============================================================
# 6. Choose join method
# ============================================================

# 原則：join_keyが両方一意ならjoin_keyで結合
use_key_join <- (nrow(dup16_key) == 0 && nrow(duposrm_key) == 0)

# 複合キーで結合できない場合、両ファイルの行数が同じならrow_idで結合
# これは、同じ元データから作成され、行順が保たれている場合のfallback。
use_row_join <- FALSE

if (!use_key_join) {
  if (nrow(d16) == nrow(dosrm)) {
    use_row_join <- TRUE
    warning(
      "join_keyに重複があるため、row_idによる結合に切り替えます。",
      "両ファイルが同じ元データ・同じ行順で作成されていることを前提にします。"
    )
  } else {
    stop(
      "join_keyに重複があり、かつ両ファイルの行数が一致しないため、安全に結合できません。",
      "check_duplicated_*_v4.csv を確認してください。"
    )
  }
}

cat("Join method:", ifelse(use_key_join, "join_key", "row_id"), "\n")


# ============================================================
# 7. Prepare columns
# ============================================================

d16_keep <- d16[
  ,
  .(
    row_id_16km,
    join_key,
    mesh_id_16km = mesh_id,
    shicode_16km = shicode,
    centroid_lon_16km = centroid_lon,
    centroid_lat_16km = centroid_lat,
    nearest_distance_km,
    nearest_distance_m = if ("nearest_distance_m" %in% names(d16)) nearest_distance_m else NA_real_,
    within_16km,
    nearest_clinic_lon_16km = if ("nearest_clinic_lon" %in% names(d16)) nearest_clinic_lon else NA_real_,
    nearest_clinic_lat_16km = if ("nearest_clinic_lat" %in% names(d16)) nearest_clinic_lat else NA_real_
  )
]

osrm_base_cols <- c(
  "row_id_osrm",
  "join_key",
  "mesh_id",
  "shicode",
  "centroid_lon",
  "centroid_lat",
  "nearest_clinic_id",
  "nearest_clinic_lon",
  "nearest_clinic_lat",
  "min_duration_min",
  "arrival_zone",
  "arrival_zone_order",
  paste0("within_", thresholds, "min")
)

osrm_base_cols <- osrm_base_cols[osrm_base_cols %in% names(dosrm)]

pop_cols_osrm <- grep("^pop_", names(dosrm), value = TRUE)

dosrm_keep <- dosrm[
  ,
  c(osrm_base_cols, pop_cols_osrm),
  with = FALSE
]


# ============================================================
# 8. Merge while preserving OSRM rows
# ============================================================

if (use_key_join) {

  dat <- merge(
    dosrm_keep,
    d16_keep,
    by = "join_key",
    all.x = TRUE,
    sort = FALSE
  )

} else {

  # row_id fallback
  d16_keep[, row_id_osrm := row_id_16km]

  dat <- merge(
    dosrm_keep,
    d16_keep[
      ,
      !c("join_key"),
      with = FALSE
    ],
    by = "row_id_osrm",
    all.x = TRUE,
    sort = FALSE
  )
}

# OSRM側の行順に戻す
setorder(dat, row_id_osrm)

cat("Merged rows:", nrow(dat), "\n")
cat("Expected OSRM rows:", expected_n, "\n")

if (nrow(dat) != expected_n) {
  stop(
    "nrow(dat) が OSRM完成ファイルの行数と一致しません。nrow(dat)=",
    nrow(dat),
    "; expected=",
    expected_n
  )
}

# 16km側がマッチしているか確認
cat("missing within_16km after merge:", sum(is.na(dat$within_16km)), "\n")

if (sum(is.na(dat$within_16km)) > 0) {
  warning("within_16kmが欠損している行があります。check_unmatched_osrm_rows_v4.csv を保存します。")
  fwrite(
    dat[is.na(within_16km)],
    "check_unmatched_osrm_rows_v4.csv"
  )
}

# shicodeとcentroidを補完
dat[is.na(shicode) & !is.na(shicode_16km), shicode := shicode_16km]
dat[is.na(centroid_lon) & !is.na(centroid_lon_16km), centroid_lon := centroid_lon_16km]
dat[is.na(centroid_lat) & !is.na(centroid_lat_16km), centroid_lat := centroid_lat_16km]


# ============================================================
# 9. Add missing population columns from 16km file if needed
# ============================================================

pop_cols_16 <- grep("^pop_", names(d16), value = TRUE)
missing_pop_from_osrm <- setdiff(pop_cols_16, names(dat))

if (length(missing_pop_from_osrm) > 0) {

  if (use_key_join) {
    d16_pop <- d16[, c("join_key", missing_pop_from_osrm), with = FALSE]
    dat <- merge(dat, d16_pop, by = "join_key", all.x = TRUE, sort = FALSE)
  } else {
    d16_pop <- d16[, c("row_id_16km", missing_pop_from_osrm), with = FALSE]
    d16_pop[, row_id_osrm := row_id_16km]
    dat <- merge(dat, d16_pop[, !c("row_id_16km"), with = FALSE], by = "row_id_osrm", all.x = TRUE, sort = FALSE)
  }

  setorder(dat, row_id_osrm)

  if (nrow(dat) != expected_n) {
    stop("人口列追加後にnrow(dat)が変化しました。")
  }
}


# ============================================================
# 10. Create time flags if missing
# ============================================================

for (th in thresholds) {
  flag_col <- paste0("within_", th, "min")
  if (!flag_col %in% names(dat)) {
    dat[, (flag_col) := !is.na(min_duration_min) & min_duration_min <= th]
  }
}


# ============================================================
# 11. Policy cross-classification: 30min and 60min
# ============================================================

dat[, policy_quadrant_30min := fcase(
  within_16km == TRUE & within_30min == TRUE,
  "A_16km以内かつ30分以内：制度上も実態上も良好",

  within_16km == TRUE & within_30min == FALSE,
  "B_16km以内だが30分超：距離上は対象内だが実態上は遠い",

  within_16km == FALSE & within_30min == TRUE,
  "C_16km超だが30分以内：制度上は制限されるが実態上は近い",

  within_16km == FALSE & within_30min == FALSE,
  "D_16km超かつ30分超：制度上も実態上もアクセス困難",

  default = "Unknown"
)]

dat[, policy_quadrant_order_30min := fcase(
  grepl("^A_", policy_quadrant_30min), 1L,
  grepl("^B_", policy_quadrant_30min), 2L,
  grepl("^C_", policy_quadrant_30min), 3L,
  grepl("^D_", policy_quadrant_30min), 4L,
  default = 99L
)]

dat[, policy_quadrant_60min := fcase(
  within_16km == TRUE & within_60min == TRUE,
  "A_16km以内かつ60分以内",

  within_16km == TRUE & within_60min == FALSE,
  "B_16km以内だが60分超",

  within_16km == FALSE & within_60min == TRUE,
  "C_16km超だが60分以内",

  within_16km == FALSE & within_60min == FALSE,
  "D_16km超かつ60分超",

  default = "Unknown"
)]

dat[, policy_quadrant_order_60min := fcase(
  grepl("^A_", policy_quadrant_60min), 1L,
  grepl("^B_", policy_quadrant_60min), 2L,
  grepl("^C_", policy_quadrant_60min), 3L,
  grepl("^D_", policy_quadrant_60min), 4L,
  default = 99L
)]


# ============================================================
# 12. Mesh-count summary
# ============================================================

mesh_summary_30 <- dat[
  ,
  .(
    n_meshes = .N,
    percent_meshes = round(.N / nrow(dat) * 100, 4),
    mean_distance_km = mean(nearest_distance_km, na.rm = TRUE),
    median_distance_km = median(nearest_distance_km, na.rm = TRUE),
    mean_duration_min = mean(min_duration_min, na.rm = TRUE),
    median_duration_min = median(min_duration_min, na.rm = TRUE)
  ),
  by = .(
    policy_quadrant_order = policy_quadrant_order_30min,
    policy_quadrant = policy_quadrant_30min
  )
][order(policy_quadrant_order)]

mesh_summary_30[, threshold := "30min"]

mesh_summary_60 <- dat[
  ,
  .(
    n_meshes = .N,
    percent_meshes = round(.N / nrow(dat) * 100, 4),
    mean_distance_km = mean(nearest_distance_km, na.rm = TRUE),
    median_distance_km = median(nearest_distance_km, na.rm = TRUE),
    mean_duration_min = mean(min_duration_min, na.rm = TRUE),
    median_duration_min = median(min_duration_min, na.rm = TRUE)
  ),
  by = .(
    policy_quadrant_order = policy_quadrant_order_60min,
    policy_quadrant = policy_quadrant_60min
  )
][order(policy_quadrant_order)]

mesh_summary_60[, threshold := "60min"]

mesh_summary <- rbind(mesh_summary_30, mesh_summary_60, use.names = TRUE, fill = TRUE)
setcolorder(mesh_summary, c("threshold", setdiff(names(mesh_summary), "threshold")))

fwrite(mesh_summary, out_mesh_summary)
print(mesh_summary)


# ============================================================
# 13. Population-weighted cross-classification summary
# ============================================================

pop_cols <- grep("^pop_", names(dat), value = TRUE)

parse_pop_col <- function(x) {
  parts <- strsplit(x, "_")[[1]]
  year <- suppressWarnings(as.integer(parts[length(parts)]))
  pop_type <- paste(parts[2:(length(parts)-1)], collapse = "_")
  data.table(col = x, pop_type = pop_type, year = year)
}

if (length(pop_cols) == 0) {
  warning("No pop_* columns found. Population-weighted summary will not be created.")
} else {

  pop_specs <- rbindlist(lapply(pop_cols, parse_pop_col), fill = TRUE)
  pop_specs <- pop_specs[!is.na(year)]
  setorder(pop_specs, year, pop_type)

  pwa_list <- list()
  idx <- 1L

  for (i in seq_len(nrow(pop_specs))) {
    pc <- pop_specs$col[i]
    yy <- pop_specs$year[i]
    pt <- pop_specs$pop_type[i]

    tmp30 <- dat[
      ,
      .(
        population = sum(get(pc), na.rm = TRUE)
      ),
      by = .(
        policy_quadrant_order = policy_quadrant_order_30min,
        policy_quadrant = policy_quadrant_30min
      )
    ]
    tmp30[, threshold := "30min"]
    tmp30[, year := yy]
    tmp30[, pop_type := pt]
    tmp30[, total_population := sum(population, na.rm = TRUE)]
    tmp30[, population_share := population / total_population]
    pwa_list[[idx]] <- tmp30
    idx <- idx + 1L

    tmp60 <- dat[
      ,
      .(
        population = sum(get(pc), na.rm = TRUE)
      ),
      by = .(
        policy_quadrant_order = policy_quadrant_order_60min,
        policy_quadrant = policy_quadrant_60min
      )
    ]
    tmp60[, threshold := "60min"]
    tmp60[, year := yy]
    tmp60[, pop_type := pt]
    tmp60[, total_population := sum(population, na.rm = TRUE)]
    tmp60[, population_share := population / total_population]
    pwa_list[[idx]] <- tmp60
    idx <- idx + 1L
  }

  pwa_summary <- rbindlist(pwa_list, use.names = TRUE, fill = TRUE)
  setcolorder(
    pwa_summary,
    c(
      "threshold",
      "year",
      "pop_type",
      "policy_quadrant_order",
      "policy_quadrant",
      "population",
      "total_population",
      "population_share"
    )
  )
  setorder(pwa_summary, threshold, year, pop_type, policy_quadrant_order)

  fwrite(pwa_summary, out_pwa_summary)
  print(head(pwa_summary, 20))
}


# ============================================================
# 14. Prefecture-level population-weighted summary
# ============================================================

dat[, pref_code := substr(as.character(shicode), 1, 2)]

if (length(pop_cols) > 0) {

  pwa_pref_list <- list()
  idx <- 1L

  for (i in seq_len(nrow(pop_specs))) {
    pc <- pop_specs$col[i]
    yy <- pop_specs$year[i]
    pt <- pop_specs$pop_type[i]

    tmp30 <- dat[
      ,
      .(
        population = sum(get(pc), na.rm = TRUE)
      ),
      by = .(
        pref_code,
        policy_quadrant_order = policy_quadrant_order_30min,
        policy_quadrant = policy_quadrant_30min
      )
    ]
    tmp30[, threshold := "30min"]
    tmp30[, year := yy]
    tmp30[, pop_type := pt]
    tmp30[, total_population := sum(population, na.rm = TRUE), by = .(pref_code, threshold, year, pop_type)]
    tmp30[, population_share := population / total_population]
    pwa_pref_list[[idx]] <- tmp30
    idx <- idx + 1L

    tmp60 <- dat[
      ,
      .(
        population = sum(get(pc), na.rm = TRUE)
      ),
      by = .(
        pref_code,
        policy_quadrant_order = policy_quadrant_order_60min,
        policy_quadrant = policy_quadrant_60min
      )
    ]
    tmp60[, threshold := "60min"]
    tmp60[, year := yy]
    tmp60[, pop_type := pt]
    tmp60[, total_population := sum(population, na.rm = TRUE), by = .(pref_code, threshold, year, pop_type)]
    tmp60[, population_share := population / total_population]
    pwa_pref_list[[idx]] <- tmp60
    idx <- idx + 1L
  }

  pwa_by_pref <- rbindlist(pwa_pref_list, use.names = TRUE, fill = TRUE)
  setcolorder(
    pwa_by_pref,
    c(
      "pref_code",
      "threshold",
      "year",
      "pop_type",
      "policy_quadrant_order",
      "policy_quadrant",
      "population",
      "total_population",
      "population_share"
    )
  )
  setorder(pwa_by_pref, pref_code, threshold, year, pop_type, policy_quadrant_order)

  fwrite(pwa_by_pref, out_pwa_by_pref)
}


# ============================================================
# 15. Mismatch case extraction
# ============================================================

mismatch_cases <- dat[
  grepl("^B_", policy_quadrant_30min) | grepl("^C_", policy_quadrant_30min)
]

front_cols <- c(
  "mesh_id",
  "shicode",
  "pref_code",
  "centroid_lon",
  "centroid_lat",
  "within_16km",
  "nearest_distance_km",
  "min_duration_min",
  "policy_quadrant_30min",
  "arrival_zone",
  "nearest_clinic_id",
  "nearest_clinic_lon",
  "nearest_clinic_lat"
)

front_cols <- front_cols[front_cols %in% names(mismatch_cases)]
other_cols <- setdiff(names(mismatch_cases), front_cols)

mismatch_cases <- mismatch_cases[, c(front_cols, other_cols), with = FALSE]
fwrite(mismatch_cases, out_mismatch)

# 各象限別CSV
fwrite(
  dat[grepl("^A_", policy_quadrant_30min)],
  "policy_crosswalk_16km_osrm_quadrant_A_16km_within_30min_v4.csv"
)

fwrite(
  dat[grepl("^B_", policy_quadrant_30min)],
  "policy_crosswalk_16km_osrm_quadrant_B_16km_over_30min_v4.csv"
)

fwrite(
  dat[grepl("^C_", policy_quadrant_30min)],
  "policy_crosswalk_16km_osrm_quadrant_C_over16km_within_30min_v4.csv"
)

fwrite(
  dat[grepl("^D_", policy_quadrant_30min)],
  "policy_crosswalk_16km_osrm_quadrant_D_over16km_over_30min_v4.csv"
)


# ============================================================
# 16. Save master file
# ============================================================

front_cols_master <- c(
  "row_id_osrm",
  "mesh_id",
  "shicode",
  "pref_code",
  "centroid_lon",
  "centroid_lat",
  "within_16km",
  "nearest_distance_km",
  "min_duration_min",
  "arrival_zone",
  "policy_quadrant_30min",
  "policy_quadrant_order_30min",
  "policy_quadrant_60min",
  "policy_quadrant_order_60min",
  "nearest_clinic_id",
  "nearest_clinic_lon",
  "nearest_clinic_lat"
)

front_cols_master <- front_cols_master[front_cols_master %in% names(dat)]
other_cols_master <- setdiff(names(dat), front_cols_master)

dat_out <- dat[, c(front_cols_master, other_cols_master), with = FALSE]
fwrite(dat_out, out_master)


# ============================================================
# 17. Final checks
# ============================================================

cat("\nFinal checks\n")
cat("nrow(dosrm):", nrow(dosrm), "\n")
cat("nrow(d16):", nrow(d16), "\n")
cat("nrow(dat):", nrow(dat), "\n")
cat("nrow(dat) == nrow(dosrm):", nrow(dat) == nrow(dosrm), "\n")
cat("missing within_16km:", sum(is.na(dat$within_16km)), "\n")
cat("missing min_duration_min:", sum(is.na(dat$min_duration_min)), "\n")
cat("missing policy_quadrant_30min:", sum(is.na(dat$policy_quadrant_30min)), "\n")

if (nrow(dat) != nrow(dosrm)) {
  stop("最終確認で nrow(dat) != nrow(dosrm) です。")
}

cat("\n30min policy quadrant table:\n")
print(dat[, .N, by = .(policy_quadrant_order_30min, policy_quadrant_30min)][order(policy_quadrant_order_30min)])

cat("\n60min policy quadrant table:\n")
print(dat[, .N, by = .(policy_quadrant_order_60min, policy_quadrant_60min)][order(policy_quadrant_order_60min)])

cat("\nSaved files:\n")
cat(out_master, "\n")
cat(out_mesh_summary, "\n")
cat(out_pwa_summary, "\n")
cat(out_pwa_by_pref, "\n")
cat(out_mismatch, "\n")

cat("\nDone.\n")

# 
# Final checks
# > cat("nrow(dosrm):", nrow(dosrm), "\n")
# nrow(dosrm): 466792 
# > cat("nrow(d16):", nrow(d16), "\n")
# nrow(d16): 466792 
# > cat("nrow(dat):", nrow(dat), "\n")
# nrow(dat): 466792 
# > cat("nrow(dat) == nrow(dosrm):", nrow(dat) == nrow(dosrm), "\n")
# nrow(dat) == nrow(dosrm): TRUE 
# > cat("missing within_16km:", sum(is.na(dat$within_16km)), "\n")
# missing within_16km: 0 
# > cat("missing min_duration_min:", sum(is.na(dat$min_duration_min)), "\n")
# missing min_duration_min: 61 
# > cat("missing policy_quadrant_30min:", sum(is.na(dat$policy_quadrant_30min)), "\n")
# missing policy_quadrant_30min: 0 
# > 
#   > if (nrow(dat) != nrow(dosrm)) {
#     +   stop("最終確認で nrow(dat) != nrow(dosrm) です。")
#     + }
# > 
#   > cat("\n30min policy quadrant table:\n")
# 
# 30min policy quadrant table:
#   > print(dat[, .N, by = .(policy_quadrant_order_30min, policy_quadrant_30min)][order(policy_quadrant_order_30min)])
# policy_quadrant_order_30min
# <int>
#   1:                           1
# 2:                           2
# 3:                           3
# 4:                           4
# policy_quadrant_30min      N
# <char>  <int>
#   1:           A_16km以内かつ30分以内：制度上も実態上も良好 446321
# 2:   B_16km以内だが30分超：距離上は対象内だが実態上は遠い   2143
# 3: C_16km超だが30分以内：制度上は制限されるが実態上は近い  11013
# 4:       D_16km超かつ30分超：制度上も実態上もアクセス困難   7315
# > 
#   > cat("\n60min policy quadrant table:\n")
# 
# 60min policy quadrant table:
#   > print(dat[, .N, by = .(policy_quadrant_order_60min, policy_quadrant_60min)][order(policy_quadrant_order_60min)])
# policy_quadrant_order_60min  policy_quadrant_60min      N
# <int>                 <char>  <int>
#   1:                           1 A_16km以内かつ60分以内 448124
# 2:                           2   B_16km以内だが60分超    340
# 3:                           3   C_16km超だが60分以内  17345
# 4:                           4     D_16km超かつ60分超    983
# > 
#   > cat("\nSaved files:\n")
# 
# Saved files:
#   > cat(out_master, "\n")
# policy_crosswalk_16km_osrm_master_v4.csv 
# > cat(out_mesh_summary, "\n")
# policy_crosswalk_16km_osrm_mesh_summary_v4.csv 
# > cat(out_pwa_summary, "\n")
# policy_crosswalk_16km_osrm_population_weighted_summary_v4.csv 
# > cat(out_pwa_by_pref, "\n")
# policy_crosswalk_16km_osrm_population_weighted_by_pref_v4.csv 
# > cat(out_mismatch, "\n")
# policy_crosswalk_16km_osrm_mismatch_cases_v4.csv 
# > 
#   > cat("\nDone.\n")
# 
# Done.