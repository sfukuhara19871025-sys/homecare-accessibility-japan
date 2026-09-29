# ============================================================
# 03_calculate_osrm_minimum_travel_time.R
# Population-weighted accessibility analysis using OSRM
# 全人口メッシュ centroid に対する、全クリニックからの最短OSRM車移動時間の計算
#
# Input:
#   1) japan_population_mesh_centroids_for_osrm_2020_2070.csv
#      または japan_population_mesh_centroids_for_osrm.csv
#      必須列: mesh_id, shicode, centroid_lon, centroid_lat
#
#   2) japan_clinic_address_only_csv_matched.csv
#      必須列: fX, fY
#      fX = longitude, fY = latitude
#
# Output:
#   1) population_mesh_nearest_clinic_duration_by_osrm.csv
#      各メッシュに対する最短OSRM到達時間
#
#   2) population_mesh_arrival_zone_from_any_clinic_osrm.csv
#      到着圏分類付き全データ
#
#   3) population_mesh_summary_by_arrival_zone.csv
#      到着圏別メッシュ数集計
#
#   4) population_mesh_summary_cumulative_reachable_by_threshold.csv
#      15,30,45,60,75,90分以内の累積到達メッシュ数
#
#   5) population_weighted_accessibility_2020_2070_summary.csv
#      年次・人口種別ごとの人口加重アクセシビリティ
#
# Notes:
#   - このスクリプトは16km以内に限定しません。
#   - すべての人口メッシュ centroid について、全クリニックからの最短OSRM時間を計算します。
#   - 全ペアのlong tableは保存せず、各メッシュの最短時間だけを逐次更新します。
#   - OSRM serverは事前に起動しておいてください。
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
# 0. Package setup
# ============================================================

required_packages <- c("osrm", "data.table", "matrixStats")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop("Install required R packages before running: ", paste(missing_packages, collapse = ", "))
}

library(osrm)
library(data.table)
library(matrixStats)


# ============================================================
# 1. User settings
# ============================================================

# Working directory and OSRM server are defined in config/config.R.
work_dir <- cfg$work_dir
setwd(work_dir)

options(
  osrm.server = cfg$osrm_server,
  osrm.profile = cfg$osrm_profile_r
)

# 入力ファイル
# 将来人口列も含めて解析したい場合は、こちらを優先
centroid_file_primary <- cfg$centroid_file_full

# 2025年までの簡易版しかない場合のfallback
centroid_file_fallback <- cfg$centroid_file_2025

# 以前作った16km判定ファイルを使う場合のfallback
# 注意: このファイルが全メッシュを含むか確認すること。
centroid_file_fallback2 <- cfg$distance_16km_file

clinic_file_primary <- cfg$facility_file
clinic_file_fallback <- cfg$facility_file

# Docker側の --max-table-size に合わせる
# 例:
# docker run --rm -t -i \
#   --name osrm-japan \
#   -p 5001:5000 \
#   -v "$PWD:/data" \
#   ghcr.io/project-osrm/osrm-backend \
#   osrm-routed --algorithm mld --max-table-size 5000 /data/japan-latest.osrm
osrm_max_table_size <- cfg$osrm_max_table_size

# chunk設定
# src_chunk_size + dst_chunk_size <= osrm_max_table_size にする
src_chunk_size <- cfg$osrm_src_chunk_size   # facilities
dst_chunk_size <- cfg$osrm_dst_chunk_size   # centroids

stopifnot(src_chunk_size + dst_chunk_size <= osrm_max_table_size)

# 何jobごとにgc()と進捗表示をするか
gc_every_jobs <- 20

# 途中保存ファイル
progress_rds <- "population_mesh_nearest_clinic_duration_progress.rds"

# 出力ファイル
nearest_file <- "population_mesh_nearest_clinic_duration_by_osrm.csv"
arrival_file <- "population_mesh_arrival_zone_from_any_clinic_osrm.csv"
summary_zone_file <- "population_mesh_summary_by_arrival_zone.csv"
summary_cumulative_file <- "population_mesh_summary_cumulative_reachable_by_threshold.csv"
pwa_summary_file <- "population_weighted_accessibility_2020_2070_summary.csv"
pwa_by_pref_file <- "population_weighted_accessibility_2020_2070_by_pref.csv"


# ============================================================
# 2. Helper functions
# ============================================================

pick_existing_file <- function(files) {
  hit <- files[file.exists(files)]
  if (length(hit) == 0) {
    stop("候補ファイルが見つかりません: ", paste(files, collapse = ", "))
  }
  hit[1]
}

make_chunks <- function(n, size) {
  split(seq_len(n), ceiling(seq_len(n) / size))
}

safe_osrm_table <- function(src, dst, max_tries = 3) {
  last_err <- NULL
  
  for (i in seq_len(max_tries)) {
    res <- try(
      osrmTable(
        src = src,
        dst = dst,
        measure = "duration"
      ),
      silent = TRUE
    )
    
    if (!inherits(res, "try-error")) {
      return(res)
    }
    
    last_err <- res
    message("OSRM error. Retry ", i, " / ", max_tries)
    Sys.sleep(2^i)
  }
  
  stop(last_err)
}


# ============================================================
# 3. Read input data
# ============================================================

centroid_file <- pick_existing_file(c(
  centroid_file_primary,
  centroid_file_fallback,
  centroid_file_fallback2
))

clinic_file <- pick_existing_file(c(
  clinic_file_primary,
  clinic_file_fallback
))

message("Centroid file: ", centroid_file)
message("Clinic file: ", clinic_file)

centroids <- fread(centroid_file)
clinics <- fread(clinic_file)

# 必須列チェック
required_centroid_cols <- c("mesh_id", "shicode", "centroid_lon", "centroid_lat")
required_clinic_cols <- c("fX", "fY")

missing_centroid_cols <- setdiff(required_centroid_cols, names(centroids))
missing_clinic_cols <- setdiff(required_clinic_cols, names(clinics))

if (length(missing_centroid_cols) > 0) {
  stop("centroids側に以下の列がありません: ",
       paste(missing_centroid_cols, collapse = ", "))
}

if (length(missing_clinic_cols) > 0) {
  stop("clinics側に以下の列がありません: ",
       paste(missing_clinic_cols, collapse = ", "))
}

# ID付与
# 既存のcentroid_idやclinic_idがあっても、今回の行順に基づいて作り直す
centroids[, centroid_id := .I]
clinics[, clinic_id := .I]

# 座標欠損・日本域外の明らかな異常値を除外
# 全メッシュを対象にするため、within_16kmではfilterしない。
centroids <- centroids[
  !is.na(centroid_lon) &
    !is.na(centroid_lat) &
    centroid_lon >= 120 & centroid_lon <= 155 &
    centroid_lat >= 20 & centroid_lat <= 50
]

clinics <- clinics[
  !is.na(fX) &
    !is.na(fY) &
    fX >= 120 & fX <= 155 &
    fY >= 20 & fY <= 50
]

message("centroids: ", nrow(centroids))
message("clinics: ", nrow(clinics))

message(
  "total pairs: ",
  format(
    as.double(nrow(centroids)) * as.double(nrow(clinics)),
    big.mark = ",",
    scientific = FALSE
  )
)

#8,370,047,352

# 全人口メッシュ件数の目安確認
# 前処理ログ上では全国人口メッシュは466,792件
if (isTRUE(cfg$study_mode) && nrow(centroids) < 400000) {
  warning(
    "centroidsの件数が400,000未満です。全人口メッシュではなく、何らかのfilter済みファイルを読んでいる可能性があります。nrow = ",
    nrow(centroids)
  )
}


# ============================================================
# 4. Prepare OSRM coordinate tables
# ============================================================

centroids_xy <- as.data.frame(
  centroids[, .(
    lon = centroid_lon,
    lat = centroid_lat
  )]
)

clinics_xy <- as.data.frame(
  clinics[, .(
    lon = fX,
    lat = fY
  )]
)

rownames(centroids_xy) <- centroids$centroid_id
rownames(clinics_xy)   <- clinics$clinic_id


# ============================================================
# 5. Chunks and OSRM connection test
# ============================================================

src_chunks <- make_chunks(nrow(clinics_xy), src_chunk_size)
dst_chunks <- make_chunks(nrow(centroids_xy), dst_chunk_size)

message("src chunks: ", length(src_chunks))
message("dst chunks: ", length(dst_chunks))
message("total OSRM jobs: ", length(src_chunks) * length(dst_chunks))

message("Testing OSRM connection...")

test_tab <- osrmTable(
  src = clinics_xy[1:min(2, nrow(clinics_xy)), , drop = FALSE],
  dst = centroids_xy[1:min(3, nrow(centroids_xy)), , drop = FALSE],
  measure = "duration"
)

print(test_tab$durations)
message("OSRM connection test completed.")


# ============================================================
# 6. Main OSRM loop
#    各centroidに対して、全クリニックからの最短時間だけを逐次更新
# ============================================================

n_centroids <- nrow(centroids)
n_clinics <- nrow(clinics)

if (file.exists(progress_rds)) {
  message("既存の進捗RDSを読み込みます: ", progress_rds)
  
  progress <- readRDS(progress_rds)
  
  # 件数が違う場合は危険なので停止
  if (length(progress$best_duration) != n_centroids) {
    stop(
      "進捗RDSのcentroid件数と現在のcentroids件数が一致しません。",
      "最初からやり直す場合は progress_rds を削除してください: ",
      progress_rds
    )
  }
  
  best_duration <- progress$best_duration
  best_clinic <- progress$best_clinic
  completed_src_chunks <- progress$completed_src_chunks
  
} else {
  best_duration <- rep(Inf, n_centroids)
  best_clinic <- rep(NA_integer_, n_centroids)
  completed_src_chunks <- integer(0)
}

total_jobs <- length(src_chunks) * length(dst_chunks)
job <- 0L
start_time <- Sys.time()

for (s in seq_along(src_chunks)) {
  
  if (s %in% completed_src_chunks) {
    message("Skip completed clinic chunk ", s)
    job <- job + length(dst_chunks)
    next
  }
  
  src_i <- src_chunks[[s]]
  clinic_ids_src <- clinics$clinic_id[src_i]
  
  message(
    "\nClinic chunk ", s, " / ", length(src_chunks),
    " ; clinics ", min(src_i), "-", max(src_i),
    " ; ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  
  for (d in seq_along(dst_chunks)) {
    
    dst_i <- dst_chunks[[d]]
    job <- job + 1L
    
    message(
      "  Job ", job, " / ", total_jobs,
      " ; centroid chunk ", d, " / ", length(dst_chunks),
      " ; centroids ", min(dst_i), "-", max(dst_i)
    )
    
    tab <- tryCatch(
      {
        safe_osrm_table(
          src = clinics_xy[src_i, , drop = FALSE],
          dst = centroids_xy[dst_i, , drop = FALSE]
        )
      },
      error = function(e) {
        message("  ERROR: ", conditionMessage(e))
        return(NULL)
      }
    )
    
    if (is.null(tab)) {
      next
    }
    
    mat <- tab$durations
    # mat: rows = clinics, columns = centroids
    
    mat[is.na(mat)] <- Inf
    
    # このclinic chunk内で、各centroid列ごとの最短時間
    chunk_min <- matrixStats::colMins(mat)
    
    # このclinic chunk内で、各centroid列ごとの最短clinic行番号
    # max.col(-t(mat)) は列ごとのwhich.min相当
    chunk_which <- max.col(-t(mat), ties.method = "first")
    
    all_unreachable <- is.infinite(chunk_min)
    
    chunk_min[all_unreachable] <- NA_real_
    chunk_which[all_unreachable] <- NA_integer_
    
    candidate_duration <- chunk_min
    candidate_clinic <- clinic_ids_src[chunk_which]
    candidate_clinic[all_unreachable] <- NA_integer_
    
    # best_duration / best_clinic はcentroidsの行順と一致
    idx <- dst_i
    
    update_flag <- !is.na(candidate_duration) &
      candidate_duration < best_duration[idx]
    
    if (any(update_flag)) {
      update_idx <- idx[update_flag]
      best_duration[update_idx] <- candidate_duration[update_flag]
      best_clinic[update_idx] <- candidate_clinic[update_flag]
    }
    
    rm(tab, mat, chunk_min, chunk_which)
    
    if (job %% gc_every_jobs == 0) {
      gc()
      
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "hours"))
      jobs_per_hour <- job / elapsed
      remain_jobs <- total_jobs - job
      eta_hours <- remain_jobs / jobs_per_hour
      
      message(
        "  Progress: ", round(job / total_jobs * 100, 2), "%",
        " ; elapsed hours: ", round(elapsed, 2),
        " ; ETA hours: ", round(eta_hours, 2)
      )
    }
  }
  
  completed_src_chunks <- c(completed_src_chunks, s)
  
  saveRDS(
    list(
      best_duration = best_duration,
      best_clinic = best_clinic,
      completed_src_chunks = completed_src_chunks,
      src_chunk_size = src_chunk_size,
      dst_chunk_size = dst_chunk_size,
      osrm_max_table_size = osrm_max_table_size,
      centroid_file = centroid_file,
      clinic_file = clinic_file,
      saved_at = Sys.time()
    ),
    progress_rds,
    compress = FALSE
  )
  
  message("Saved progress after clinic chunk ", s)
}


# ============================================================
# 7. Save nearest OSRM duration result
# ============================================================

best_duration[is.infinite(best_duration)] <- NA_real_

nearest <- data.table(
  centroid_id = centroids$centroid_id,
  mesh_id = centroids$mesh_id,
  shicode = centroids$shicode,
  centroid_lon = centroids$centroid_lon,
  centroid_lat = centroids$centroid_lat,
  nearest_clinic_id = best_clinic,
  min_duration_min = best_duration
)

# クリニック座標を付与
clinic_info <- copy(clinics)
clinic_info[, clinic_lon := fX]
clinic_info[, clinic_lat := fY]

nearest <- merge(
  nearest,
  clinic_info[, .(
    nearest_clinic_id = clinic_id,
    nearest_clinic_lon = clinic_lon,
    nearest_clinic_lat = clinic_lat
  )],
  by = "nearest_clinic_id",
  all.x = TRUE
)

# mergeで順序が変わるため戻す
setorder(nearest, centroid_id)

fwrite(nearest, nearest_file)

message("Saved nearest file: ", nearest_file)
print(summary(nearest$min_duration_min))
message("reachable: ", sum(!is.na(nearest$min_duration_min)))
message("unreachable: ", sum(is.na(nearest$min_duration_min)))

# > print(summary(nearest$min_duration_min))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.00    3.40    6.20   10.27   11.00 4716.40      61 
# > message("reachable: ", sum(!is.na(nearest$min_duration_min)))
# reachable: 466731
# > message("unreachable: ", sum(is.na(nearest$min_duration_min)))
# unreachable: 61

# ============================================================
# 8. Arrival zone classification
# ============================================================

# 元のcentroids全列にnearest結果を結合
dat <- merge(
  centroids,
  nearest[, .(
    centroid_id,
    nearest_clinic_id,
    nearest_clinic_lon,
    nearest_clinic_lat,
    min_duration_min
  )],
  by = "centroid_id",
  all.x = TRUE
)

# 到着圏カテゴリ
dat[, arrival_zone := fcase(
  is.na(min_duration_min), "到達不可能圏",
  min_duration_min <= 15, "15分以内に到着可能",
  min_duration_min <= 30, "15分超30分以内に到着可能",
  min_duration_min <= 45, "30分超45分以内に到着可能",
  min_duration_min <= 60, "45分超60分以内に到着可能",
  min_duration_min <= 75, "60分超75分以内に到着可能",
  min_duration_min <= 90, "75分超90分以内に到着可能",
  default = "90分超到着にかかる"
)]

dat[, arrival_zone_order := fcase(
  arrival_zone == "15分以内に到着可能", 1L,
  arrival_zone == "15分超30分以内に到着可能", 2L,
  arrival_zone == "30分超45分以内に到着可能", 3L,
  arrival_zone == "45分超60分以内に到着可能", 4L,
  arrival_zone == "60分超75分以内に到着可能", 5L,
  arrival_zone == "75分超90分以内に到着可能", 6L,
  arrival_zone == "90分超到着にかかる", 7L,
  arrival_zone == "到達不可能圏", 8L
)]

# 累積到達フラグ
thresholds <- c(15, 30, 45, 60, 75, 90)

for (th in thresholds) {
  flag_col <- paste0("within_", th, "min")
  dat[, (flag_col) := !is.na(min_duration_min) & min_duration_min <= th]
}

# 保存
fwrite(dat, arrival_file)
message("Saved arrival zone file: ", arrival_file)

# 到着圏別集計
summary_zone <- dat[
  ,
  .(
    n_meshes = .N,
    percent = round(.N / nrow(dat) * 100, 2)
  ),
  by = .(arrival_zone_order, arrival_zone)
][order(arrival_zone_order)]

fwrite(summary_zone, summary_zone_file)
print(summary_zone)

# 累積到達集計
cumulative_summary <- data.table(
  threshold_min = thresholds,
  reachable_n = c(
    sum(dat$within_15min),
    sum(dat$within_30min),
    sum(dat$within_45min),
    sum(dat$within_60min),
    sum(dat$within_75min),
    sum(dat$within_90min)
  )
)

cumulative_summary[, total_n := nrow(dat)]
cumulative_summary[, not_reachable_n := total_n - reachable_n]
cumulative_summary[, reachable_percent := round(reachable_n / total_n * 100, 2)]
cumulative_summary[, not_reachable_percent := round(not_reachable_n / total_n * 100, 2)]

fwrite(cumulative_summary, summary_cumulative_file)
print(cumulative_summary)
# 
# > print(summary_zone)
# arrival_zone_order             arrival_zone n_meshes percent
# <int>                   <char>    <int>   <num>
#   1:                  1       15分以内に到着可能   402668   86.26
# 2:                  2 15分超30分以内に到着可能    54666   11.71
# 3:                  3 30分超45分以内に到着可能     7233    1.55
# 4:                  4 45分超60分以内に到着可能      902    0.19
# 5:                  5 60分超75分以内に到着可能      105    0.02
# 6:                  6 75分超90分以内に到着可能       63    0.01
# 7:                  7       90分超到着にかかる     1094    0.23
# 8:                  8             到達不可能圏       61    0.01
# > 
#   > # 累積到達集計
#   > cumulative_summary <- data.table(
#     +   threshold_min = thresholds,
#     +   reachable_n = c(
#       +     sum(dat$within_15min),
#       +     sum(dat$within_30min),
#       +     sum(dat$within_45min),
#       +     sum(dat$within_60min),
#       +     sum(dat$within_75min),
#       +     sum(dat$within_90min)
#       +   )
#     + )
# > 
#   > cumulative_summary[, total_n := nrow(dat)]
# > cumulative_summary[, not_reachable_n := total_n - reachable_n]
# > cumulative_summary[, reachable_percent := round(reachable_n / total_n * 100, 2)]
# > cumulative_summary[, not_reachable_percent := round(not_reachable_n / total_n * 100, 2)]
# > 
#   > fwrite(cumulative_summary, summary_cumulative_file)
# > print(cumulative_summary)
# threshold_min reachable_n total_n not_reachable_n reachable_percent
# <num>       <int>   <int>           <int>             <num>
#   1:            15      402668  466792           64124             86.26
# 2:            30      457334  466792            9458             97.97
# 3:            45      464567  466792            2225             99.52
# 4:            60      465469  466792            1323             99.72
# 5:            75      465574  466792            1218             99.74
# 6:            90      465637  466792            1155             99.75
# not_reachable_percent
# <num>
#   1:                 13.74
# 2:                  2.03
# 3:                  0.48
# 4:                  0.28
# 5:                  0.26
# 6:                  0.25
# > 
# 

# ============================================================
# 9. Population-weighted accessibility summary
#    2020, 2025, 2030, ..., 2070
# ============================================================

# 利用可能な人口列を自動検出
pop_specs <- data.table(
  year = integer(),
  pop_type = character(),
  col = character()
)

# 2020年は総人口のみ
if ("pop_total_2020" %in% names(dat)) {
  pop_specs <- rbind(
    pop_specs,
    data.table(year = 2020, pop_type = "total", col = "pop_total_2020")
  )
}

# 2025年以降
for (yy in seq(2025, 2070, by = 5)) {
  candidates <- data.table(
    year = yy,
    pop_type = c("total", "age65plus", "age75plus", "age80plus"),
    col = c(
      paste0("pop_total_", yy),
      paste0("pop_65plus_", yy),
      paste0("pop_75plus_", yy),
      paste0("pop_80plus_", yy)
    )
  )
  candidates <- candidates[col %in% names(dat)]
  pop_specs <- rbind(pop_specs, candidates)
}

if (nrow(pop_specs) == 0) {
  warning("人口加重集計に使えるpop_*列が見つかりません。PWA summaryは作成しません。")
} else {
  
  pwa_list <- vector("list", nrow(pop_specs))
  
  for (i in seq_len(nrow(pop_specs))) {
    yy <- pop_specs$year[i]
    pt <- pop_specs$pop_type[i]
    pc <- pop_specs$col[i]
    
    tmp <- dat[
      ,
      .(
        year = yy,
        pop_type = pt,
        population_total = sum(get(pc), na.rm = TRUE),
        
        population_within_15min = sum(get(pc)[within_15min], na.rm = TRUE),
        population_within_30min = sum(get(pc)[within_30min], na.rm = TRUE),
        population_within_45min = sum(get(pc)[within_45min], na.rm = TRUE),
        population_within_60min = sum(get(pc)[within_60min], na.rm = TRUE),
        population_within_75min = sum(get(pc)[within_75min], na.rm = TRUE),
        population_within_90min = sum(get(pc)[within_90min], na.rm = TRUE)
      )
    ]
    
    tmp[, pwa_15min := population_within_15min / population_total]
    tmp[, pwa_30min := population_within_30min / population_total]
    tmp[, pwa_45min := population_within_45min / population_total]
    tmp[, pwa_60min := population_within_60min / population_total]
    tmp[, pwa_75min := population_within_75min / population_total]
    tmp[, pwa_90min := population_within_90min / population_total]
    
    tmp[, population_not_within_30min := population_total - population_within_30min]
    tmp[, population_not_within_60min := population_total - population_within_60min]
    tmp[, population_not_within_90min := population_total - population_within_90min]
    
    pwa_list[[i]] <- tmp
  }
  
  pwa_summary <- rbindlist(pwa_list, use.names = TRUE, fill = TRUE)
  setorder(pwa_summary, year, pop_type)
  
  fwrite(pwa_summary, pwa_summary_file)
  print(pwa_summary)
  message("Saved PWA summary: ", pwa_summary_file)
  
  # 都道府県別集計
  dat[, pref_code := substr(as.character(shicode), 1, 2)]
  
  pwa_pref_list <- vector("list", nrow(pop_specs))
  
  for (i in seq_len(nrow(pop_specs))) {
    yy <- pop_specs$year[i]
    pt <- pop_specs$pop_type[i]
    pc <- pop_specs$col[i]
    
    tmp <- dat[
      ,
      .(
        year = yy,
        pop_type = pt,
        population_total = sum(get(pc), na.rm = TRUE),
        
        population_within_15min = sum(get(pc)[within_15min], na.rm = TRUE),
        population_within_30min = sum(get(pc)[within_30min], na.rm = TRUE),
        population_within_45min = sum(get(pc)[within_45min], na.rm = TRUE),
        population_within_60min = sum(get(pc)[within_60min], na.rm = TRUE),
        population_within_75min = sum(get(pc)[within_75min], na.rm = TRUE),
        population_within_90min = sum(get(pc)[within_90min], na.rm = TRUE)
      ),
      by = pref_code
    ]
    
    tmp[, pwa_15min := population_within_15min / population_total]
    tmp[, pwa_30min := population_within_30min / population_total]
    tmp[, pwa_45min := population_within_45min / population_total]
    tmp[, pwa_60min := population_within_60min / population_total]
    tmp[, pwa_75min := population_within_75min / population_total]
    tmp[, pwa_90min := population_within_90min / population_total]
    
    tmp[, population_not_within_30min := population_total - population_within_30min]
    tmp[, population_not_within_60min := population_total - population_within_60min]
    tmp[, population_not_within_90min := population_total - population_within_90min]
    
    pwa_pref_list[[i]] <- tmp
  }
  
  pwa_by_pref <- rbindlist(pwa_pref_list, use.names = TRUE, fill = TRUE)
  setorder(pwa_by_pref, pref_code, year, pop_type)
  
  fwrite(pwa_by_pref, pwa_by_pref_file)
  message("Saved PWA by prefecture: ", pwa_by_pref_file)
}

# year  pop_type population_total population_within_15min population_within_30min
# <num>    <char>            <num>                   <num>                   <num>
#   1:  2020     total        126146099               124563838               125918057
# 2:  2025 age65plus         36528902                35860579                36436210
# 3:  2025 age75plus         21546558                21150188                21492502
# 4:  2025 age80plus         13126356                12872036                13091590
# 5:  2025     total        123262450               121844501               123058595
# 6:  2030 age65plus         36961947                36341981                36876409
# 7:  2030 age75plus         22612951                22211990                22558283
# 8:  2030 age80plus         15444096                15176135                15407955
# 9:  2030     total        120115783               118846441               119933391
# 10:  2035 age65plus         37732157                37170639                37655019
# 11:  2035 age75plus         22383776                22001169                22331637
# 12:  2035 age80plus         16068141                15793343                16031027
# 13:  2035     total        116638900               115508396               116476302
# 14:  2040 age65plus         39284984                38775993                39215306
# 15:  2040 age75plus         22274970                21927008                22227752
# 16:  2040 age80plus         15621740                15361303                15586552
# 17:  2040     total        112837404               111836596               112692995
# 18:  2045 age65plus         39451493                38998838                39389596
# 19:  2045 age75plus         22771502                22466760                22730312
# 20:  2045 age80plus         15483447                15251627                15452244
# 21:  2045     total        108801339               107922503               108673744
# 22:  2050 age65plus         38878226                38481652                38823820
# 23:  2050 age75plus         24331533                24061229                24295171
# 24:  2050 age80plus         16115823                15917014                16089225
# 25:  2050     total        104686386               103919294               104574097
# 26:  2055 age65plus         37779232                37434284                37731471
# 27:  2055 age75plus         24790500                24550390                24758165
# 28:  2055 age80plus         17712975                17534424                17689180
# 29:  2055     total        100508401                99838171               100409249
# 30:  2060 age65plus         36437398                36137936                36395656
# 31:  2060 age75plus         24368296                24155920                24339545
# 32:  2060 age80plus         18064838                17904689                18043526
# 33:  2060     total         96147840                95561553                96060156
# 34:  2065 age65plus         35134222                34870680                35097322
# 35:  2065 age75plus         23162615                22977838                23137149
# 36:  2065 age80plus         17481356                17338920                17462139
# 37:  2065     total         91586504                91073098                91508501
# 38:  2070 age65plus         33671445                33438891                33638662
# 39:  2070 age75plus         21802114                21642780                21779793
# 40:  2070 age80plus         16320656                16197553                16303619
# 41:  2070     total         86996004                86546885                86926441
# year  pop_type population_total population_within_15min population_within_30min
# <num>    <char>            <num>                   <num>                   <num>
#   population_within_45min population_within_60min population_within_75min
# <num>                   <num>                   <num>
#   1:               126070465               126096199               126097684
# 2:                36498772                36509352                36510074
# 3:                21529075                21535353                21535761
# 4:                13115186                13119204                13119456
# 5:               123194468               123217265               123218558
# 6:                36934246                36943957                36944596
# 7:                22595195                22601523                22601956
# 8:                15432390                15436632                15436903
# 9:               120054482               120074754               120075868
# 10:                37707303                37715982                37716519
# 11:                22366933                22372890                22373296
# 12:                16056132                16060421                16060718
# 13:               116583700               116601604               116602550
# 14:                39262474                39270294                39270738
# 15:                22259879                22265181                22265523
# 16:                15610444                15614445                15614719
# 17:               112787746               112803430               112804219
# 18:                39431454                39438317                39438669
# 19:                22758426                22762974                22763236
# 20:                15473558                15477029                15477245
# 21:               108756659               108770310               108770964
# 22:                38860422                38866380                38866658
# 23:                24319871                24323895                24324095
# 24:                16107426                16110333                16110490
# 25:               104646139               104657980               104658520
# 26:                37763442                37768585                37768809
# 27:                24780044                24783608                24783756
# 28:                17705353                17707989                17708102
# 29:               100471973               100482309               100482752
# 30:                36423397                36427810                36427997
# 31:                24358858                24361978                24362094
# 32:                18057944                18060290                18060379
# 33:                96114761                96123759                96124122
# 34:                35121659                35125491                35125644
# 35:                23154173                23156860                23156965
# 36:                17475047                17477120                17477190
# 37:                91556232                91564110                91564412
# 38:                33660014                33663427                33663538
# 39:                21794598                21796892                21796975
# 40:                16315013                16316775                16316839
# 41:                86968138                86975028                86975274
# population_within_45min population_within_60min population_within_75min
# <num>                   <num>                   <num>
#   population_within_90min pwa_15min pwa_30min pwa_45min pwa_60min pwa_75min pwa_90min
# <num>     <num>     <num>     <num>     <num>     <num>     <num>
#   1:               126099239 0.9874569 0.9981922 0.9994004 0.9996044 0.9996162 0.9996285
# 2:                36510919 0.9817043 0.9974625 0.9991752 0.9994648 0.9994846 0.9995077
# 3:                21536286 0.9816040 0.9974912 0.9991886 0.9994800 0.9994989 0.9995233
# 4:                13119803 0.9806252 0.9973515 0.9991490 0.9994551 0.9994744 0.9995008
# 5:               123219924 0.9884965 0.9983462 0.9994485 0.9996334 0.9996439 0.9996550
# 6:                36945344 0.9832269 0.9976858 0.9992505 0.9995133 0.9995306 0.9995508
# 7:                22602483 0.9822685 0.9975825 0.9992148 0.9994946 0.9995138 0.9995371
# 8:                15437267 0.9826496 0.9976599 0.9992421 0.9995167 0.9995343 0.9995578
# 9:               120077047 0.9894323 0.9984815 0.9994897 0.9996584 0.9996677 0.9996775
# 10:                37717153 0.9851183 0.9979556 0.9993413 0.9995713 0.9995855 0.9996024
# 11:                22373780 0.9829070 0.9976707 0.9992475 0.9995136 0.9995318 0.9995534
# 12:                16061085 0.9828980 0.9976902 0.9992526 0.9995195 0.9995380 0.9995608
# 13:               116603547 0.9903077 0.9986060 0.9995267 0.9996802 0.9996884 0.9996969
# 14:                39271265 0.9870436 0.9982263 0.9994270 0.9996261 0.9996374 0.9996508
# 15:                22265924 0.9843788 0.9978802 0.9993225 0.9995605 0.9995759 0.9995939
# 16:                15615044 0.9833285 0.9977475 0.9992769 0.9995330 0.9995505 0.9995714
# 17:               112805050 0.9911305 0.9987202 0.9995599 0.9996989 0.9997059 0.9997133
# 18:                39439105 0.9885263 0.9984311 0.9994921 0.9996660 0.9996749 0.9996860
# 19:                22763551 0.9866174 0.9981911 0.9994258 0.9996255 0.9996370 0.9996508
# 20:                15477504 0.9850279 0.9979848 0.9993613 0.9995855 0.9995994 0.9996162
# 21:               108771655 0.9919226 0.9988273 0.9995893 0.9997148 0.9997208 0.9997272
# 22:                38867006 0.9897996 0.9986006 0.9995421 0.9996953 0.9997025 0.9997114
# 23:                24324347 0.9888908 0.9985056 0.9995207 0.9996861 0.9996943 0.9997047
# 24:                16110686 0.9876638 0.9983496 0.9994790 0.9996593 0.9996691 0.9996813
# 25:               104659085 0.9926725 0.9989274 0.9996155 0.9997287 0.9997338 0.9997392
# 26:                37769093 0.9908694 0.9987358 0.9995820 0.9997182 0.9997241 0.9997316
# 27:                24783968 0.9903145 0.9986957 0.9995782 0.9997220 0.9997280 0.9997365
# 28:                17708263 0.9899198 0.9986566 0.9995697 0.9997185 0.9997249 0.9997340
# 29:               100483222 0.9933316 0.9990135 0.9996376 0.9997404 0.9997448 0.9997495
# 30:                36428218 0.9917815 0.9988544 0.9996157 0.9997369 0.9997420 0.9997481
# 31:                24362266 0.9912847 0.9988202 0.9996127 0.9997407 0.9997455 0.9997525
# 32:                18060515 0.9911348 0.9988202 0.9996184 0.9997483 0.9997532 0.9997607
# 33:                96124517 0.9939022 0.9990880 0.9996560 0.9997495 0.9997533 0.9997574
# 34:                35125812 0.9924990 0.9989497 0.9996424 0.9997515 0.9997558 0.9997606
# 35:                23157103 0.9920226 0.9989005 0.9996355 0.9997515 0.9997561 0.9997620
# 36:                17477300 0.9918521 0.9989007 0.9996391 0.9997577 0.9997617 0.9997680
# 37:                91564750 0.9943943 0.9991483 0.9996695 0.9997555 0.9997588 0.9997625
# 38:                33663673 0.9930934 0.9990264 0.9996605 0.9997619 0.9997652 0.9997692
# 39:                21797080 0.9926918 0.9989762 0.9996553 0.9997605 0.9997643 0.9997691
# 40:                16316928 0.9924572 0.9989561 0.9996542 0.9997622 0.9997661 0.9997716
# 41:                86975561 0.9948375 0.9992004 0.9996797 0.9997589 0.9997617 0.9997650
# population_within_90min pwa_15min pwa_30min pwa_45min pwa_60min pwa_75min pwa_90min
# <num>     <num>     <num>     <num>     <num>     <num>     <num>
#   population_not_within_30min population_not_within_60min population_not_within_90min
# <num>                       <num>                       <num>
#   1:                   228041.91                   49899.704                   46860.188
# 2:                    92691.96                   19549.586                   17983.424
# 3:                    54055.54                   11204.827                   10272.142
# 4:                    34765.71                    7152.367                    6552.892
# 5:                   203854.90                   45185.495                   42526.249
# 6:                    85537.73                   17990.228                   16602.851
# 7:                    54667.92                   11428.313                   10467.727
# 8:                    36141.39                    7464.119                    6828.627
# 9:                   182391.85                   41029.190                   38735.520
# 10:                    77138.15                   16175.140                   15003.775
# 11:                    52138.69                   10886.514                    9995.739
# 12:                    37114.34                    7720.173                    7056.371
# 13:                   162597.85                   37296.145                   35353.270
# 14:                    69678.37                   14690.261                   13719.212
# 15:                    47217.98                    9788.967                    9046.017
# 16:                    35188.27                    7295.425                    6696.159
# 17:                   144408.52                   33973.664                   32354.045
# 18:                    61896.91                   13175.681                   12388.099
# 19:                    41190.39                    8527.592                    7951.030
# 20:                    31202.74                    6417.568                    5942.592
# 21:                   127594.65                   31029.544                   29683.966
# 22:                    54405.87                   11846.093                   11219.802
# 23:                    36361.65                    7638.433                    7186.167
# 24:                    26598.20                    5490.080                    5136.821
# 25:                   112288.76                   28406.008                   27301.000
# 26:                    47761.14                   10646.658                   10139.362
# 27:                    32334.58                    6892.159                    6532.402
# 28:                    23795.19                    4985.659                    4711.633
# 29:                    99152.45                   26091.736                   25178.604
# 30:                    41742.33                    9587.544                    9180.236
# 31:                    28750.60                    6318.135                    6030.387
# 32:                    21312.47                    4547.682                    4322.964
# 33:                    87683.77                   24080.962                   23322.612
# 34:                    36900.24                    8731.424                    8410.247
# 35:                    25466.25                    5755.095                    5512.338
# 36:                    19217.23                    4236.126                    4055.795
# 37:                    78003.42                   22394.035                   21753.954
# 38:                    32782.85                    8018.410                    7772.275
# 39:                    22321.03                    5222.206                    5033.714
# 40:                    17036.96                    3880.821                    3728.098
# 41:                    69563.49                   20976.023                   20442.897
# population_not_within_30min population_not_within_60min population_not_within_90min
# <num>                       <num>                       <num>


# ============================================================
# 10. Category-specific CSVs
# ============================================================

fwrite(
  dat[arrival_zone == "15分以内に到着可能"],
  "population_mesh_arrival_zone_00_15min.csv"
)

fwrite(
  dat[arrival_zone == "15分超30分以内に到着可能"],
  "population_mesh_arrival_zone_15_30min.csv"
)

fwrite(
  dat[arrival_zone == "30分超45分以内に到着可能"],
  "population_mesh_arrival_zone_30_45min.csv"
)

fwrite(
  dat[arrival_zone == "45分超60分以内に到着可能"],
  "population_mesh_arrival_zone_45_60min.csv"
)

fwrite(
  dat[arrival_zone == "60分超75分以内に到着可能"],
  "population_mesh_arrival_zone_60_75min.csv"
)

fwrite(
  dat[arrival_zone == "75分超90分以内に到着可能"],
  "population_mesh_arrival_zone_75_90min.csv"
)

fwrite(
  dat[arrival_zone == "90分超到着にかかる"],
  "population_mesh_arrival_zone_over_90min.csv"
)

fwrite(
  dat[arrival_zone == "到達不可能圏"],
  "population_mesh_arrival_zone_unreachable.csv"
)


# ============================================================
# 11. Final checks
# ============================================================

message("Final checks")
message("nrow(dat): ", nrow(dat))
message("classified total: ", sum(summary_zone$n_meshes))
message("arrival_zone NA: ", sum(is.na(dat$arrival_zone)))
message("reachable: ", sum(!is.na(dat$min_duration_min)))
message("unreachable: ", sum(is.na(dat$min_duration_min)))

print(summary(dat$min_duration_min))

message("Done.")

# 
# Final checks
# > message("nrow(dat): ", nrow(dat))
# nrow(dat): 466792
# > message("classified total: ", sum(summary_zone$n_meshes))
# classified total: 466792
# > message("arrival_zone NA: ", sum(is.na(dat$arrival_zone)))
# arrival_zone NA: 0
# > message("reachable: ", sum(!is.na(dat$min_duration_min)))
# reachable: 466731
# > message("unreachable: ", sum(is.na(dat$min_duration_min)))
# unreachable: 61
# > 
#   > print(summary(dat$min_duration_min))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.00    3.40    6.20   10.27   11.00 4716.40      61 