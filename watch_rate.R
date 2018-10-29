# 視聴率について

## setup ----

# ライブラリ
library(tidyverse)
library(lubridate)
library(dbplyr)
library(dbplot)
library(RPostgreSQL)
library(ggforce)
library(DT)

# 作図オプション
theme_set(theme_bw(base_family = "HiraKakuPro-W3"))

# データベース接続
conn <- dbConnect(PostgreSQL(),
                  host = scan("connection.txt", what = character())[1],
                  port = scan("connection.txt", what = character())[2], 
                  dbname = scan("connection.txt", what = character())[3], 
                  user = scan("connection.txt", what = character())[4], 
                  password = scan("connection.txt", what = character())[5])

# データインポート
## ログ
orgn2 <- conn %>% 
  tbl(from = dbplyr::in_schema("edit", "tv_orgn_program_2")) 
orgn <- conn %>% 
  tbl(from = dbplyr::in_schema("processed", "tv_orgn_p_cv")) 
play <- conn %>% 
  tbl(from = dbplyr::in_schema("processed", "tv_play_p_cv"))
web <- conn %>% 
  tbl(from = dbplyr::in_schema("processed", "t_weblog"))
## CM
jiten <- conn %>% 
  tbl(from = dbplyr::in_schema("processed", "jiten_data"))
brand <- conn %>% 
  tbl(from = dbplyr::in_schema("processed", "m_cm_tv_brand"))
adv <- conn %>% 
  tbl(from = dbplyr::in_schema("processed", "m_cm_tv_advertiser"))
## その他マスタ
program <- conn %>% 
  tbl(from = dbplyr::in_schema("processed", "tv_program")) 
ban <- conn %>% 
  tbl(from = dbplyr::in_schema("processed", "bancluster_mst")) 
ban1 <- conn %>% 
  tbl(from = dbplyr::in_schema("sub_mst", "ban_code1_mst")) 
sta <- conn %>% 
  tbl(from = dbplyr::in_schema("processed", "sta_mst"))
sample <- conn %>% 
  tbl(from = dbplyr::in_schema("processed", "tv_sample_p_cv"))
job <- conn %>% 
  tbl(from = dbplyr::in_schema("processed", "job_mst"))
## アンケート
prof_data <- conn %>% 
  tbl(from = dbplyr::in_schema("processed", "profiledata"))
prof_mst <- conn %>% 
  tbl(from = dbplyr::in_schema("processed", "profilemaster"))
prof <- prof_data %>% 
  left_join(prof_mst, by = c("qu_genre_code", "question_code", "answer_code"))


# リアルタイムの集計 ----
# リアルタイムの方については簡単
# 以下のコードの後に，放送日の標本数で割る必要あり
realtime_tmp <- orgn %>% 
  filter(data_agg_type == 1) %>% 
  select(house_num, station_code, br_date, br_start_datetime, br_end_datetime)
  
#タイムシフトの集計 ----
# orgnに統合
# realtimeとtimeshiftの重複を削除
# timeshiftで何度も見てるやつを削除

timeshift <- play %>% 
  distinct(house_num, timeshift_datetime, station_code, timeshift_date) %>% 
  left_join(realtime_tmp, by = c("house_num", "station_code", "timeshift_date" = "br_date" )) %>% #
  mutate(flag1 = br_start_datetime <= timeshift_datetime,
         flag2 = br_end_datetime >= timeshift_datetime) %>% 
  mutate(flag3 = flag1 & flag2) %>% 
  group_by(house_num, station_code, timeshift_datetime) %>% 
  filter(sum(as.integer(flag3)) == 0) %>% 
  ungroup() %>% 
  distinct(house_num, station_code, timeshift_datetime, timeshift_date) %>% 
  left_join(program, by = c("station_code", "timeshift_date" = "program_date")) %>% 
  filter(program_start_time < timeshift_datetime &&  timeshift_datetime < program_end_time) %>% 
  group_by(station_code, program_start_time, program_time) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(n / as.double(program_time))

# realtime
