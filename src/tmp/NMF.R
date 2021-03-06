
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
library(NMF)
library(fastICA)
library(doParallel)

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


# TF-IDF ----
# テキスト数を視聴者数で置き換え
# 単語の頻度を視聴時間で置き換え
# tf

tf <- dbGetQuery(conn,
           "SELECT
  program_code,
  house_num,
  sum(watch_time) as watch_time
FROM
  edit.tv_orgn_program_2
GROUP BY
  house_num, program_code") %>% 
  as_tibble()

idf <- dbGetQuery(conn,
                  "SELECT DISTINCT program_code, house_num
                  FROM
                    edit.tv_orgn_program_2") %>% 
  as_tibble()
tf <- tf %>% 
  group_by(house_num) %>% 
  mutate(tf = watch_time / sum(watch_time)) %>% 
  select(-watch_time) %>% 
  ungroup()
idf <- idf %>% 
  count(program_code)
tf_idf <- tf %>% 
  left_join(idf, by = "program_code") %>% 
  transmute(house_num, program_code, 
            tf_idf = tf * log1p(5000 / n))


mat <- tf_idf %>% 
  drop_na() %>% 
  spread(house_num, tf_idf, fill=0) %>% 
  as.data.frame() %>% 
  column_to_rownames(var = "program_code") %>% 
  as.matrix()

mat <- mat[which(apply(mat,1,sum)!=0), which(apply(mat,2,sum)!=0)]
rm(list = c("tf", "idf", "tf_idf"));gc()

res <- list()
cl <- makeCluster(2)
registerDoParallel(cl)
res <- foreach(i = 3:9, .packages = c("NMF")) %dopar%{
  nmf(mat, i, method = "brunet", seed = "ica")
}
stopCluster(cl)
gc()


basis <- basis(res)
pro_mst <- program %>% 
  count(program_code, program_name) %>% 
  collect() %>% 
  arrange(program_code, desc(n)) %>% 
  group_by(program_code) %>% 
  slice(1) %>% 
  ungroup()
basis %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "program_code") %>% 
  as_tibble() %>% 
  arrange(desc(V1, V2, V3)) %>% 
  slice(1:20) %>% 
  select(program_code, V1) %>% 
  left_join(pro_mst, by = "program_code") %>% 
  select(program_code,program_name) %>% 
  head(20)
  

basis %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "program_code") %>% 
  as_tibble() %>% 
  arrange(desc(V2, V3)) %>% 
  slice(1:20) %>% 
  select(program_code, V2) %>% 
  left_join(pro_mst, by = "program_code") %>% 
  select(program_code,program_name) %>% 
  head(20)

basis %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "program_code") %>% 
  as_tibble() %>% 
  arrange(desc(V3)) %>% 
  slice(1:20) %>% 
  select(program_code, V3) %>% 
  left_join(pro_mst, by = "program_code") %>% 
  select(program_code,program_name) %>% 
  head(20)

coef()

res3 <- read_rds("data/result_nmf_3.rds")
res5 <- read_rds("data/result_nmf_5.rds")
