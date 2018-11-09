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
