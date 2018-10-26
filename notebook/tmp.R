conn <- dbConnect(PostgreSQL(),
                  host = scan("connection.txt", what = character())[1],
                  port = scan("connection.txt", what = character())[2], 
                  dbname = scan("connection.txt", what = character())[3], 
                  user = scan("connection.txt", what = character())[4], 
                  password = scan("connection.txt", what = character())[5])

orgn <- conn %>% 
  tbl(from = dbplyr::in_schema("edit", "tv_orgn_program_2")) 
tmp <- conn %>% 
  tbl(from = dbplyr::in_schema("processed", "tv_orgn_p_cv")) 
program <- conn %>% 
  tbl(from = dbplyr::in_schema("processed", "tv_program")) 
play <- conn %>% 
  tbl(from = dbplyr::in_schema("processed", "tv_play_p_cv")) 
ban <- conn %>% 
  tbl(from = dbplyr::in_schema("processed", "bancluster_mst")) 
ban1 <- conn %>% 
  tbl(from = dbplyr::in_schema("sub_mst", "ban_code1_mst")) 
prof_data <- conn %>% 
  tbl(from = dbplyr::in_schema("processed", "profiledata"))
prof_mst <- conn %>% 
  tbl(from = dbplyr::in_schema("processed", "profilemaster"))
web <- conn %>% 
  tbl(from = dbplyr::in_schema("processed", "t_weblog"))
sta <- conn %>% 
  tbl(from = dbplyr::in_schema("processed", "sta_mst"))
sample <- conn %>% 
  tbl(from = dbplyr::in_schema("processed", "tv_sample_p_cv"))
adv <- conn %>% 
  tbl(from = dbplyr::in_schema("processed", "m_cm_tv_advertiser"))
brand <- conn %>% 
  tbl(from = dbplyr::in_schema("processed", "m_cm_tv_brand"))
jiten <- conn %>% 
  tbl(from = dbplyr::in_schema("processed", "jiten_data"))



prof <- prof_data %>% 
  left_join(prof_mst, by = c("qu_genre_code", "question_code", "answer_code"))

a <- web %>% 
  filter(house_num==1996, domain == "amazon.co.jp") %>%
  head(1000) %>% 
  collect() 

a %>% 
  arrange(web_start_datetime) %>% 
  filter(referrer_domain == "amazon.co.jp") %>% 
  select(web_date, web_start_datetime, web_title) %>% 
  group_by(web_date) %>% 
  mutate(flag = web_title == "支払い方法の選択") %>% 
  filter(flag == T)
  nest() %>% 
  .$data %>% 
  .[[6]]

  
play %>% 
  filter(timeshift_date %in% as_date("2017-06-01"))
dbGetQuery(conn, "
SELECT
  *
FROM
  edit.tv_orgn_program_2
WHERE
  EXTRACT(day from timeshift_datetime) = 14 and
  EXTRACT(month from timeshift_datetime) = 5 and
  station_code = 3
            ")  
program %>%
  filter(program_date == "2017-05-14", station_code == 3) %>% 
  collect() %>% 
  arrange(program_start_time) %>% 
  tail(20)


a <- orgn %>% 
  filter(station_code == 3,
         program_start_time == "2017-05-14 18:30:00") %>% 
  collect() 
a %>% 
  mutate(br_start_datetime = as.character(br_start_datetime),
         br_end_datetime = as.character(br_end_datetime)
         ) %>% 
  mutate(br_start_datetime = if_else(as_datetime(br_start_datetime) < as_datetime("2017-05-14 18:30:00"), 
                                     as_datetime("2017-05-14 18:30:00"),
                                     as_datetime(br_start_datetime)),
         br_end_datetime = if_else(as_datetime(br_end_datetime) > as_datetime("2017-05-14 19:00:00"),
                                   as_datetime("2017-05-14 19:00:00"),
                                   as_datetime(br_end_datetime))
         ) %>% 
  transmute(diff = as.integer(br_end_datetime - br_start_datetime)/60) %>% 
  mutate(rate = diff/20) %>% 
  summarise(sum(rate)/5149) %>% 
  unlist()

test <- sample %>% 
  filter(sample_date == "2017-05-14") %>% 
  collect() 
  
sazae1 <- play %>% 
  filter(station_code == "3",
         timeshift_date == "2017-05-14") %>% 
  collect()

sazae1 %>% 
  filter(timeshift_datetime <= as_datetime("2017-05-14 19:00:00",tz = "Asia/Tokyo"),
         timeshift_datetime >= as_datetime("2017-05-14 18:30:00", tz = "Asia/Tokyo")) %>% 
  count(minute(timeshift_datetime)) 
%>% 
  mutate(rate = n / 5149) 
%>% 
  ggplot(aes(min, rate))+
  geom_line()
sazae1 %>% 
  filter(timeshift_datetime <= as_datetime("2017-05-14 10:00:00"),
         timeshift_datetime >= as_datetime("2017-05-14 09:30:00")) %>% 
  count(min = minute(timeshift_datetime)) %>% 
  mutate(rate = n / 5149) %>% 
  summarise(rate = sum(rate) / 25)



# jiten ----
jiten_ <- jiten %>% 
  filter(cm_date == "2017-05-14") %>% 
  collect()

jiten_ %>% 
  filter(cm_start_time < as_datetime("2017-05-14 10:00:00"),
         cm_start_time >= as_datetime("2017-05-14 09:30:00"),
         station_code == 3) %>% 
  summarise(sum(cm_time))
