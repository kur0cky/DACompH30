program <- collect(program)

tmp <- watch_rate %>% 
  select(station_code, program_start_time, all_watch_rate) %>% 
  mutate(cm_date = as_date(program_start_time)) %>% 
  left_join(program, by = c("station_code", "program_start_time")) %>% 
  select(station_code, program_code, program_start_time, program_end_time, all_watch_rate, cm_date)

spot <- jiten %>% 
  filter(adv_type %in% c(2, 5)) %>% 
  collect()
time <- jiten %>% 
  filter(adv_type %in% c(0)) %>% 
  collect()


tmp2 <- time %>% 
  left_join(tmp, by = c("station_code", "cm_date")) 
tmp3 <- tmp2 %>%  
  mutate(flag1 = program_start_time <= cm_start_time,
         flag2 = cm_start_time <= program_end_time) %>% 
  filter(flag1==TRUE & flag2==TRUE)


# これをさらにtmp3にくっつける必要
time %>% 
  anti_join(tmp3, by = c("cm_start_time", "station_code"))
program %>% 
  filter(station_code == "1", program_date == "2017-04-07") %>% 
  View()


tmp3 <- program %>% 
  select(station_code, program_code, program_start_time, program_end_time) %>% 
  mutate(date = as_date(program_start_time)) %>% 
  right_join(time, by = c("station_code", "date" = "cm_date")) %>% 
  mutate(flag1 = program_start_time <= cm_start_time,
         flag2 = cm_start_time <= program_end_time) %>% 
  filter(flag1==TRUE & flag2==TRUE) %>% 
  select(-flag1, -flag2)

tmp3 %>% 
  left_join(select(watch_rate, station_code, program_start_time, all_watch_rate),
            by = c("station_code", "program_start_time")) %>% 
  group_by(brand_code) %>% 
  summarise(GRP = sum(all_watch_rate)) %>% 
  arrange(desc(GRP)) %>% 
  left_join(collect(brand), by = "brand_code") %>% 
  View()
  
