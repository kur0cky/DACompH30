library(tidyverse)
library(lubridate)

pro <- read_csv("data/raw/program.csv")

pro %>% 
  summarise_if(is.numeric, c("min", "max", "mean"), na.rm=TRUE)

# NA込みのuniqueの数
pro %>% 
  apply(2, function(x) sum(!is.na(unique(x))))
ban %>% 
  apply(2, function(x) length(unique(x)))

pro %>% 
  mutate(開始時刻 = as.integer(開始時刻)) %>% 
  summarise_at(c("開始時刻", "放送分数"), funs(min, max, mean), na.rm=TRUE)

jiten <- read_csv("data/raw/jiten_data.csv")

pro %>% 
  filter(is.na(番組漢字名称))


# 空き番組をViewに
# sin_tokuとfinal_codeをバイナリ
# 放送形式のNULLを0に

dim(jiten)
jiten %>% 
  group_by(銘柄コード, 放送日, CM挿入時刻) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))
