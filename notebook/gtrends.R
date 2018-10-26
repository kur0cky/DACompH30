

gtrends(c("NHL", "NFL"), time = "now 1-d")
keyWords <- c("たけのこの里", "きのこの山")
getTrends<-gtrends(keyword = keyWords, #キーワードは文字列ベクトル。5つまで。
                   geo = "JP", #検索地域は日本で。
                   time =  #"now 1-H",      #今から1時間前まで
                     #"now 4-H",     #Last four hours
                     #"now 1-d",     #Last day
                     #"now 7-d",     #Last seven days
                     #"today 1-m",    #Past 30 days
                     #"today 3-m",   #Past 90 days
                     #"today 12-m",  #Past 12 months
                     "today+5-y",   #Last five years (default)
                   #"all",         #Since the beginning of Google Trends (2004)
                   #"Y-m-d Y-m-d", #Time span between two dates (ex.: "2010-01-01 2010-04-03")
                   gprop = "web" 
                   #"news"
                   #"images"
                   #"froogle"
                   #"youtube"
)

getTrends$interest_over_time %>% 
  tail