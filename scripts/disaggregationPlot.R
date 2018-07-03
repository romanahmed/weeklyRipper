######################################

air_visits %>%
  filter(visit_date > ymd("2016-04-15") & visit_date < ymd("2016-06-15")) %>%
  group_by(visit_date) %>%
  summarise(all_visitors = sum(visitors)) %>%
  ggplot(aes(visit_date,all_visitors)) +
  geom_line() +
  geom_smooth(method = "loess", color = "blue", span = 1/7) +
  labs(y = "All visitors", x = "Date")

air_visits_tsbl <- as_tsibble(air_visits, key = id(air_store_id))
plot(air_visits_tsbl)

air_visits_tsbl %>%
  # group_by(air_store_id) %>%
  index_by(date = visit_date) %>%
  summarize(maxV = max(visitors),
            minV = min(visitors))

#####################################

air_reserve %>%
  filter(visit_datetime > ymd("2016-04-15") & visit_datetime < ymd("2017-03-15")) %>%
  mutate(visit_date = as_date(ymd_hms(visit_datetime))) %>%
  group_by(air_store_id, visit_date) %>%
  summarise(all_visitors = sum(reserve_visitors)) -> tmp_air
tmp_air %>%
  ggplot(aes(visit_date,all_visitors, color = air_store_id)) +
  geom_line() +
  geom_smooth(method = "loess", color = "blue", span = 2/3) +
  labs(y = "All resereve", x = "Date") +
  theme(legend.position="none")



hpg_reserve %>%
  filter(visit_datetime > ymd("2016-04-15") & visit_datetime < ymd("2016-06-15")) %>%
  mutate(visit_date = as_date(ymd_hms(visit_datetime))) %>%
  group_by(hpg_store_id, visit_date) %>%
  summarise(all_visitors = sum(reserve_visitors)) -> tmp_hpg
tmp_hpg %>%
  filter(hpg_store_id %in% sample(tmp_hpg$hpg_store_id, 50)) %>%
  ggplot(aes(visit_date,all_visitors, color = hpg_store_id)) +
  geom_line() +
  geom_smooth(method = "loess", color = "blue", span = 2/3) +
  labs(y = "All resereve", x = "Date") +
  theme(legend.position="none")

hpg_reserve %>%
  mutate(visit_Wday = wday(visit_datetime, label = TRUE),
         reserve_Wday = wday(reserve_datetime, label = TRUE),
         visit_Weekend = if_else(visit_Wday %in% c("Sat", "Sun"), 
                                 "Weekend", "Weekday"),
         reserve_Weekend = if_else(reserve_Wday %in% c("Sat", "Sun"), 
                                 "Weekend", "Weekday")) -> hpg_reserve2
hpg_reserve2 %>%
  mutate(Date = as_date(ymd_hms(visit_datetime))) -> hpg_reserve2

prop.table(table(hpg_reserve2$reserve_Wday, hpg_reserve2$visit_Wday),
           margin = 1) -> tblCir
tblCir %>% as.data.frame() -> tblCir
colnames(tblCir) <-  c("Reserved", "Visited", "value")
tblCir$Reserved <- factor(str_c(tblCir$Reserved))



chordDiagram(tblCir)

tblCir %>% group_by(Reserved) %>%
  ggplot(aes(x = Visited, y = value)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Reserved) +
  coord_polar()

hpg_reserve2 %>%
  mutate(visit_Time = hour(visit_datetime)) -> hpg_reserve2

hpg_reserve2 %>% 
  frame_calendar(
    x = visit_Time, y = reserve_visitors, date = Date, calendar = "monthly"
  )

p <- hpg_reserve2 %>%
  ggplot(aes(x = .visit_Time, y = reserve_visitors, group = Date, colour = Weekend)) +
  geom_line() +
  theme(legend.position = "bottom")
prettify(p, label.padding = unit(0.08, "lines"))

################

p1 <- air_visits %>%
  
  group_by(visit_date) %>%
  
  summarise(all_visitors = sum(visitors)) %>%
  
  ggplot(aes(visit_date,all_visitors)) +
  
  geom_line(col = "blue") +
  
  labs(y = "All visitors", x = "Date")



p2 <- air_visits %>%
  
  ggplot(aes(visitors)) +
  
  geom_vline(xintercept = 20, color = "orange") +
  
  geom_histogram(fill = "blue", bins = 30) +
  
  scale_x_log10()



p3 <- air_visits %>%
  
  mutate(wday = wday(visit_date, label = TRUE)) %>%
  
  group_by(wday) %>%
  
  summarise(visits = median(visitors)) %>%
  
  ggplot(aes(wday, visits, fill = wday)) +
  
  geom_col() +
  
  theme(legend.position = "none", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9)) +
  
  labs(x = "Day of the week", y = "Median visitors")



p4 <- air_visits %>%
  
  mutate(month = month(visit_date, label = TRUE)) %>%
  
  group_by(month) %>%
  
  summarise(visits = median(visitors)) %>%
  
  ggplot(aes(month, visits, fill = month)) +
  
  geom_col() +
  
  theme(legend.position = "none") +
  
  labs(x = "Month", y = "Median visitors")



layout <- matrix(c(1,1,1,1,2,3,4,4),2,4,byrow=TRUE)

multiplot(p1, p2, p3, p4, layout=layout)



