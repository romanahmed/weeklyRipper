######################################

air_visits %>%
  filter(visit_date > ymd("2016-04-15") & visit_date < ymd("2017-03-15")) %>%
  group_by(visit_date) %>%
  summarise(all_visitors = sum(visitors)) %>%
  ggplot(aes(visit_date,all_visitors)) +
  geom_line() +
  geom_smooth(method = "loess", color = "blue", span = 1/7) +
  labs(y = "All visitors", x = "Date")

# air_visits_tsbl <- as_tsibble(air_visits, key = id(air_store_id)) %>%
#   group_by(air_store_id) %>%
#   index_by(month = month(visit_date)) %>%
#   summarize(maxV = max(visitors),
#             minV = min(visitors),
#             total = sum(visitors),
#             nObs = n()) %>%
#   mutate(air_store_id = factor(air_store_id))
# ggplot(air_visits_tsbl, aes(x = month, y=total, color = air_store_id)) + 
#   geom_line() +  
#   geom_smooth(se = 1, color = "red", size = 2) +
#   theme(legend.position="none")


#####################################

air_reserve %>%
  # filter(visit_datetime > ymd("2016-04-15") & visit_datetime < ymd("2017-03-15")) %>%
  select(-reserve_datetime) %>%
  group_by(air_store_id, visit_datetime) %>%
  mutate(totReservation = sum(reserve_visitors)) %>%
  ungroup() %>%
  select(-reserve_visitors) %>%
  unique() %>%
  mutate(visit_date = date(visit_datetime)) %>%
  select(-visit_datetime) %>%
  group_by(air_store_id, visit_date) %>%
  mutate(air_totReservedVisitors = sum(totReservation)) %>%
  select(-totReservation) -> air_reservedVisitors


air_reserve_visits <- left_join(air_visits, air_reservedVisitors)

air_reserve_visits <- left_join(air_reserve_visits, 
                                store_ids) %>%
  filter(air_store_id %in% store_ids$air_store_id) %>%
  na.omit

ggplot(air_reserve_visits %>% filter(air_store_id == "air_37189c92b6c761ec"),
       aes(x=visit_date, y =visitors)) +
  geom_line() +
  geom_line(aes(y=air_totReservedVisitors, col ="red"))

##########################################


hpg_reserve %>%
  # filter(visit_datetime > ymd("2016-04-15") & visit_datetime < ymd("2017-03-15")) %>%
  select(-reserve_datetime) %>%
  group_by(hpg_store_id, visit_datetime) %>%
  mutate(totReservation = sum(reserve_visitors)) %>%
  ungroup() %>%
  select(-reserve_visitors) %>%
  unique() %>%
  mutate(visit_date = date(visit_datetime)) %>%
  select(-visit_datetime) %>%
  group_by(hpg_store_id, visit_date) %>%
  mutate(hpg_totReservedVisitors = sum(totReservation)) %>%
  select(-totReservation) -> hpg_reservedVisitors

hpg_reservedVisitors <- left_join(hpg_reservedVisitors, store_ids)


#####################3

airVisit_2RevSys <- inner_join(air_reserve_visits, hpg_reservedVisitors)
airVisit_2RevSys$totRevVisit <- rowSums(cbind(airVisit_2RevSys$air_totReservedVisitors,
                                              airVisit_2RevSys$hpg_totReservedVisitors),
                                        na.rm = T)
airVisit_2RevSys %>%
  mutate(visitor2reserveRatio = visitors/totRevVisit) -> airVisit_2RevSys


ggplot(airVisit_2RevSys %>% filter(air_store_id == "air_7ef9a5ea5c8fe39f"),
       aes(x=visit_date, y =visitors)) +
  geom_line() +
  geom_line(aes(y=air_totReservedVisitors, col ="red")) +
  geom_line(aes(y=hpg_totReservedVisitors, col ="green"))


ggplot(airVisit_2RevSys, 
       aes(x = visit_date, y = air_totReservedVisitors, color = air_store_id))+
  geom_point() +
  theme(legend.position = "none") #+
  geom_smooth(method = "loess", color = "blue")

ggplot(airVisit_2RevSys, 
       aes(x = visit_date, y = hpg_totReservedVisitors, color = air_store_id))+
  geom_point() +
  theme(legend.position = "none") #+
  geom_smooth(method = "loess", color = "blue")
  
  

#####################################


left_join(air_visits, store_ids) %>%
  filter(air_store_id %in% store_ids$air_store_id) %>%
  ggplot(aes(x = visit_date, y = visitors, color = air_store_id))+
  geom_line() +
  theme(legend.position = "none") 









#####################################3333
###########################################



ggplot(airVisit_2RevSys %>%
         filter(air_store_id == "air_37189c92b6c761ec"), 
       aes(x = visit_date, 
           y = visitor2reserveRatio, 
           color = air_store_id)) +
  geom_line() +
  theme(legend.position = "none")






air_reserve %>%
  filter(visit_datetime > ymd("2016-04-15") & visit_datetime < ymd("2017-03-15")) %>%
  group_by(air_store_id, reserve_datetime) %>%
  mutate(totReservation = sum(reserve_visitors)) %>%
  ungroup() %>%
  mutate(reserve_date = as_date(ymd_hms(reserve_datetime))) -> tmp_air

tmp_air %>%
  ggplot(aes(reserve_date, totReservation, color = air_store_id)) +
  geom_line() +
  geom_smooth(method = "loess", color = "blue", span = 2/3) +
  labs(y = "All resereve", x = "Date") +
  theme(legend.position="none") +
  scale_y_log10() 


tmp_air %>%
  group_by(air_store_id, reserve_datetime) %>%
  mutate

dupIdx <- find_duplicates(tmp_air %>% select(-visit_datetime),
                          key = id(air_store_id),
                          index = reserve_datetime) 

deDup <- air_reserve[dupIdx,]
air_reserve_tsbl <- as.tsibble(tmp_air %>% select(-visit_datetime), 
                               key = id(air_store_id),
                               index = reserve_datetime)
  
%>%
  group_by(air_store_id) %>%
  index_by(month = month(visit_date)) %>%
  summarize(maxV = max(visitors),
            minV = min(visitors),
            total = sum(visitors),
            nObs = n()) %>%
  mutate(air_store_id = factor(air_store_id))

##################################################


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



# Rservation day to Visit day transition ----
prop.table(table(hpg_reserve2$reserve_Wday, hpg_reserve2$visit_Wday),
           margin = 1) -> tblCir
tblCir %>% as.data.frame() -> tblCir
colnames(tblCir) <-  c("Reserved", "Visited", "Proportion")
tblCir$Reserved <- factor(str_c(tblCir$Reserved))
# chordDiagram(tblCir)
tblCir %>% group_by(Reserved) %>%
  ggplot(aes(x = Visited, y = Proportion, fill = Reserved)) +
  # geom_point() +
  geom_bar(stat = "identity") +
  facet_wrap(~Reserved) +
  coord_polar() +
  theme_bw() +
  labs(title = "Distribution of visit day against reservation day")
######

hpg_reserve %>%
  mutate(visit_Wday = wday(visit_datetime, label = TRUE),
         reserve_Wday = wday(reserve_datetime, label = TRUE),
         visit_Weekend = if_else(visit_Wday %in% c("Sat", "Sun"), 
                                 "Weekend", "Weekday"),
         reserve_Weekend = if_else(reserve_Wday %in% c("Sat", "Sun"), 
                                   "Weekend", "Weekday"),
         Date = as_date(ymd_hms(visit_datetime)),
         Day = day(Date),
         Month = month(Date),
         Year = year(Date)) -> hpg_reserve2

hpg_reserve2 %>%
  mutate() -> hpg_reserve2

hpg_reserve2 %>%
  mutate(visit_Time = hour(visit_datetime)) -> hpg_reserve2

hpg_reserve2 %>%
  mutate()

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



