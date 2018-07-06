# Transition reserve_day vs visit_day from air_reserve data ----
air_reserve %>%
  mutate(air_reserve_date = date(reserve_datetime),
         air_reserve_day = wday(air_reserve_date, label = TRUE)) %>%
  mutate(visit_date = date(visit_datetime),
         visit_day = wday(visit_date, label = TRUE)) %>%
  select(-reserve_datetime, -visit_datetime) %>%
  group_by(air_store_id, air_reserve_date, visit_date) %>%
  mutate(airTotReservation = sum(reserve_visitors)) %>%
  ungroup() %>%
  select(-reserve_visitors) %>%
  unique() -> airReserve

# Define color pallet
colPal <- "Darjeeling1"
airResCol <- wes_palette(colPal)[2] # green
hpgResCol <- wes_palette(colPal)[4] # orange
airVisCol <- wes_palette(colPal)[3]

# Join airReserve to air_vist data ----
airResVis <- full_join(air_visits, airReserve)

# Join airResVis with store_location, store_id and holiday data -----
airData <- left_join(left_join(left_join(airResVis, air_store),
                                holidays %>%
                        dplyr::rename(visit_date = date)),
                     store_ids) 

# Rservation day to Visit day transition ----

airData %>%
  select(air_store_id, visit_date, air_reserve_day, visit_day) %>%
  unique() %>%
  na.omit -> airResVsVisDay
prop.table(table(airResVsVisDay$air_reserve_day, airResVsVisDay$visit_day),
           margin = 1) -> reserveTransTabAir
reserveTransTabAir %>% as.data.frame() -> reserveTransTabAir
colnames(reserveTransTabAir) <-  c("Reserved", "Visited", "Proportion")
reserveTransTabAir$reserveSys <- "AIR"
reserveTransTabAir$Reserved <- factor(str_c(reserveTransTabAir$Reserved))
reserveTransTabAir %>% group_by(Reserved) %>%
  ggplot(aes(x = Visited, y = Proportion, fill = Reserved)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values= wes_palette(colPal, 7, type = "continuous")) +
  facet_wrap(~Reserved) +
  coord_polar() +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Distribution of visit day by AIR reservation day") -> airReservationLSmemory

# Plot aggregated number of visitors for all stores -----

airData %>%
  select(air_store_id, visit_date, visitors) %>%
  unique() %>%
  group_by(visit_date) %>%
  summarise(airVisitor = sum(visitors, na.rm = T)) %>%
  left_join(., airData %>%
              group_by(visit_date) %>%
              summarise(airReservedVis = sum(airTotReservation, na.rm = T))) -> airResVisData
airResVisData[airResVisData == 0] <- NA
airResVisData %>%
ggplot(aes(visit_date, airVisitor)) +
  geom_line(color = airVisCol, size = 0.8) +
  # geom_smooth(aes(visit_date, airVisitor),
  #             method = "loess", color = "blue", span = 0.075) +
  # 0.075 comes from rounding of 28/nrow()
  geom_line(aes(y=airReservedVis), color = airResCol, size = 0.8) +
  # geom_smooth(aes(visit_date, airReservedVis),
  #             method = "loess", color = "airResCol", span = 0.075)+
  labs(y = "Total visitors all AIR stores", x = "Date") +
  theme_bw() +
  labs(title = "Total guests visiting and reserving air stores over time") -> smoothAggrAirAllStores

  
# Plot aggregated number of visitors by day for all stores -----

airData %>%
  select(air_store_id, visit_date, visitors) %>%
  unique() %>%
  mutate(visitDay = wday(visit_date, label = T)) %>%
  group_by(visitDay) %>% 
  summarize(agg_visitors = sum(visitors, na.rm=T)) %>%
  ggplot(aes(visitDay, agg_visitors, fill = visitDay)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values= wes_palette(colPal, 7, type = "continuous")) +
  labs(y = "Total visitors", x = "") +
  theme_bw() +
  theme(legend.position = "none") +
  # coord_polar() + 
  labs(title = "Total guests visiting by day of the week") -> smoothAggrAirReserveByDay
  

# Plot aggregated number of visitors by month for all stores -----
airData %>%
  select(air_store_id, visit_date, visitors) %>%
  filter(visit_date < "2017-01-01") %>%
  unique() %>%
  mutate(visitMonth = month(visit_date, label = T)) %>%
  group_by(visitMonth) %>% 
  summarize(agg_visitors = sum(visitors, na.rm=T)) %>%
  ggplot(aes(visitMonth, agg_visitors, fill = visitMonth)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values= wes_palette(colPal, 12, type = "continuous")) +
  labs(y = "Total visitors", x = "") +
  theme_bw() +
  # coord_polar() +
  theme(legend.position = "none") + 
  labs(title = "Total guests visiting by month for 2016") -> smoothAggrAirReserveByMonth

# Transition reserve_day vs visit_day from hpg_reserve data ----
hpg_reserve %>%
  mutate(hpg_reserve_date = date(reserve_datetime),
         hpg_reserve_day = wday(hpg_reserve_date, label = TRUE)) %>%
  mutate(visit_date = date(visit_datetime),
         visit_day = wday(visit_date, label = TRUE)) %>%
  select(-reserve_datetime, -visit_datetime) %>%
  group_by(hpg_store_id, hpg_reserve_date, visit_date) %>%
  mutate(hpgTotReservation = sum(reserve_visitors)) %>%
  ungroup() %>%
  select(-reserve_visitors) %>%
  unique() -> hpgReserve



# Join hpg and air data ----
hpgAir <- full_join(hpgReserve, airData) %>%
  filter(air_store_id %in% airData$air_store_id)



# Rservation day to Visit day transition ----

hpgAir %>%
  select(hpg_store_id, visit_date, hpg_reserve_day, visit_day) %>%
  unique() %>%
  na.omit -> hpgResVsVisDay

prop.table(table(hpgResVsVisDay$hpg_reserve_day, hpgResVsVisDay$visit_day),
           margin = 1) -> reserveTransTabHpg
reserveTransTabHpg %>% as.data.frame() -> reserveTransTabHpg
colnames(reserveTransTabHpg) <-  c("Reserved", "Visited", "Proportion")
reserveTransTabHpg$reserveSys <- "HPG"
reserveTransTabHpg$Reserved <- factor(str_c(reserveTransTabHpg$Reserved))
reserveTransTabHpg %>% group_by(Reserved) %>%
  ggplot(aes(x = Visited, y = Proportion, fill = Reserved)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values= wes_palette(colPal, 7, type = "continuous")) +
  facet_wrap(~Reserved) +
  coord_polar() +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Distribution of visit day by HPG reservation day") -> hpgReservationLSmemory


# Combined reserve to visist plot ----

res2visTran <- bind_rows(reserveTransTabAir, reserveTransTabHpg)
res2visTran$sameDayVis <- res2visTran$Reserved == res2visTran$Visited
res2visTran %>%
  ggplot(aes(x = Visited, y = Proportion, fill = sameDayVis)) +
  scale_fill_manual(values= wes_palette(colPal, 10, type = "continuous")[c(5,10)]) +
  geom_bar(stat = "identity") +
  facet_grid(reserveSys ~ Reserved) +
  # coord_polar() +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Distribution of visit day by reservation day") -> hpgNairReservationLSmemory
hpgNairReservationLSmemoryPC <- hpgNairReservationLSmemory + coord_polar()


# Plot aggregated number of visitors for all stores -----

hpgAir %>%
  select(air_store_id, visit_date, visitors) %>%
  unique() %>%
  group_by(visit_date) %>%
  summarise(airVisitor = sum(visitors, na.rm = T)) %>%
  left_join(., airData %>%
              group_by(visit_date) %>%
              summarise(airReservedVis = sum(airTotReservation, na.rm = T))) %>%
  left_join(.,hpgAir %>%
              select(visit_date, hpgTotReservation) %>%
              unique() %>%
              group_by(visit_date) %>%
              mutate(hpgReservedVis = sum(hpgTotReservation, na.rm = T)) %>%
              select(-hpgTotReservation) %>%
              unique) -> combPlotData


combPlotData[combPlotData == 0] <- NA
combPlotData %>%
  ggplot(aes(visit_date, airVisitor)) +
  geom_line(size = 1.2, color = airVisCol) +
  geom_smooth(aes(visit_date, airVisitor),
              method = "loess", color = airVisCol, span = 0.075) +
  # 0.075 comes from rounding of 28/nrow()
  geom_line(aes(y=airReservedVis), size = 1.2, col = airResCol) +
  geom_smooth(aes(visit_date, airReservedVis),
              method = "loess", color = airResCol, span = 0.075)+
  geom_line(aes(y=hpgReservedVis), size = 1.2, col = hpgResCol) +
  geom_smooth(aes(visit_date, hpgReservedVis),
              method = "loess", color = hpgResCol, span = 0.075)+
  labs(y = "Total visitors all AIR stores", x = "Date") +
  theme_bw() +
  labs(title = "Total guests reserving (using AIR and HPG) and visiting AIR stores over time") -> smoothAggrAirAllStores

smoothAggrAirAllStores + scale_y_continuous(trans = "log1p")



combPlotData %>%
  ggplot(aes(visit_date, airVisitor)) +
  geom_line(size = 1.2, color = airVisCol) +
  # geom_smooth(aes(visit_date, airVisitor),
  #             method = "loess", color = "blue", span = 0.075) +
  # 0.075 comes from rounding of 28/nrow()
  geom_line(aes(y=airReservedVis), size = 1.2, col = airResCol) +
  # geom_smooth(aes(visit_date, airReservedVis),
  #             method = "loess", color = "airResCol", span = 0.075)+
  geom_line(aes(y=hpgReservedVis), size = 1.2, col = hpgResCol) +
  # geom_smooth(aes(visit_date, hpgReservedVis),
  #             method = "loess", color = "hpgResCol", span = 0.075)+
  labs(y = "Total visitors/reservation all AIR stores", x = "Date") +
  theme_bw() +
  labs(title = "Total guests reserving (using AIR and HPG) and visiting AIR stores over time") -> aggrAirAllStoresTP

aggrAirAllStoresTPLog <- aggrAirAllStoresTP + scale_y_continuous(trans = "log1p")

# Check lead time from reservation to visit ----
hpgAir %>%
  select(hpg_store_id, hpg_reserve_date, visit_date, hpgTotReservation) %>%
  unique() %>%
  mutate(visitLeadDay = time_length(visit_date - hpg_reserve_date, 
                                    unit = "day")) %>%
  select(visitLeadDay, hpgTotReservation) %>%
  group_by(visitLeadDay) %>%
  summarise(hpgLeadTime = sum(hpgTotReservation, na.rm = T)) %>%
  mutate(chpgLeadTime = cumsum(hpgLeadTime)) %>%
  mutate(hpgLeadTimeDist = chpgLeadTime/sum(hpgLeadTime)) %>%
  left_join(., hpgAir %>%
              select(hpg_store_id, air_reserve_date, visit_date, airTotReservation) %>%
              unique() %>%
              mutate(visitLeadDay = time_length(visit_date - air_reserve_date, 
                                                unit = "day")) %>%
              select(visitLeadDay, airTotReservation) %>%
              group_by(visitLeadDay) %>%
              summarise(airLeadTime = sum(airTotReservation, na.rm = T)) %>%
              mutate(cairLeadTime = cumsum(airLeadTime)) %>%
              mutate(airLeadTimeDist = cairLeadTime/sum(airLeadTime)) ) -> res2visitLeadTime

res2visitLeadTime %>%
  ggplot(aes(x = visitLeadDay, y = airLeadTimeDist)) +
  geom_line(size = 1.25, color = airResCol) +
  geom_line(aes(y = hpgLeadTimeDist), size = 1.25,  color = hpgResCol) +
  theme_bw() +
  labs(title = "Lead time in days from reservation to visit for AIR (green) and HPG (orange)",
       x = "Lead time in days",
       y = "Proportion of guests visited") +
  geom_vline(xintercept= c(7, 14, 21) , 
             col = "black", size = 0.7,
             linetype = "dashed") -> res2VisitTimeDistPlot
ff

