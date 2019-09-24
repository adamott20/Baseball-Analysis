load("data/events_pre_1970.Rda")
load("data/events_post_1970.Rda")
events <- rbind(events_pre_1970, events_post_1970)
rm(events_pre_1970)
rm(events_post_1970)

