library(dplyr)
library(stringr)
library(readr)

extract_video_id <- function(url) str_match(url, "/video/([0-9]+)/")[,2]

#P01
video_browse_p01 <- watch_raw_p01$tiktok_video_browsing_history[[1]]

unique_urls_p01 <- video_browse_p01 %>%
  transmute(
    url = `Video watched`,
    video_id = extract_video_id(`Video watched`)
  ) %>%
  filter(!is.na(video_id)) %>%
  distinct(video_id, url)

nrow(unique_urls_p01)
write_csv(unique_urls_p01, "gen/temp/unique_video_urls_p01.csv")

#P02
video_browse_p02 <- watch_raw_p02$tiktok_video_browsing_history[[1]]

unique_urls_p02 <- video_browse_p02 %>%
  transmute(
    url = `Video watched`,
    video_id = extract_video_id(`Video watched`)
  ) %>%
  filter(!is.na(video_id)) %>%
  distinct(video_id, url)

nrow(unique_urls_p02)
write_csv(unique_urls_p02, "gen/temp/unique_video_urls_p02.csv")

#P04
video_browse_p04 <- watch_raw_p04$tiktok_video_browsing_history[[1]]

unique_urls_p04 <- video_browse_p04 %>%
  transmute(
    url = `Video watched`,
    video_id = extract_video_id(`Video watched`)
  ) %>%
  filter(!is.na(video_id)) %>%
  distinct(video_id, url)

nrow(unique_urls_p04)
write_csv(unique_urls_p04, "gen/temp/unique_video_urls_p04.csv")

#P05
video_browse_p05 <- watch_raw_p05$tiktok_video_browsing_history[[1]]

unique_urls_p05 <- video_browse_p05 %>%
  transmute(
    url = `Video watched`,
    video_id = extract_video_id(`Video watched`)
  ) %>%
  filter(!is.na(video_id)) %>%
  distinct(video_id, url)

nrow(unique_urls_p05)
write_csv(unique_urls_p05, "gen/temp/unique_video_urls_p05.csv")

#P06
video_browse_p06 <- watch_raw_p06$tiktok_video_browsing_history[[1]]

unique_urls_p06 <- video_browse_p06 %>%
  transmute(
    url = `Video watched`,
    video_id = extract_video_id(`Video watched`)
  ) %>%
  filter(!is.na(video_id)) %>%
  distinct(video_id, url)

nrow(unique_urls_p06)
write_csv(unique_urls_p06, "gen/temp/unique_video_urls_p06.csv")
