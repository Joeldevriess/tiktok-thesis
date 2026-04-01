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
write_csv(unique_urls_p01, "python/input/unique_video_urls_p01.csv")

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
write_csv(unique_urls_p02, "python/input/unique_video_urls_p02.csv")

#P03
video_browse_p03 <- watch_raw_p03$tiktok_video_browsing_history[[1]]

unique_urls_p03 <- video_browse_p03 %>%
  transmute(
    url = `Video watched`,
    video_id = extract_video_id(`Video watched`)
  ) %>%
  filter(!is.na(video_id)) %>%
  distinct(video_id, url)

nrow(unique_urls_p03)
write_csv(unique_urls_p03, "python/input/unique_video_urls_p03.csv")

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
write_csv(unique_urls_p04, "python/input/unique_video_urls_p04.csv")

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
write_csv(unique_urls_p05, "python/input/unique_video_urls_p05.csv")

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
write_csv(unique_urls_p06, "python/input/unique_video_urls_p06.csv")

#P07
video_browse_p07 <- watch_raw_p07$tiktok_video_browsing_history[[1]]

unique_urls_p07 <- video_browse_p07 %>%
  transmute(
    url = `Video watched`,
    video_id = extract_video_id(`Video watched`)
  ) %>%
  filter(!is.na(video_id)) %>%
  distinct(video_id, url)

nrow(unique_urls_p07)
write_csv(unique_urls_p07, "python/input/unique_video_urls_p07.csv")

#P08
video_browse_p08 <- watch_raw_p08$tiktok_video_browsing_history[[1]]

unique_urls_p08 <- video_browse_p08 %>%
  transmute(
    url = `Video watched`,
    video_id = extract_video_id(`Video watched`)
  ) %>%
  filter(!is.na(video_id)) %>%
  distinct(video_id, url)

nrow(unique_urls_p08)
write_csv(unique_urls_p08, "python/input/unique_video_urls_p08.csv")

#P09
video_browse_p09 <- watch_raw_p09$tiktok_video_browsing_history[[1]]

unique_urls_p09 <- video_browse_p09 %>%
  transmute(
    url = `Video watched`,
    video_id = extract_video_id(`Video watched`)
  ) %>%
  filter(!is.na(video_id)) %>%
  distinct(video_id, url)

nrow(unique_urls_p09)
write_csv(unique_urls_p09, "python/input/unique_video_urls_p09.csv")

#P10
video_browse_p10 <- watch_raw_p10$tiktok_video_browsing_history[[1]]

unique_urls_p10 <- video_browse_p10 %>%
  transmute(
    url = `Video watched`,
    video_id = extract_video_id(`Video watched`)
  ) %>%
  filter(!is.na(video_id)) %>%
  distinct(video_id, url)

nrow(unique_urls_p10)
write_csv(unique_urls_p10, "python/input/unique_video_urls_p10.csv")

#P11
video_browse_p11 <- watch_raw_p11$tiktok_video_browsing_history[[1]]

unique_urls_p11 <- video_browse_p11 %>%
  transmute(
    url = `Video watched`,
    video_id = extract_video_id(`Video watched`)
  ) %>%
  filter(!is.na(video_id)) %>%
  distinct(video_id, url)

nrow(unique_urls_p11)
write_csv(unique_urls_p11, "python/input/unique_video_urls_p11.csv")

#P12 -- No video data
#video_browse_p12 <- watch_raw_p12$tiktok_video_browsing_history[[1]]

#unique_urls_p12 <- video_browse_p12 %>%
  #transmute(
  #  url = `Video watched`,
 #   video_id = extract_video_id(`Video watched`)
 # ) %>%
 # filter(!is.na(video_id)) %>%
 # distinct(video_id, url)

#nrow(unique_urls_p12)
#write_csv(unique_urls_p12, "python/input/unique_video_urls_p12.csv")

#P13
video_browse_p13 <- watch_raw_p13$tiktok_video_browsing_history[[1]]

unique_urls_p13 <- video_browse_p13 %>%
  transmute(
    url = `Video watched`,
    video_id = extract_video_id(`Video watched`)
  ) %>%
  filter(!is.na(video_id)) %>%
  distinct(video_id, url)

nrow(unique_urls_p13)
write_csv(unique_urls_p13, "python/input/unique_video_urls_p13.csv")

#P14
video_browse_p14 <- watch_raw_p14$tiktok_video_browsing_history[[1]]

unique_urls_p14 <- video_browse_p14 %>%
  transmute(
    url = `Video watched`,
    video_id = extract_video_id(`Video watched`)
  ) %>%
  filter(!is.na(video_id)) %>%
  distinct(video_id, url)

nrow(unique_urls_p14)
write_csv(unique_urls_p14, "python/input/unique_video_urls_p14.csv")

#P15
video_browse_p15 <- watch_raw_p15$tiktok_video_browsing_history[[1]]

unique_urls_p15 <- video_browse_p15 %>%
  transmute(
    url = `Video watched`,
    video_id = extract_video_id(`Video watched`)
  ) %>%
  filter(!is.na(video_id)) %>%
  distinct(video_id, url)

nrow(unique_urls_p15)
write_csv(unique_urls_p15, "python/input/unique_video_urls_p15.csv")

#P20 -- No video data
#video_browse_p20 <- watch_raw_p20$tiktok_video_browsing_history[[1]]

#unique_urls_p20 <- video_browse_p20 %>%
 # transmute(
  #  url = `Video watched`,
   # video_id = extract_video_id(`Video watched`)
#  ) %>%
 # filter(!is.na(video_id)) %>%
#  distinct(video_id, url)

#nrow(unique_urls_p20)
#write_csv(unique_urls_p20, "python/input/unique_video_urls_p20.csv")