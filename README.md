# Does Algorithmic Discovery Drive Engagement? Evidence from TikTok Data Donations

This repository contains the data preparation workflow and analysis code for my master thesis *Does Algorithmic Discovery Drive Engagement? Evidence from TikTok Data Donations*, submitted in partial fulfillment of the requirements for the MSc Marketing Analytics at Tilburg University.

The thesis investigates how exposure to content from previously unseen creators — operationalized as the **Discovery Ratio** — affects users' **Session Duration** and **Like-Rate**, and whether this relationship depends on **Search Intensity** as a moderator.

## Research Questions

- To what extent does the Discovery Ratio influence Session Duration on TikTok?
- To what extent does the Discovery Ratio influence users' Like-Rate on TikTok?
- To what extent does Search Intensity moderate the relationship between Discovery Ratio and Session Duration?
- To what extent does Search Intensity moderate the relationship between Discovery Ratio and Like-Rate?

## Variables

| Variable | Description |
|---|---|
| **Discovery Ratio** | Share of videos in a given week originating from creators never previously encountered in the user's full cumulative watch history |
| **Session Duration** | Average duration (in minutes) of TikTok sessions per week, where a session is defined as consecutive views with gaps of less than 30 minutes |
| **Like-Rate** | Number of likes divided by total videos watched in a given week |
| **Search Intensity** | Number of in-app searches performed in a given week |

## Data

Data are collected via voluntary TikTok data donations from approximately 20 participants. Each participant exported their personal TikTok data package, which includes:

- Video browsing history (watch history with timestamps)
- Like history
- Search history
- Favorite videos

The raw data files are stored at the participant level (`data/raw_json/participant_XX/`) and are **not included** in this repository to protect participant privacy.

Creator usernames are resolved from TikTok video URLs using the Python script in `python/src/`, which fetches each unique video URL and extracts the creator handle. The resolved creator maps are stored in `data/unique_urls/`.

## Repository Overview

```
├── README.md
├── tiktok-thesis.Rproj
├── .gitignore
├── data                         ← excluded from repo (see .gitignore)
├── gen
│   ├── temp
│   │   └── unique_video_urls_pXX.csv
│   ├── cleaned
│   └── output
├── python
│   ├── input
│   │   └── unique_video_urls_pXX.csv
│   ├── output
│   │   ├── creator_map_final_pXX.csv
│   │   └── creator_map_partial_pXX.csv
│   └── src
│       └── 01_resolve_creators_v3.ipynb
└── src
    ├── dataprep
    │   ├── 01_inspect_json.R
    │   ├── 02_unique_urls.R
    │   └── 03_building_variables.R
    └── analysis
```

> **Note:** The `data/` folder is excluded from this repository via `.gitignore` to protect participant privacy. It contains the raw TikTok JSON exports (`data/raw_json/`) and the resolved creator maps (`data/unique_urls/`). To reproduce the pipeline, place your own data donations in the appropriate subfolders.

## Pipeline

The data preparation workflow consists of three R scripts and one Python notebook, run in the following order:

### Step 1 — `src/dataprep/01_inspect_json.R`
Reads and parses the raw TikTok JSON export files for each participant. Loads the watch history, like list, favorite videos, and search history into the R environment as structured data frames.

### Step 2 — `src/dataprep/02_unique_urls.R`
Extracts all unique TikTok video URLs from each participant's watch history and saves them to `gen/temp/unique_video_urls_pXX.csv`. These files serve as input for the Python creator resolution step.

### Step 3 — `python/src/01_resolve_creators_v3.ipynb`
Takes the unique URL lists and resolves each URL to a TikTok creator username by fetching the video page. Outputs a `creator_map_final_pXX.csv` per participant stored in `data/unique_urls/`. Videos that could not be resolved (deleted, private, or rate-limited) are logged separately.

### Step 4 — `src/dataprep/03_building_variables.R`
Joins the watch history with the resolved creator maps and constructs the four weekly panel variables (Discovery Ratio, Session Duration, Like-Rate, Search Intensity) for each participant. Outputs the final panel dataset to `gen/output/weekly_panel_all.csv`.

## Dependencies

### R
```r
install.packages(c("dplyr", "lubridate", "readr", "tidyr"))
```

### Python
```bash
pip install requests pandas jupyter
```

## Output

The final output of the pipeline is `gen/output/weekly_panel_all.csv`, a longitudinal panel dataset at the participant × week level with the following columns:

| Column | Description |
|---|---|
| `participant` | Participant identifier (e.g., `p05`) |
| `week` | ISO week start date (Monday) |
| `n_videos_total` | Total videos watched that week |
| `n_videos_mapped` | Videos successfully linked to a creator |
| `n_new_creator_videos` | Videos from previously unseen creators |
| `discovery_ratio` | Share of videos from new creators |
| `n_sessions` | Number of TikTok sessions |
| `avg_session_dur_mins` | Average session duration in minutes |
| `n_likes` | Number of likes given |
| `like_rate` | Likes per video watched |
| `search_intensity` | Number of in-app searches |

## Author

**Joël de Vries**  
MSc Marketing Analytics — Tilburg School of Economics and Management, Tilburg University  
Supervisor: Banarjee, Shrabastee  
Second assessor: Sudhaharan, Roshini
