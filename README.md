# AFLTables Screen Scraper

Scrapes the scoring events and player stats from the match pages at `afltables.com`.

## Run

Edit the years key in `afltables.cfg` to set the desired seasons from which to get stats. Then run

```
  make  # assumes you have stack installed
```

Output is `_data/scoreEvent.csv` and `_data/playerEvent.csv`.

## Todo

 - factor out common code
 - tidy
 - make sqlite db in addition to csvs (use beam?)
