# AFLTables Screen Scraper

Scrapes the scoring events and player statistics from the match pages at `afltables.com`.

## Run

Edit the `years` key in `years.cfg` to set the desired seasons from which to get stats. Then run

```
  make  # builds binary, assumes you have stack installed
  make csvs  # generates csv stats files in _data
```

Output is `_data/ScoreEvent.csv` and `_data/PlayerEvent.csv`.

## Dependencies

* curl
* html tidy
