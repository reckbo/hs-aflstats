.PHONY: all
all:
	stack build

.PHONY: csvs
csvs:
	stack exec scrape
