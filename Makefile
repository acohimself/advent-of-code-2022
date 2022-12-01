.DEFAULT_GOAL := help
.PHONY: help day1

help:
	$(info Advent of code 2022)
	$(info elm-live is required (npm i --global elm-live))
	$(info make dayX to run a day)

day1: ## Run elm-live for day1 
	cd day1 && elm-live src/Main.elm --start-page=Main.html -- --output=elm.js


