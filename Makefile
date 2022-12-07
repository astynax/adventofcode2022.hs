DAY:=$(shell date +%d)

.PHONY: day
day: Day$(DAY).hs Day$(DAY).input
	@echo bootstraped

Day$(DAY).hs:
	@cat .make/Template.hs | sed -e "s/DAY/$(DAY)/" > $@
	@cat .make/chunk.cabal | sed -e "s/DAY/$(DAY)/" >> adventofcode2022.cabal

Day$(DAY).input:
	@cp ~/Downloads/input $@
