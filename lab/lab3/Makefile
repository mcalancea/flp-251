
dist=miniHaskell
distns=lab3-bare

all: lab3-nostack

lab3-nostack: $(distns)/lab3.pdf $(distns)/src/Exp.hs $(distns)/src/Parsing.hs $(distns)/src/Printing.hs $(distns)/src/REPLCommand.hs $(distns)/src/Main.hs $(distns)/src/Lab2.hs

init:
	stack new $(dist)
	
$(distns):
	mkdir -p $@

$(distns)/src: $(distns)
	mkdir -p $@

$(distns)/src/Parsing.hs: lab3-nostack.md $(distns)/src lua
	pandoc --from markdown --to tangle.lua --metadata=code:.Parsing $< | cat -s > $@

$(distns)/src/Printing.hs: lab3-nostack.md $(distns)/src lua
	pandoc --from markdown --to tangle.lua --metadata=code:.Printing $< | cat -s > $@

$(distns)/src/Exp.hs: lab3-nostack.md $(distns)/src lua
	pandoc --from markdown --to tangle.lua --metadata=code:.Exp $< | cat -s > $@

$(distns)/src/REPLCommand.hs: lab3-nostack.md $(distns)/src lua
	pandoc --from markdown --to tangle.lua --metadata=code:.REPLCommand $< | cat -s > $@

$(distns)/src/Main.hs: lab3-nostack.md $(distns)/src lua
	pandoc --from markdown --to tangle.lua --metadata=code:.Main $< | cat -s > $@

$(distns)/lab3.pdf: lab3-nostack.md $(distns) lua
	pandoc  --pdf-engine=context $< -o $@

$(distns)/src/Lab2.hs: ../lab2/Lab2.sol.hs $(distns)/src
	cp $< $@

clean:
	rm -rf $(dist) $(distns) *.lua

lua: tangle.lua css-selector.lua utilities.lua

%.lua: ../%.lua
	cp $< $@

.PHONY: all clean init lua lab3-nostack
