
all: Lab2.hs Test.hs lab2.pdf 

Lab2.hs: lab2.md lua
	pandoc --from markdown --to tangle.lua --metadata=code:.solve $< | cat -s > $@

Test.hs: lab2.md lua
	pandoc --from markdown --to tangle.lua --metadata=code:.test $< | cat -s > $@

lab2.pdf: lab2.md lua
	pandoc  --pdf-engine=context $< -o $@

test: Lab2.hs Test.hs
	doctest Test.hs 2>&1 | head

clean:
	rm -rf Lab2.hs Test.hs lab2.pdf *.lua

lua: tangle.lua css-selector.lua utilities.lua

%.lua: ../%.lua
	cp $< $@

.PHONY: all clean lua
