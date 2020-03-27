TOOLS := build/Emulator

run: $(TOOLS)
	build/Emulator

build/% : src/%.pas
	fpc -gl -FEbuild $<

clean:
	rm $(TOOLS)
	rm build/*.o
	rm build/*.ppu
