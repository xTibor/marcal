TOOLS := \
	build/Emulator \
	build/Assembler

run: $(TOOLS)
	build/Emulator

build/% : src/%.pas
	fpc -gl -FEbuild $<

clean:
	rm -f $(TOOLS)
	rm -f build/*.o
	rm -f build/*.ppu
