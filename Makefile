TOOLS := \
	build/Emulator \
	build/Assembler

EXAMPLES := \
	build/Example01.t \
	build/Example02.t \
	build/Example03.t

run: $(TOOLS) $(EXAMPLES)
	build/Emulator build/Example01.t

asm: $(TOOLS) $(EXAMPLES)

build/% : src/%.pas
	fpc -gl -FEbuild $<

build/%.t : asm/%.s build/Assembler
	build/Assembler $< $@

clean:
	rm -f $(TOOLS)
	rm -f build/*.o
	rm -f build/*.ppu
	rm -f build/*.t
