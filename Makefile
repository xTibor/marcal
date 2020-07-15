TOOLS := \
	build/Emulator \
	build/Assembler

EXAMPLES := \
	build/Example01.t \
	build/Example02.t \
	build/Example03.t \
	build/Multiply.t  \
	build/Jumps.t \
	build/Data.t \
	build/System.t

run: $(TOOLS) $(EXAMPLES)
	build/Emulator build/System.t

asm: $(TOOLS) $(EXAMPLES)

build/% : src/%.pas
	fpc -gl -FEbuild $< \
		-Fusrc/Common \
		-Fusrc/Emulator

build/%.t : asm/%.s build/Assembler
	build/Assembler $< $@

clean:
	rm -f $(TOOLS)
	rm -f build/*.o
	rm -f build/*.ppu
	rm -f build/*.t
