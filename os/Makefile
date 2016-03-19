arch ?= x86_64

kernel := build/kernel-$(arch).bin
iso := build/os-$(arch).iso

linker_script := src/arch/$(arch)/linker.ld
grub_cfg := src/arch/$(arch)/grub.cfg
assembly_source_files := $(wildcard src/arch/$(arch)/*.asm)
assembly_object_files := $(patsubst src/arch/$(arch)/%.asm, build/arch/$(arch)/%.o, $(assembly_source_files))
cc_source_files := $(wildcard src/*.cc)
cc_object_files := $(patsubst src/%.cc, build/%.o, $(cc_source_files))

cc_warnings := -Wall -Wextra
cc_flags :=  -O0 -std=c++14 -fno-rtti -ffreestanding -fno-exceptions -nostdlib

.PHONY: all clean run iso debug

all: $(kernel)

clean:
	rm -fr build/

run: iso
	qemu-system-x86_64 -cdrom $(iso)

debug: iso
	qemu-system-x86_64 -cdrom $(iso) -s -S

iso: $(iso)

$(iso): $(kernel) $(grub_cfg)
	mkdir -p build/isofiles/boot/grub
	cp $(grub_cfg) build/isofiles/boot/grub/
	cp $(kernel) build/isofiles/boot/kernel.bin
	grub-mkrescue -o $(iso) -d /usr/lib/grub/i386-pc build/isofiles
	rm -r build/isofiles

$(kernel): $(assembly_object_files) $(linker_script) $(cc_object_files)
	ld -n -o $(kernel) -T $(linker_script) $(assembly_object_files) $(cc_object_files)

build/arch/$(arch)/%.o: src/arch/$(arch)/%.asm
	mkdir -p $(shell dirname $@)
	nasm -f elf64 $< -o $@

build/%.o: src/%.cc
	mkdir -p build
	g++ $(cc_flags) -c -o $@ $<
