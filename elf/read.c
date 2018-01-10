#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>


typedef struct {
    unsigned char e_ident[16];
    uint16_t e_type;
    uint16_t e_machine;
    uint32_t e_version;
    uint64_t e_entry;
    uint64_t e_phoff;
    uint64_t e_shoff;
    uint32_t e_flags;
    uint16_t e_ehsize;
    uint16_t e_phentsize;
    uint16_t e_phnum;
    uint16_t e_shentsize;
    uint16_t e_shnum;
    uint16_t e_shstrndx;
} Elf_Header_t;


const char *ELF_MAGIC = "ELF";


int main(int argc, char *argv[])
{
    void *elf_base;
    if (argc < 2) {
        exit(1);
    }
    {
        int fd;
        printf("Filename: %s\n", argv[1]);
        fd = open(argv[1], O_RDONLY);
        elf_base = mmap(NULL, 128, PROT_READ, MAP_PRIVATE, fd, 0);
        close(fd);
    }
    {
        Elf_Header_t *header;
        header = (Elf_Header_t*) elf_base;
        const char *magic_start = header->e_ident + 1;
        if (strcmp(magic_start, ELF_MAGIC) <= 0) {
            printf("Not an ELF file!\n");
            printf("%s", header->e_ident);
            exit(2);
        }
        printf("Executable has entry point address %lx\n", header->e_entry);
    }
    return munmap(elf_base, 128);
}
