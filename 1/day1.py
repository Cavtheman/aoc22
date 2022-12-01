if __name__ == "__main__":
    elf_list = [0]
    for line in open ("inputSimple.txt", "r"):
        if line == "\n":
            elf_list.append (0)
        else:
            elf_list[-1] += int (line)
    elf_list.sort(reverse=True)
    print (elf_list[0])
    print (sum (elf_list[:3]))
