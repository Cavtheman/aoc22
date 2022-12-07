class FileTree():
    def __init__ (self, key, value, leaves):
        self.key = key
        self.value = value
        self.leaves = leaves
        self.child_value = None
    def insert (self, keyChain, val):
        if len (keyChain) == 2:
            self.leaves.update ({keyChain[1]: FileTree (keyChain[1], val, {})})
        else:
            if keyChain[1] in self.leaves:
                self.leaves[keyChain[1]].insert (keyChain[1:], val)

    def update (self, keyChain, val):
        if len (keyChain) == 1:
            if self.value is None:
                self.value = val
            else:
                self.value += val
        else:
            self.leaves[keyChain[1]].update (keyChain[1:], val)

    def __str__ (self):
        return "FileTree: " + str ((self.key, self.value, self.leaves))

    def update_weights (self):
        child_weights = sum ([ child.update_weights() for child in self.leaves.values() ])
        if self.value is None:
            self.child_value = child_weights
        else:
            self.child_value = self.value + child_weights
        return self.child_value

def get_small_files (tree, n):
    ret_val = []
    def rec_helper (tree):
        if tree.child_value <= n:
            ret_val.append (tree.child_value)
        for child in tree.leaves.values():
            rec_helper (child)
    rec_helper (tree)
    return sum (ret_val)

def min_larger_tree (lst, n):
    larger_elems = [ elem for elem in lst if elem is not None and elem.child_value >= n ]
    if larger_elems:
        return sorted (larger_elems, key=lambda x: x.child_value)[0]
    else:
        temp = FileTree ("infinite", float("infinity"), {})
        temp.child_value = float("infinity")
        return temp

def remove_largest_small (max_space, unused_wanted, tree):
    space_left = max_space - tree.child_value
    space_wanted = unused_wanted - space_left
    def helper (tree):
        children = [ helper (child)
                     for child in tree.leaves.values() ]
        children.append (tree.child_value)

        ret_val = [ child for child in children if child >= space_wanted ]
        ret_val.append (float("infinity"))
        return min (ret_val)
    return helper (tree)

if __name__ == "__main__":
    lines = [ line for line in open ("inputSimple.txt") ]
    lines = [ line for line in open ("input.txt") ]

    full_tree = FileTree ("/", None, {})
    cur_dir = ["/"]
    for line in lines[1:]:
        if line[2:7] == "cd ..":
            cur_dir = cur_dir[:-1]
        elif line[2:4] == "cd":
            dir_name = line[5:]
            cur_dir.append (dir_name)
            #full_tree.insert (cur_dir, None)
        elif line[2:4] == "ls":
            continue
        elif line[:3] == "dir":
            dir_name = line[4:]
            cur_dir.append (dir_name)
            full_tree.insert (cur_dir, None)
            cur_dir = cur_dir[:-1]
        else:
            weight, _name = line.split()
            full_tree.update (cur_dir, int (weight))
    full_tree.update_weights()
    print (get_small_files (full_tree, 100000))
    print (remove_largest_small (70000000, 30000000, full_tree))
