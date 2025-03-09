# compiler

JIT compiled toy idiot mixture language that's kinda like python in that it's both easy to
write and understand but also compiles to something semi-performant at runtime.

Strongly typed if I can be bothered.

I'm probably going to write a fully compiled, basic version first and go from there.

## Language Example

```none
struct Tree:
  left: Tree
  right: Tree
  value: int

  init():
    self.left = Tree()
    self.right = Tree()

  iter(self):
    [self.left, self.right]

  fn bfs(self, targ: int) -> bool:
    queue = [self]
    while (root := queue.safepop()):
      if root.value == targ: return True
      queue.extendfrom(root)
    False
```

