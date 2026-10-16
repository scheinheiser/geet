import Geet

def process_cmd : List String → IO Unit
  | "init" :: _ => Init.init
  | "hash-obj" :: fname :: _ => Hash.makeObj fname
  | "cat-obj" :: fname :: _ => Hash.catObj fname
  | _ => pure ()

def main (args : List String) : IO Unit := do
  process_cmd args;

  for s in args do
    IO.println s!"arg ↦ {s}"
  IO.println s!"Hello, lean!"
