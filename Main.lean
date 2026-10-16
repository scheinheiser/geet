import Geet

def process_cmd : List String → IO Unit
  | "init" :: _ => Init.init
  | "hash-obj" :: fname :: _ => Hash.makeObj fname
  | "cat-file" :: fname :: _ => Hash.catObj fname
  | _ => pure ()

def main (args : List String) : IO Unit := do
  process_cmd args;
  IO.println s!"Hello, lean!"
