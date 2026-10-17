import Geet

def process_cmd : List String → IO Unit
  | "init" :: _ => Init.init
  | "hash-obj" :: fname :: _ => do IO.println (← Hash.hashBlob fname)
  | "cat-obj" :: obj :: fname :: _ => Hash.catFile (Hash.Obj.ofString obj) fname
  | "hash-tree" :: dname :: _ => do IO.println (← Hash.hashTree (← IO.currentDir).toString dname)
  | _ => pure ()

def main (args : List String) : IO Unit := do
  process_cmd args;
  IO.println s!"Hello, lean!"
