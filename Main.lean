import Geet

def process_cmd : List String → IO Unit
  | "init" :: _ => Init.init
  | "hash-obj" :: fname :: _ => Hash.makeObjFile Hash.Obj.blob fname
  | "cat-obj" :: fname :: _ => Hash.catFile Hash.Obj.blob fname
  | _ => pure ()

def main (args : List String) : IO Unit := do
  process_cmd args;
  IO.println s!"Hello, lean!"
