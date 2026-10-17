import Geet

def process_cmd : List String → IO Unit
  | "init" :: _ => Init.init
  | "hash-obj" :: fname :: _ => do IO.println (← Hash.hashBlob fname)
  | "cat-obj" :: obj :: fname :: _ => Hash.catFile (Hash.Obj.ofString obj) fname
  | "hash-tree" :: _ => do IO.println (← Hash.hashTree (← IO.currentDir))
  | "write-tree" :: hash :: _ => do Hash.readTree (← IO.currentDir) hash; Log.info s!"successfully reverted to {hash}."
  | _ => IO.println "failed to recognise command"

def main (args : List String) : IO Unit := process_cmd args
