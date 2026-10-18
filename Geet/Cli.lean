import Cli
import Geet.Commands
import Geet.Log

namespace Cli

open Cli
private def const.{u, v} {α : Sort u} {β : Sort v} (a : α) : β → α := Function.const β a

private def initCmd := `[Cli|
  init VIA init;
  "initialise the .geet directory."
]
where
  init (_ : Parsed) : IO UInt32 := do
    Init.init;
    pure 0

private def hashBlobCmd := `[Cli|
  hashblob VIA hashBlob;
  "hash and store the given files."

  ARGS:
    ...hashes : String; "The hashes passed to the command."
]
where
  hashBlob (p : Parsed) : IO UInt32 := do
    let fnames : Array String := p.variableArgsAs! String;
    for f in fnames do
      IO.println (← Hash.hashBlob (.error f))

    pure 0

private def catObjCmd := `[Cli|
  catobj VIA catObj;
  "writes the contents of a hashed file to stdout."

  ARGS:
    objtype : String; "The type of the object being written to stdout."
    hash    : String; "The hash of the object being written to stdout."
]
where
  catObj (p : Parsed) : IO UInt32 := do
    let obj : String := p.positionalArg! "objtype" |>.as! String;
    let hash : String := p.positionalArg! "hash" |>.as! String;
    Hash.catFile (Hash.Obj.ofString obj) hash;
    pure 0

private def hashTreeCmd := `[Cli|
  hashtree VIA hashTree;
  "hash and store the cwd."
]
where
  hashTree (_ : Parsed) : IO UInt32 := do
    IO.println (← Hash.hashTree (← IO.currentDir))
    pure 0

private def writeTreeCmd := `[Cli|
  writetree VIA writeTree;
  "restore the project to the state stored in the given hash."

  ARGS:
    hash : String; "The hash of the tree being restored."
]
where
  writeTree (p : Parsed) : IO UInt32 := do
    let hash : String := p.positionalArg! "hash" |>.as! String;
    Hash.readTree (← IO.currentDir) hash;
    Log.info s!"successfully reverted to {hash}.";
    pure 0

private def commitCmd := `[Cli|
  commit VIA commit;
  "store the state of a project."

  FLAGS:
    m, message : String; "provide a one line message for the commit."
]
where
  commit (p : Parsed) : IO UInt32 := do
    let msg : String := p.flag! "message" |>.as! String;
    IO.println (← Hash.commitTree (← IO.currentDir) msg);
    pure 0

private def logCmd := `[Cli|
  log VIA log;
  "show the commit tree."

  FLAGS:
    s, start : String; "provide a hash to see the tree from a specific point."
]
where
  log (p : Parsed) : IO UInt32 := do
    let start : Option String :=
      match p.flag? "start" with
      | .none => .none
      | .some f => .some $ f.as! String

    Hash.logCommits (begin? := start);
    pure 0

protected def geet : Cmd := `[Cli|
  geet NOOP;
  "a cool VC thing"

  SUBCOMMANDS:
    initCmd;
    hashBlobCmd;
    catObjCmd;
    hashTreeCmd;
    writeTreeCmd;
    commitCmd;
    logCmd
]

end Cli
