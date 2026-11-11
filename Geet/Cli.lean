import Cli
import Geet.Commands
import Geet.Log
import Std.Data.HashSet

open Std (HashSet)

namespace Cli

open Cli

private def initCmd := `[Cli|
  init VIA init;
  "initialise the .geet directory."
] where
  init (_ : Parsed) : IO UInt32 := do
    Init.init;
    pure 0

private def hashBlobCmd := `[Cli|
  hashblob VIA hashBlob;
  "hash and store the given files."

  ARGS:
    ...hashes : String; "The hashes passed to the command."
] where
  hashBlob (p : Parsed) : IO UInt32 := do
    let fnames : Array String := p.variableArgsAs! String;
    for f in fnames do
      IO.println (← Commands.hashBlob $ .inl f)

    pure 0

-- TODO: remove the need to pass in the object type OR make it a flag to verify.
private def catObjCmd := `[Cli|
  catobj VIA catObj;
  "writes the contents of a hashed file to stdout."

  ARGS:
    objtype : String; "The type of the object being written to stdout."
    hash    : String; "The hash/tag of the object being written to stdout."
] where
  catObj (p : Parsed) : IO UInt32 := do
    let obj : String := p.positionalArg! "objtype" |>.as! String;
    let hash : String := p.positionalArg! "hash" |>.as! String;
    Commands.catFile (Hash.Obj.ofString obj) hash;
    pure 0

private def hashTreeCmd := `[Cli|
  hashtree VIA hashTree;
  "hash and store the cwd."
] where
  hashTree (_ : Parsed) : IO UInt32 := do
    IO.println (← Commands.hashTree (← IO.currentDir))
    pure 0

private def writeTreeCmd := `[Cli|
  writetree VIA writeTree;
  "restore the project to the state stored in the given hash."

  ARGS:
    hash : String; "The hash/tag of the tree being restored."
] where
  writeTree (p : Parsed) : IO UInt32 := do
    let hash : String := p.positionalArg! "hash" |>.as! String;
    Commands.readTree (← IO.currentDir) hash;
    Log.info s!"successfully reverted to {hash}.";
    pure 0

private def commitCmd := `[Cli|
  commit VIA commit;
  "store the state of a project."

  FLAGS:
    m, message : String; "provide a one line message for the commit."
] where
  commit (p : Parsed) : IO UInt32 := do
    let msg : String :=
      match p.flag? "message" with
      | .some p => p.as! String
       -- TODO: maybe add a number to show the amount of commits that have been made
       -- TODO: or function like git & open an editor? use a temp file and stuff
      | .none => "commit"
    IO.println (← Commands.commitTree (← IO.currentDir) msg);
    pure 0

private def logCmd := `[Cli|
  log VIA log;
  "show the commit tree."

  FLAGS:
    s, start : String; "provide a hash/tag to see the tree from a specific point."
] where
  log (p : Parsed) : IO UInt32 := do
    let start : String :=
      match p.flag? "start" with
      | .none => "@"
      | .some f => f.as! String

    Commands.logCommits start;
    pure 0

private def checkoutCmd := `[Cli|
  checkout VIA checkout;
  "return the project to a specific commit; if no commit is given, it defaults to HEAD."

  FLAGS:
    c, commit : String; "the hash/tag of the commit that you want to return to."
] where
  checkout (p : Parsed) : IO UInt32 := do
    let commit : String :=
      match p.flag? "commit" with
      | .some f => f.as! String
      | .none => "@";
    Commands.checkoutCommit commit;
    pure 0

private def tagCmd := `[Cli|
  tag VIA tag;
  "alias a specific commit to a name; if no commit is given, it defaults to HEAD."

  FLAGS:
    c, commit : String; "the commit that you want to alias."

  ARGS:
    alias  : String; "the name that you'll alias the commit to."
] where
  tag (p : Parsed) : IO UInt32 := do
    let alias : String := p.positionalArg! "alias" |>.as! String;
    let commit : String :=
      match p.flag? "commit" with
      | .none => "@"
      | .some p => p.as! String;
    Commands.createTag alias commit;
    pure 0

private partial def kCmd := `[Cli|
  k VIA k;
  "visualise the commit history for the project."
] where
  k (_ : Parsed) : IO UInt32 := do
    let digraph := "digraph commits {\n";
    let (objIds, digraph) := connectRefs (← Commands.walkRefs) [] digraph;
    let digraph := connectCommits (← followCommits objIds []) digraph;

    IO.println $ digraph ++ "}"
    pure 0

  connectRefs : List (String × Option String) → List String → String → (List String × String)
    | [], ids, acc => (ids, acc)
    | (f, fc) :: refs, ids, acc =>
      match fc with
      | .none => connectRefs refs ids acc
      | .some ref =>
        let refStr := s!"\"{f}\" [shape=note]\n\"{f}\" -> \"{ref}\"\n";
        connectRefs refs (ids.insert ref) (acc ++ refStr)

  followCommits : List String → List (String × Option String) → IO (List (String × Option String))
    | [], acc => pure acc
    | oid :: oids, acc =>
      if acc.any (·.fst == oid)
      then followCommits oids acc
      else do
        let (_, .some parent, _) ← Commands.getCommit oid
          | followCommits oids $ acc.insert (oid, .none)

        followCommits (oids.insert parent) $ acc.insert (oid, .some parent)

  connectCommits : List (String × Option String) → String → String
    | [], acc => acc
    | (oid, parent) :: comms, acc =>
      let acc := acc ++ s!"\"{oid}\" [shape=box style=filled label=\"{oid}\"]\n";
      match parent with
      | .none => connectCommits comms acc
      | .some p =>
        let p := s!"\"{oid}\" -> \"{p}\"\n";
        connectCommits comms (acc ++ p)

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
    logCmd;
    checkoutCmd;
    tagCmd;
    kCmd
]

end Cli
