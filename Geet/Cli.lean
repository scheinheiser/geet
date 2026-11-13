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
  hash VIA hash;
  "hash and store the given file(s)."

  ARGS:
    ...hashes : String; "The hashes passed to the command."
] where
  hash (p : Parsed) : IO UInt32 := do
    let fnames : Array String := p.variableArgsAs! String;
    for f in fnames do
      IO.println (← Commands.hashBlob $ .inl f)

    pure 0

private def catObjCmd := `[Cli|
  "cat-obj" VIA cat;
  "writes the contents of a hashed file to stdout."

  FLAGS:
    t, type : String; "The type of the object being written to stdout."

  ARGS:
    hash    : String; "The hash/tag of the object being written to stdout."
] where
  cat (p : Parsed) : IO UInt32 := do
    let obj : Option Hash.Obj := p.flag? "type" >>= pure ∘ Hash.Obj.ofString ∘ (·.as! String);
    let hash : String := p.positionalArg! "hash" |>.as! String;
    Commands.catFile (obj := obj) hash;
    pure 0

private def hashTreeCmd := `[Cli|
  "hash-tree" VIA hashTree;
  "hash and store the cwd."
] where
  hashTree (_ : Parsed) : IO UInt32 := do
    IO.println (← Commands.hashTree (← IO.currentDir))
    pure 0

private def writeTreeCmd := `[Cli|
  "write-tree" VIA writeTree;
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

  EXTENSIONS:
    defaultValues! #[("start", "@")]
] where
  log (p : Parsed) : IO UInt32 := do
    let start : String := p.flag! "start" |>.as! String;

    Commands.logCommits start;
    pure 0

private def checkoutCmd := `[Cli|
  checkout VIA checkout;
  "checkout a specific branch of the project or return the project to a specific commit."

  ARGS:
    commit : String; "the name of the branch or hash/tag of the commit that you want to return to."
] where
  checkout (p : Parsed) : IO UInt32 := do
    let commit : String := p.positionalArg! "commit" |>.as! String;
    Commands.checkoutCommit commit;
    pure 0

private def tagAddCmd := `[Cli|
  add VIA tagAdd;
  "alias a specific commit to a name; if no commit is given, it defaults to HEAD."

  FLAGS:
    c, commit : String; "the commit that you want to alias."

  ARGS:
    alias : String; "the name that you'll alias the commit to."

  EXTENSIONS:
    defaultValues! #[("commit", "@")]
] where
  tagAdd (p : Parsed) : IO UInt32 := do
    let alias : String := p.positionalArg! "alias" |>.as! String;
    let commit : String := p.flag! "commit" |>.as! String;

    Commands.createTag alias commit;
    pure 0

private def tagCmd := `[Cli|
  tag VIA tag;
  "create a new tag or view all tags."

  FLAGS:
    l, list; "list all tags."

  SUBCOMMANDS:
    tagAddCmd
] where
  name (fp : String) : String := (fp.splitOn (sep := "/")).getLast!

  tag (p : Parsed) : IO UInt32 := do
    unless not $ p.hasFlag "list" do
      for (r, h) in ← Commands.walkRefs (pre := ".geet/refs/tags") do
          IO.println s!"{name r} → {h.get!}"

    pure 0

private partial def kCmd := `[Cli|
  k VIA k;
  "visualise the commit history for the project."
] where
  k (_ : Parsed) : IO UInt32 := do
    let .some geet ← Init.geetDirExists?
      | Log.err "failed to find .geet directory.";

    let digraph := "digraph commits {\n";
    let (objIds, digraph) := connectRefs (← Commands.walkRefs (geetDir := .some geet)) [] digraph;
    let digraph := connectCommits (← Commands.followCommits objIds geet []) digraph;

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

  connectCommits : List (String × String × Option String × String) → String → String
    | [], acc => acc
    | (oid, _, parent, _) :: comms, acc =>
      let acc := acc ++ s!"\"{oid}\" [shape=box style=filled label=\"{oid}\"]\n";
      match parent with
      | .none => connectCommits comms acc
      | .some p =>
        let p := s!"\"{oid}\" -> \"{p}\"\n";
        connectCommits comms (acc ++ p)

private partial def branchAddCmd := `[Cli|
  add VIA branchAdd;
  "create a branch in the project history."

  FLAGS:
    s, start : String; "the commit that you want to brach from; defaults to HEAD."

  ARGS:
    alias : String; "the branch name."

  EXTENSIONS:
    defaultValues! #[("start", "@")]
] where
  branchAdd (p : Parsed) : IO UInt32 := do
    let alias : String := p.positionalArg! "alias" |>.as! String;
    let start : String := p.flag! "start" |>.as! String;

    Commands.createBranch alias start;
    pure 0

-- TODO: make subcommands for branch! list to see branches, add to make a new branch
-- do the same for tag too!
private partial def branchCmd := `[Cli|
  branch VIA branch;
  "create a branch in the project history or view all branches."

  FLAGS:
    l, list; "list all branches in the project history."

  SUBCOMMANDS:
    branchAddCmd
] where
  name (fp : String) : String := (fp.splitOn (sep := "/")).getLast!

  branch (p : Parsed) : IO UInt32 := do
    unless not $ p.hasFlag "list" do
      let .some geet ← Init.geetDirExists?
        | Log.err "failed to find .geet directory."

      let curr := (← Commands.getBranch? (geetDir := .some geet)).getD ""
      for (r, _) in ← Commands.walkRefs (geetDir := .some geet) (pre := ".geet/refs/heads") do
        let r := name r;
        if r == curr
        then IO.println s!"* {r}"
        else IO.println s!"  {r}"

    pure 0

private partial def statusCmd := `[Cli|
  status VIA status;
  "report the status of the project."
] where
  status (_ : Parsed) : IO UInt32 := do
    let .some geet ← Init.geetDirExists?
      | Log.err "failed to find .geet directory."

    let head ← Commands.getObjId geet "@"
    match ← Commands.getBranch? (geetDir := geet) with
    | .none   => IO.println s!"HEAD detached at {head}"
    | .some b => IO.println s!"On branch {b}"

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
    logCmd;
    checkoutCmd;
    tagCmd;
    kCmd;
    branchCmd;
    statusCmd
]

end Cli
