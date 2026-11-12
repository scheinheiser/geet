import Geet.Log
open Lean

instance : Functor (Prod α) where
  map {β} {ζ} (f : β → ζ) (p : α × β) := (p.fst, f p.snd)

namespace Init
open System

def geetDir   : String := ".geet"
def objDir    : String := ".geet/objects"
def refDir    : String := ".geet/refs"
def headsDir  : String := ".geet/refs/heads"
def tagDir    : String := ".geet/refs/tags"
def headFile  : String := ".geet/refs/HEAD"

-- TODO: make this customisable from some .env file or something
def defaultBranch : String := "main"
def maxDepth : Nat := 10

private def addDir (fp : FilePath) : String → IO Unit :=
  IO.FS.createDir ∘ (fp / ·)

private def addFile (fp : FilePath) : String → IO Unit :=
  flip IO.FS.writeFile "" ∘ (fp / ·)

protected def geetDirExists? : IO (Option FilePath) := do
  let curr ← IO.currentDir;
  aux maxDepth curr
where
  aux : Nat → FilePath → IO (Option FilePath)
    | 0, _ => pure .none
    | n + 1, fp => do
      let .false ← (FilePath.isDir ∘ (· / geetDir)  $ fp).toIO
        | pure $ .some fp

      if let .some parent := fp.parent
      then aux n parent
      else pure .none

private def addHeadFile (fp : FilePath) : IO Unit := do
  let .false ← (FilePath.pathExists ∘ (· / headFile) $ fp).toIO
    | pure ()

  Init.addFile fp headFile

private def addTagDir (fp : FilePath) : IO Unit := do
  let .false ← (FilePath.isDir ∘ (· / tagDir) $ fp).toIO
    | pure ()

  Init.addDir fp tagDir;

private def addRefDir (fp : FilePath) : IO Unit := do
  let .false ← (FilePath.isDir ∘ (· / refDir) $ fp).toIO
    | pure ()

  Init.addDir fp refDir;

private def addHeadsDir (fp : FilePath) : IO Unit := do
  let .false ← (FilePath.isDir ∘ (· / headsDir) $ fp).toIO
    | pure ()

  Init.addDir fp headsDir

private def addObjDir (fp : FilePath) : IO Unit := do
  let .false ← (FilePath.isDir ∘ (· / objDir) $ fp).toIO
    | pure ()

  Init.addDir fp objDir;

protected def init : IO Unit := do
  let exists? ← Init.geetDirExists?;
  if let .some parent := exists?
  then Log.err s!".geet directory already exists at {parent}."
  else do
    let curr ← IO.currentDir;
    Init.addDir curr geetDir;
    Log.info s!"initialised empty .geet directory in {curr}."

    Init.addObjDir   curr
    Init.addRefDir   curr
    Init.addHeadsDir curr
    Init.addTagDir   curr
    Init.addHeadFile curr

    -- set HEAD to main
    Init.addFile    (curr / headsDir) defaultBranch
    IO.FS.writeFile (curr / headFile) s!"ref: heads/{defaultBranch}"

end Init

namespace Hash

open System

inductive Obj where
  | blob
  | tree
  | commit
  | any -- we don't care what the obj type is here
  deriving Inhabited

instance : BEq Obj where
  beq : Obj → Obj → Bool
    | .blob, .blob      => true
    | .tree, .tree      => true
    | .commit, .commit  => true
    | .any, _ | _, .any => true
    | _, _ => false

instance : ToString Obj where
  toString : Obj → String
    | .commit => "commit"
    | .blob   => "blob"
    | .tree   => "tree"
    | .any    => "any"

protected def Obj.ofString : String → Obj
  | "blob"   => .blob
  | "tree"   => .tree
  | "commit" => .commit
  | _ => .any

private def Obj.toBytes : Obj → ByteArray
  | .any => .empty
  | obj => (toString obj ++ "\x00").toByteArray

private def Obj.byteLength : Obj → Nat
  | .any  => 0
  | obj => (toString obj).length + 1 -- we need to account for the null byte

-- strip the object prefix from an object file.
private def strip (b : ByteArray) : Obj × ByteArray :=
  let obj := Obj.ofString ∘ String.fromUTF8! ∘ List.toByteArray $ b.toList.takeWhile (· != 0)
  (obj, .mk (b.data.drop $ Obj.byteLength obj))

private def hashStr : String → String :=
  BitVec.toHex ∘ UInt64.toBitVec ∘ String.hash

private def makeObjFile (obj : Obj) (geetDir : FilePath) (data : String) : IO String := do
  let obj_fn := hashStr data;
  let fc := obj.toBytes ++ data.toByteArray;
  IO.FS.writeBinFile (geetDir / Init.objDir / obj_fn) fc
  pure obj_fn

end Hash

namespace Commands

open System

structure RefValue where
  symbolic? : Bool
  value     : Option String

private def nonSym (value : Option String) : RefValue := { symbolic? := .false, value }
private def sym    (value : Option String) : RefValue := { symbolic? := .true,  value }

private def ignorableFiles := [".geet", ".git", ".lake", ".DS_Store", "thumbs.db", "desktop.ini"]

-- read a ref file, chasing any symbolic refs.
-- returns both the name of the ref file that was read, and id found.
private partial def _readRef (geet : FilePath) (ref : String) (deref? : Bool) : IO (String × RefValue) := do
  let dir := geet / Init.refDir / ref;
  let .true ← (FilePath.pathExists dir).toIO
    | Log.err s!"ref {ref} doesn't exist."

  let fc ← IO.FS.readFile (geet / Init.refDir / ref)
  match fc.splitOn (sep := "ref: ") with
  | [ nonSymId ] => if nonSymId.isEmpty then pure (ref, nonSym .none) else pure (ref, nonSym $ .some nonSymId)
  | [ _, isSym ] => if deref? then _readRef geet isSym deref? else pure (ref, sym $ .some isSym)
  | _ => unreachable!

private partial def readRef (geet : FilePath) (ref : String) (deref? : Bool := true) : IO RefValue := do
  pure $ (← _readRef geet ref deref?).snd

private def setRef (geet : FilePath) (ref : String) (hash : RefValue) (deref? : Bool := true) : IO Unit := do
  let (refName, _) ← _readRef geet ref deref?;
  let hash :=
    if hash.symbolic?
    then s!"ref: {hash.value.get!}"
    else hash.value.get!;

  IO.FS.writeFile (geet / Init.refDir / refName) hash

-- get the commit hash from a given string.
-- the string is either @ (HEAD), a tag or a commit hash.
protected def getObjId (geet : FilePath) (commit : String) (deref? : Bool := true) : IO (Option String) :=
  match commit with
  | "@" => do pure (← readRef geet "HEAD" deref?).value
  | n =>
    let toTry := [n, s!"refs/{n}", s!"refs/tags/{n}", s!"refs/heads/{n}"]
    if n.all (·.isHexDigit)
    then pure $ .some n
    else tryDirs geet toTry
where
  tryDirs : FilePath → List String → IO (Option String)
    | _, [] => pure .none
    | geet, d :: dirs => do
      let dir := geet / ".geet" / d
      let .true ← (FilePath.pathExists dir).toIO
        | tryDirs geet dirs

      let fc ← IO.FS.readFile dir;
      match fc.splitOn (sep := "ref: ") with
      | [ nonSymId ]   => if nonSymId.isEmpty then pure .none else pure $ .some nonSymId
      | [ _, isSym ] => do pure (← readRef geet isSym deref?).value
      | _ => unreachable!

-- traverse the ref directory, and return the ref filepath with the possible commit hash.
protected partial def walkRefs (geetDir : Option FilePath := .none) : IO (List (String × Option String)) := do
  let .some geet ← (
    match geetDir with
    | .some geet => pure $ .some geet
    | .none => Init.geetDirExists?
  ) | Log.err "failed to find .geet directory."

  let refs ← getFiles [] $ geet / Init.refDir
  getFC geet refs
where
  getFiles (refs : List FilePath) (curr : FilePath) : IO (List FilePath) := do
    let mut currRefs : List FilePath := []
    for e in (← FilePath.readDir curr) do
      let file? := e.path
      let .true ← (FilePath.isDir file?).toIO
        | currRefs := file? :: currRefs

      currRefs := (← getFiles refs file?) ++ currRefs

    pure $ currRefs ++ refs

  getFC : FilePath → List FilePath → IO (List (String × Option String))
    | _, [] => pure []
    | geet, f :: fs => do
      pure $ (f.toString, ← Commands.getObjId geet f.toString (deref? := false)) :: (← getFC geet fs)

-- parse a commit file.
private def parseCommit (commit : ByteArray) : (String × Option String × String) :=
  let commit := String.fromUTF8! commit
  let get_snd (s : String) : String :=
    match s.splitOn with
    | [_, hash] => hash
    | _ => unreachable!

  match (commit.splitOn (sep := "\n")).filter (not $ ·.isEmpty) with
  | [tree, phash, msg] => (getTreeHash tree, .some $ get_snd phash, msg)
  | [tree, msg] => (getTreeHash tree, .none, msg)
  | _ => unreachable!
where
  getTreeHash (tree : String) : String :=
    match tree.splitOn with
    | [_, hash] => hash
    | _ => unreachable!

-- returns the tree, possible parent hash and message found in a commit.
protected def getCommit (commit : String) (geetDir : Option FilePath := .none) : IO (String × Option String × String) := do
  let .some geet ← (
    match geetDir with
    | .some geet => pure $ .some geet
    | .none => Init.geetDirExists?
  ) | Log.err "failed to find .geet directory."

  let dir := geet / Init.objDir / commit;
  let .true ← FilePath.pathExists dir
    | Log.err s!"failed to find commit {commit}."

  let (.commit, fc) := Hash.strip (← IO.FS.readBinFile dir)
    | Log.err "Expected a commit hash."

  pure $ parseCommit fc

-- given an possible object type and a object filename, write the contents of the object file to stdout.
protected def catFile (obj : Option Hash.Obj := .none) (obj_fn : String) : IO Unit := do
  let .some geet ← Init.geetDirExists?
    | Log.err "failed to find .geet directory"

  let .some id ← Commands.getObjId geet obj_fn
    | Log.err s!"failed to find hash {obj_fn}."

  let dir := geet / Init.objDir / id
  let (obj', fc) := String.fromUTF8! <$> Hash.strip (← IO.FS.readBinFile dir)

  match obj with
  | .none => IO.println fc
  | .some obj =>
    if obj == obj'
    then IO.println fc
    else Log.err s!"expected a(n) {obj} file, but got a(n) {obj'} file."

-- hash a given filename or filepath, saving it a blob.
protected def hashBlob (fname : String ⊕ FilePath) (geetDir : Option FilePath := .none) : IO String := do
  let .some geet ← (
    match geetDir with
    | .some geet => pure $ .some geet
    | .none => Init.geetDirExists?
  ) | Log.err "failed to find .geet directory."

  let curr ← IO.currentDir;
  let fp :=
    match fname with
    | .inl fname => curr / fname
    | .inr fname => fname

  let fc ← IO.FS.readFile fp;
  Hash.makeObjFile .blob geet fc

-- hash and save a project.
protected partial def hashTree (dir : FilePath) (geetDir : Option FilePath := .none): IO String := do
  let .some geet ← (
    match geetDir with
    | .some geet => pure $ .some geet
    | .none => Init.geetDirExists?
  ) | Log.err "failed to find .geet directory."

  let entries := (← FilePath.readDir dir).toList;
  let hashed ← handleEntries geet entries []
  Hash.makeObjFile .tree geet (hashed.map (λ (obj, fn, hash) => s!"{obj} {hash} {fn}") |> "\n".intercalate)
where
  handleEntries : FilePath → List IO.FS.DirEntry → List (Hash.Obj × String × String) → IO (List (Hash.Obj × String × String))
    | _, [], acc => pure acc
    | geet, e :: es, acc => do
      if List.elem e.fileName ignorableFiles
      then handleEntries geet es acc
      else
        let edir := e.path
        let isDir? ← edir.isDir.toIO
        if not isDir?
        then do
          let hash ← Commands.hashBlob (.inr edir) (geetDir := .some geet)
          handleEntries geet es ((.blob, e.fileName, hash) :: acc)
        else do
          let hash ← Commands.hashTree edir (geetDir := .some geet)
          handleEntries geet es ((.tree, e.fileName, hash) :: acc)

-- given a tree hash, revert all files within a project to their state at the hash.
-- TODO: modify this to handle .commits in the logic
protected partial def readTree (curr : FilePath) (treeHash : String) (geetDir : Option FilePath := .none) : IO Unit := do
  let .some geet ← (
    match geetDir with
    | .some geet => pure $ .some geet
    | .none => Init.geetDirExists?
  ) | Log.err "failed to find .geet directory."
  let objDir := geet / Init.objDir

  let .some id ← Commands.getObjId geet treeHash
    | Log.err s!"failed to find file {treeHash}."

  let dir := geet / Init.objDir / id
  let (obj, fc) := Hash.strip (← IO.FS.readBinFile dir)
  if not (obj == .tree)
  then Log.err s!"expected a tree hash, but got a {obj} hash."
  else do
    let (blobs, trees) := List.partition (·.fst == .blob) ∘ parseFC $ String.fromUTF8! fc
    writeBlobs objDir curr (blobs.map (·.snd))

    for (_, hash, dirname) in trees do
      Commands.readTree (curr / dirname) hash (geetDir := .some geet)
where
  parseFC (fc : String) : List (Hash.Obj × String × String) :=
    (fc.splitOn (sep := "\n")).map
      (match String.splitOn · with
        | [obj, hash, fn] => (Hash.Obj.ofString obj, hash, fn)
        | _ => unreachable!)

  writeBlobs : FilePath → FilePath → List (String × String) → IO Unit
    | _, _, [] => pure ()
    | odir, curr, (hash, filename) :: blobs => do
      let (_, fc) := Hash.strip (← IO.FS.readBinFile $ odir / hash)
      IO.FS.writeBinFile (curr / filename) fc
      writeBlobs odir curr blobs

-- commit (save) any changes made to the project.
protected def commitTree (dir : FilePath) (msg : String) : IO String := do
  let .some geet ← Init.geetDirExists?
    | Log.err "failed to find .geet directory."

  let thash ← Commands.hashTree dir (geetDir := .some geet)
  let {symbolic? := _, value := prev } ← readRef geet "HEAD"
  let data :=
    match prev with
    | .none => s!"tree {thash}\n\n{msg}"
    | .some p => s!"tree {thash}\nparent {p}\n\n{msg}"
  let hash ← Hash.makeObjFile .commit geet data
  setRef geet "HEAD" { symbolic? := false, value := .some hash }
  pure hash

-- trace a commit and its parents, ensuring that no duplicates are followed.
-- returns a list of the commit hash, tree hash, possible parent hash and commit message.
protected partial def followCommits : List String → System.FilePath → List (String × String × Option String × String) → IO (List (String × String × Option String × String))
  | [], _, acc => pure acc
  | oid :: oids, geet, acc =>
    if acc.any (·.fst == oid)
    then Commands.followCommits oids geet acc
    else do
      match (← Commands.getCommit (geetDir := geet) oid) with
      | (tree, .none, msg) => Commands.followCommits oids geet $ acc.insert (oid, tree, .none, msg)
      | (tree, .some parent, msg) => Commands.followCommits (oids.insert parent) geet $ acc.insert (oid, tree, .some parent, msg)

-- display the commit history from a given start.
protected def logCommits (start : String) : IO Unit := do
  let .some geet ← Init.geetDirExists?
    | Log.err "failed to find .geet directory."

  let .some start ← Commands.getObjId geet start
    | Log.err s!"failed to find hash {start}."

  let commits ← Commands.followCommits [start] geet []
  let logMsg : String := "\n\n".intercalate $ commits.reverse.map compose
  IO.println logMsg
where
  compose : String × String × Option String × String → String
    | (commit, tree, .none, msg)   => s!"commit {commit}\ntree   {tree}\n\n {msg}"
    | (commit, tree, .some p, msg) => s!"commit {commit}\ntree   {tree}\nparent {p}\n\n {msg}"

-- is given identifier a branch?
private def isBranch? (branch : String) (geetDir : Option FilePath := .none) : IO Bool := do
  let .some geet ← (
    match geetDir with
    | .some geet => pure $ .some geet
    | .none => Init.geetDirExists?
  ) | Log.err "failed to find .geet directory."

  (FilePath.pathExists $ geet / Init.headsDir / branch).toIO

-- revert the project to a specific commit
protected def checkoutCommit (commit : String) : IO Unit := do
  let .some geet ← Init.geetDirExists?
    | Log.err "failed to find .geet directory"

  let .some id ← Commands.getObjId geet commit
    | Log.err s!"failed to find the commit {commit}."

  let ref : RefValue :=
    if (← isBranch? (geetDir := .some geet) commit)
    then { symbolic? := true, value := .some s!"heads/{commit}" }
    else { symbolic? := false, value := .some id};

  setRef geet "HEAD" ref (deref? := .false)
  let (thash, _, _) ← Commands.getCommit (geetDir := geet) id

  Commands.readTree (← IO.currentDir) thash (geetDir := .some geet)
  Log.info s!"successfully reverted to {if commit == "@" then "HEAD" else commit}."

-- is the alias invalid?
private def invalidAlias? (geet : FilePath) (alias : String) : IO Bool := do
  let .false ← (FilePath.pathExists $ geet / Init.headsDir / alias).toIO
    | pure .true

  let .false ← (FilePath.pathExists $ geet / Init.tagDir / alias).toIO
    | pure .true

  pure $ alias == "HEAD"

-- create a commit tag, given an alias and a commit hash
protected def createTag (alias commit : String) : IO Unit := do
  let .some geet ← Init.geetDirExists?
    | Log.err "failed to find .geet directory."

  unless not (← invalidAlias? geet alias) do
    Log.err s!"invalid alias {alias}.";

  let .some commitId ← Commands.getObjId geet commit
    | Log.err s!"failed to find commit {commit}"

  setRef geet s!"tags/{alias}" { symbolic? := false, value := .some commitId }
  Log.info s!"successfully created tag {alias}"

-- create a branch given a branch name and a commit to branch from.
protected def createBranch (branch commit : String) : IO Unit := do
  let .some geet ← Init.geetDirExists?
    | Log.err "failed to find .geet directory."

  unless not (← invalidAlias? geet branch) do
    Log.err s!"invalid branch name {branch}.";

  let .some commit ← Commands.getObjId geet commit
    | Log.err s!"expected a commit hash."

  -- NOTE: may or may not be an issue
  IO.FS.writeFile (geet / Init.headsDir / branch) commit;
  Log.info s!"successfully created branch {branch}";

-- get the name of the current branch, or return nothing if detached.
protected def getBranch? (geetDir : Option FilePath := .none) : IO (Option String) := do
  let .some geet ← (
    match geetDir with
    | .some geet => pure $ .some geet
    | .none => Init.geetDirExists?
  ) | Log.err "failed to find .geet directory."

  let hd ← readRef geet "HEAD" (deref? := false)
  if not hd.symbolic?
  then pure .none
  else pure (FilePath.fileName hd.value.get!)

end Commands
