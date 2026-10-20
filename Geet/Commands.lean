import Geet.Log
open Lean

namespace Init
open System

def geetDir : String := ".geet"
def objDir : String := ".geet/objects"
def refDir : String := ".geet/refs"
def tagDir : String := ".geet/refs/tags"
def headFile : String := ".geet/refs/HEAD"

def maxDepth : Nat := 10 -- TODO: make this customisable from some .env file

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
  Log.info "created .geet/HEAD file."

private def addTagDir (fp : FilePath) : IO Unit := do
  let .false ← (FilePath.isDir ∘ (· / tagDir) $ fp).toIO
    | pure ()

  Init.addDir fp tagDir;
  Log.info "created .geet/refs/tags directory."

private def addRefDir (fp : FilePath) : IO Unit := do
  let .false ← (FilePath.isDir ∘ (· / refDir) $ fp).toIO
    | pure ()

  Init.addDir fp refDir;
  Log.info "created .geet/refs directory."

private def addObjDir (fp : FilePath) : IO Unit := do
  let .false ← (FilePath.isDir ∘ (· / objDir) $ fp).toIO
    | pure ()

  Init.addDir fp objDir;
  Log.info "created .geet/objects directory."

protected def init : IO Unit := do
  let exists? ← Init.geetDirExists?;
  if let .some parent := exists?
  then Log.err s!".geet directory already exists at {parent}."
  else do
    let curr ← IO.currentDir;
    Init.addDir curr geetDir;
    Log.info s!"initialised .geet directory in {curr}."

    Init.addObjDir curr
    Init.addRefDir curr
    Init.addTagDir curr
    Init.addHeadFile curr

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

private def ignorableFiles := [".geet", ".git", ".lake", ".DS_Store", "thumbs.db", "desktop.ini"]

private def readRef (geet : FilePath) (ref : String) : IO (Option String) := do
  let dir := geet / Init.refDir / ref;
  let .true ← (FilePath.pathExists dir).toIO
    | Log.err "ref doesn't exist."; pure .none

  let fc ← IO.FS.readFile (geet / Init.refDir / ref)
  if fc.isEmpty
  then pure .none
  else pure $ .some fc

private def setRef (geet : FilePath) (ref : String) : String → IO Unit :=
  IO.FS.writeFile (geet / Init.refDir / ref)

private def getObjId (geet : FilePath) : String → IO (Option String)
  | "@" => readRef geet "HEAD"
  | n =>
    let toTry := [n, s!"refs/{n}", s!"refs/tags/{n}"] -- TODO: add refs/heads/{n}
    if isHex n
    then pure $ .some n
    else tryDirs geet toTry
where
  tryDirs : FilePath → List String → IO (Option String)
    | _, [] => pure .none
    | geet, d :: dirs => do
      let dir := geet / ".geet" / d
      let .true ← (FilePath.pathExists dir).toIO
        | tryDirs geet dirs

      pure $ .some (← IO.FS.readFile dir)

  -- from https://github.com/leanprover/lean4/blob/c9afcddf514a6772bd4d649f7cfc04eff0b9f15e/src/lake/Lake/Util/String.lean#L26-L37
  isHex (s : String) : Bool :=
    s.utf8ByteSize.all fun i h =>
      let c := s.getUTF8Byte ⟨i⟩ h
      if c ≤ 57 then -- '9'
        48 ≤ c -- '0'
      else if c ≤ 102 then -- 'f'
        97 ≤ c -- 'a'
      else if c ≤ 70 then -- 'F'
        65 ≤ c -- 'A'
      else
        false

-- you can either pass in a file name or a full path (it's not an error).
protected def hashBlob (fname : Except String FilePath) (geetDir : Option FilePath := .none) : IO String := do
  let .some geet ← (
    match geetDir with
    | .some geet => pure $ .some geet
    | .none => Init.geetDirExists?
  ) | Log.err "failed to find .geet directory."; pure ""

  let curr ← IO.currentDir;
  let fp :=
    match fname with
    | .error fname => curr / fname
    | .ok fname    => fname

  let fc ← IO.FS.readFile fp;
  Hash.makeObjFile .blob geet fc

protected partial def hashTree (dir : FilePath) (geetDir : Option FilePath := .none): IO String := do
  let .some geet ← (
    match geetDir with
    | .some geet => pure $ .some geet
    | .none => Init.geetDirExists?
  ) | Log.err "failed to find .geet directory."; pure ""

  let entries := (← FilePath.readDir dir).toList;
  let hashed ← handleEntries geet entries []
  Hash.makeObjFile .tree geet (entriesToContent hashed)
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
          let hash ← Commands.hashBlob (.ok edir) (geetDir := .some geet)
          handleEntries geet es ((.blob, e.fileName, hash) :: acc)
        else do
          let hash ← Commands.hashTree edir (geetDir := .some geet)
          handleEntries geet es ((.tree, e.fileName, hash) :: acc)

  entriesToContent : List (Hash.Obj × String × String) → String
    | [] => ""
    | [(obj, fn, hash)] => s!"{obj} {hash} {fn}"
    | (obj, fn, hash) :: es => s!"{obj} {hash} {fn}\n" ++ entriesToContent es

protected def catFile (obj : Hash.Obj) (obj_fn : String) : IO Unit := do
  let .some geet ← Init.geetDirExists?
    | Log.err "failed to find .geet directory"

  let .some id ← getObjId geet obj_fn
    | Log.err s!"failed to find file {obj_fn}."

  let dir := geet / Init.objDir / id
  let (obj', fc) := Hash.strip (← IO.FS.readBinFile dir)
  if obj == obj'
  then IO.println $ String.fromUTF8! fc
  else Log.err "expected a(n) {obj} file, but got a(n) {obj'} file."

protected partial def readTree (curr : FilePath) (treeHash : String) (geetDir : Option FilePath := .none) : IO Unit := do
  let .some geet ← (
    match geetDir with
    | .some geet => pure $ .some geet
    | .none => Init.geetDirExists?
  ) | Log.err "failed to find .geet directory."
  let objDir := geet / Init.objDir

  let .some id ← getObjId geet treeHash
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

protected def commitTree (dir : FilePath) (msg : String) : IO String := do
  let .some geet ← Init.geetDirExists?
    | Log.err "failed to find .geet directory."; pure ""

  let thash ← Commands.hashTree dir (geetDir := .some geet)
  let prev ← readRef geet "HEAD"
  let data :=
    match prev with
    | .none => s!"tree {thash}\n\n{msg}"
    | .some p => s!"tree {thash}\nparent {p}\n\n{msg}"
  let hash ← Hash.makeObjFile .commit geet data
  setRef geet "HEAD" hash
  pure hash

private partial def traceCommits (geet : FilePath) (commit : FilePath) : IO Unit := do
  let contents@(_, phash, _) := parseCommit ∘ (·.snd) $ Hash.strip (← IO.FS.readBinFile $ geet / Init.objDir / commit)
  IO.println $ compose commit contents
  match phash with
  | .none => pure ()
  | .some next => do
    match (← getObjId geet next) with
    | .none => unreachable! -- any stored commit must point to an actual commit.
    | .some next => traceCommits geet next
where
  parseCommit (commit : ByteArray) : (String × Option String × String) :=
    let commit := String.fromUTF8! commit
    let get_snd (s : String) : String :=
      match s.splitOn with
      | [_, hash] => hash
      | _ => unreachable!
    match (commit.splitOn (sep := "\n")).filter (not $ ·.isEmpty) with
    | [tree, phash, msg] => (tree, .some $ get_snd phash, msg)
    | [tree, msg] => (tree, .none, msg)
    | _ => unreachable!

  compose (commit : FilePath) (contents : String × Option String × String) : String :=
    match commit.fileName with
    | .none => unreachable! -- we used it to read the file.
    | .some commit => s!"commit {commit}\n{contents.fst}\n\n  {contents.snd.snd}\n"

protected def logCommits (start : String) : IO Unit := do
  let .some geet ← Init.geetDirExists?
    | Log.err "failed to find .geet directory."

  let .some start ← getObjId geet start
    | Log.err s!"failed to find hash {start}."

  traceCommits geet start

protected def checkoutCommit (commit : String) : IO Unit := do
  let .some geet ← Init.geetDirExists?
    | Log.err "failed to find .geet directory"

  let .some id ← getObjId geet commit
    | Log.err s!"failed to find the commit {commit}."

  setRef geet "HEAD" id
  let dir := geet / Init.objDir / id
  let .some thash ← getTreeHash dir
    | pure ()
  Commands.readTree (← IO.currentDir) thash (geetDir := .some geet)
  Log.info s!"successfully reverted to {if commit == "@" then "HEAD" else commit}."
where
  getTreeHash (commit : FilePath) : IO (Option String) := do
    let (.commit, fc) := Hash.strip (← IO.FS.readBinFile commit)
      | Log.err "expected a commit hash."; pure .none

    let fc := String.fromUTF8! fc;
    match fc.splitOn (sep := "\n") with
    | tree :: _ => do
      let [_, thash] := tree.splitOn
        | unreachable!
      pure $ .some thash
    | _ => pure .none

protected def createTag (alias commit : String) : IO Unit :=
  if h : alias ≠ "HEAD"
  then aux alias commit h
  else Log.err "alias cannot be HEAD."
where
  aux (alias commit : String) (_ : alias ≠ "HEAD") : IO Unit := do
    let .some geet ← Init.geetDirExists?
      | Log.err "failed to find .geet directory."

    let .false ← (FilePath.pathExists $ geet / Init.tagDir / alias).toIO
      | Log.err "alias already exists."

    let .some commitId ← getObjId geet commit
      | Log.err s!"failed to find commit {commit}"

    setRef geet s!"tags/{alias}" commitId
    Log.info s!"successfully added alias {alias}"

end Commands
