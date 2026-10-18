import Geet.Log
open Lean

namespace Init
open System

def geetDir : String := ".geet"
def objDir : String := ".geet/objects"
def headFile : String := ".geet/HEAD"

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

private def addObjDir (fp : FilePath) : IO Unit := do
  let .false ← (FilePath.isDir ∘ (· / objDir) $ fp).toIO
    | pure ()

  Init.addDir fp objDir;
  Log.info "created .geet/objects directory."

protected def init : IO Unit := do
  let exists? ← Init.geetDirExists?;
  if let .some parent := exists?
  then Log.err s!".geet directory already exists at {parent}."; pure ()
  else do
    let curr ← IO.currentDir;
    Init.addDir curr geetDir;
    Log.info s!"initialised .geet directory in {curr}."

    Init.addObjDir curr
    Init.addHeadFile curr

end Init

namespace Hash

open System

private def ignorableFiles := [".geet", ".git", ".lake", ".DS_Store", "thumbs.db", "desktop.ini"]

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
  | .blob | .tree => 5 -- we ensure to account for the null byte
  | .commit => 7
  | .any  => 0

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
  makeObjFile .blob geet fc

protected partial def hashTree (dir : FilePath) (geetDir : Option FilePath := .none): IO String := do
  let .some geet ← (
    match geetDir with
    | .some geet => pure $ .some geet
    | .none => Init.geetDirExists?
  ) | Log.err "failed to find .geet directory."; pure ""

  let entries := (← FilePath.readDir dir).toList;
  let hashed ← handleEntries geet entries []
  makeObjFile .tree geet (entriesToContent hashed)
where
  handleEntries : FilePath → List IO.FS.DirEntry → List (Obj × String × String) → IO (List (Obj × String × String))
    | _, [], acc => pure acc
    | geet, e :: es, acc => do
      if List.elem e.fileName ignorableFiles
      then handleEntries geet es acc
      else
        let edir := e.path
        let isDir? ← edir.isDir.toIO
        if not isDir?
        then do
          let hash ← Hash.hashBlob (.ok edir) (geetDir := .some geet)
          handleEntries geet es ((.blob, e.fileName, hash) :: acc)
        else do
          let hash ← Hash.hashTree edir (geetDir := .some geet)
          handleEntries geet es ((.tree, e.fileName, hash) :: acc)

  entriesToContent : List (Obj × String × String) → String
    | [] => ""
    | [(obj, fn, hash)] => s!"{obj} {hash} {fn}"
    | (obj, fn, hash) :: es => s!"{obj} {hash} {fn}\n" ++ entriesToContent es

protected def catFile (obj : Obj) (hash_fn : String) : IO Unit := do
  let .some dir ← Init.geetDirExists?
    | Log.err "failed to find .geet directory"

  let dir := dir / Init.objDir / hash_fn
  let .true ← (FilePath.pathExists dir).toIO
    | Log.err s!"failed to find file {dir}."

  let (obj', fc) := strip (← IO.FS.readBinFile dir)
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

  let dir := objDir / treeHash
  let .true ← (FilePath.pathExists dir).toIO
    | Log.err s!"failed to find file {dir}."

  let (obj, fc) := strip (← IO.FS.readBinFile dir)
  if not (obj == .tree)
  then Log.err s!"expected a tree hash, but got a {obj} hash."
  else do
    let (blobs, trees) := List.partition (·.fst == .blob) ∘ parseFC $ String.fromUTF8! fc
    writeBlobs objDir curr (blobs.map (·.snd))

    for (_, hash, dirname) in trees do
      Hash.readTree (curr / dirname) hash (geetDir := .some geet)
where
  parseFC (fc : String) : List (Obj × String × String) :=
    (fc.splitOn (sep := "\n")).map
      (match String.splitOn · with
        | [obj, hash, fn] => (Obj.ofString obj, hash, fn)
        | _ => unreachable!)

  writeBlobs : FilePath → FilePath → List (String × String) → IO Unit
    | _, _, [] => pure ()
    | odir, curr, (hash, filename) :: blobs => do
      let (_, fc) := strip (← IO.FS.readBinFile $ odir / hash)
      IO.FS.writeBinFile (curr / filename) fc
      writeBlobs odir curr blobs

private def readHead (geet : FilePath) : IO (Option String) := do
  let fc ← IO.FS.readFile (geet / Init.headFile)
  if fc.isEmpty
  then pure .none
  else pure $ .some fc

protected def commitTree (dir : FilePath) (msg : String) : IO String := do
  let .some geet ← Init.geetDirExists?
    | Log.err "failed to find .geet directory."; pure ""

  let thash ← Hash.hashTree dir (geetDir := .some geet)
  let prev ← readHead geet
  let data :=
    match prev with
    | .none => s!"tree {thash}\n\n{msg}"
    | .some p => s!"tree {thash}\nparent {p}\n\n{msg}"
  let hash ← makeObjFile .commit geet data
  IO.FS.writeFile (geet / Init.headFile) hash
  pure hash

private partial def traceCommits (geet : FilePath) (commit : String) : IO Unit := do
  let contents@(_, phash, _) := parseCommit ∘ (·.snd) $ strip (← IO.FS.readBinFile (geet / Init.objDir / commit))
  IO.println $ compose commit contents
  match phash with
  | .none => pure ()
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

  compose (commit : String) (contents : String × Option String × String) : String :=
    s!"commit {commit}\n{contents.fst}\n\n  {contents.snd.snd}\n"

protected def logCommits (begin? : Option String := .none) : IO Unit := do
  let .some geet ← Init.geetDirExists?
    | Log.err "failed to find .geet directory."

  match begin? with
  | .none => do
    let .some commit ← readHead geet
      | Log.err "no commits have been made."
    traceCommits geet commit;
  | .some commit => traceCommits geet commit

end Hash
