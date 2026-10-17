import Geet.Log
open Lean

namespace Init
def geetDir : String := ".geet"
def objDir : String := ".geet/objects"
def maxDepth : Nat := 10 -- TODO: make this customisable from some .env file

open System

protected def pushDir (fp : FilePath) (d : String) : System.FilePath :=
  s!"{fp.toString}/{d}"

protected def addDir (fp : FilePath) : String → IO Unit :=
  IO.FS.createDir ∘ Init.pushDir fp

protected def geetDirExists? : IO (Option FilePath) := do
  let curr ← IO.currentDir;
  aux maxDepth curr
where
  aux : Nat → FilePath → IO (Option FilePath)
    | 0, _ => pure .none
    | n + 1, fp => do
      let .false ← (FilePath.isDir ∘ flip Init.pushDir geetDir $ fp).toIO
        | pure $ .some fp

      if let .some parent := fp.parent
      then aux n parent
      else pure .none

protected def addObjDir (fp : FilePath) : IO Unit := do
  let .false ← (FilePath.isDir ∘ flip Init.pushDir objDir $ fp).toIO
    | pure ()

  Init.addDir fp objDir;
  Log.info "created .geet/objects directory."

protected def addGeetDir : IO FilePath := do
  let exists? ← Init.geetDirExists?;
  if let .some parent := exists?
  then Log.err ".geet directory already exists."; pure parent
  else do
    let curr ← IO.currentDir;
    Init.addDir curr geetDir;
    Log.info s!"initialised .geet directory in {curr}."; pure curr

protected def init : IO Unit := do
  let geetAt ← Init.addGeetDir;
  Init.addObjDir geetAt

end Init

namespace Hash

open System

private def ignorableFiles := [".geet", ".git", ".lake", ".DS_Store", "thumbs.db", "desktop.ini"]

inductive Obj where
  | blob
  | tree
  | any -- we don't care what the obj type is here
  deriving Inhabited

instance : BEq Obj where
  beq : Obj → Obj → Bool
    | .blob, .blob => true
    | .tree, .tree => true
    | .any, _ | _, .any => true
    | _, _ => false

instance : ToString Obj where
  toString : Obj → String
    | .blob => "blob"
    | .tree => "tree"
    | .any  => "any"

protected def Obj.ofString : String → Obj
  | "blob" => .blob
  | "tree" => .tree
  | _ => .any

private def Obj.toBytes : Obj → ByteArray
  | .blob => "blob\x00".toByteArray
  | .tree => "tree\x00".toByteArray
  | .any => .empty

private def Obj.byteLength : Obj → Nat
  | .blob => 5 -- we ensure to account for the null byte
  | .tree => 5
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
  IO.FS.writeBinFile s!"{geetDir}/{Init.objDir}/{obj_fn}" fc
  pure obj_fn

-- TODO: make fname and fp an either dataclass
protected def hashBlob (fname : String) (fp : Option FilePath := .none): IO String := do
  let .some dir ← Init.geetDirExists?
    | Log.err "failed to find .geet directory."; pure ""

  let curr ← IO.currentDir;
  let fp :=
    match fp with
    | .none => s!"{curr}/{fname}"
    | .some fp => fp.toString

  let fc ← IO.FS.readFile fp;
  makeObjFile .blob dir fc

protected partial def hashTree (dir : FilePath) : IO String := do
  let .some geet ← Init.geetDirExists?
    | Log.err "failed to find .geet directory"; pure ""

  let entries := (← FilePath.readDir dir).toList;
  let hashed ← handleEntries entries []
  makeObjFile .tree geet (entriesToContent hashed)
where
  handleEntries : List IO.FS.DirEntry → List (Obj × String × String) → IO (List (Obj × String × String))
    | [], acc => pure acc
    | e :: es, acc => do
      if List.elem e.fileName ignorableFiles
      then handleEntries es acc
      else
        let edir := e.path
        let isDir? ← edir.isDir.toIO
        if not isDir?
        then do
          let hash ← Hash.hashBlob "" (fp := .some edir)
          handleEntries es ((.blob, e.fileName, hash) :: acc)
        else do
          let hash ← Hash.hashTree edir
          handleEntries es ((.tree, e.fileName, hash) :: acc)

  entriesToContent : List (Obj × String × String) → String
    | [] => ""
    | [(obj, fn, hash)] => s!"{obj} {hash} {fn}"
    | (obj, fn, hash) :: es => s!"{obj} {hash} {fn}\n" ++ entriesToContent es

protected def catFile (obj : Obj) (hash_fn : String) : IO Unit := do
  let .some dir ← Init.geetDirExists?
    | Log.err "failed to find .geet directory"

  let dir := s!"{Init.pushDir dir Init.objDir}/{hash_fn}";
  let .true ← (FilePath.pathExists dir).toIO
    | Log.err s!"failed to find file {dir}."

  let (obj', fc) := strip (← IO.FS.readBinFile dir)
  if obj == obj'
  then IO.println $ String.fromUTF8! fc
  else Log.err "expected a(n) {obj} file, but got a(n) {obj'} file."

protected partial def readTree (curr : FilePath) (treeHash : String) : IO Unit := do
  let .some geet ← Init.geetDirExists?
    | Log.err "failed to find .geet directory."
  let obj_dir := Init.pushDir geet Init.objDir

  let dir := s!"{obj_dir}/{treeHash}"
  let .true ← (FilePath.pathExists dir).toIO
    | Log.err s!"failed to find file {dir}."

  let (obj, fc) := strip (← IO.FS.readBinFile dir)
  unless obj == .tree do Log.err s!"expected a tree hash, but got a {obj} hash."

  let (blobs, trees) := List.partition (·.fst == .blob) ∘ parseFC $ String.fromUTF8! fc
  writeBlobs obj_dir curr (blobs.map (·.snd))

  for (_, hash, dirname) in trees do
    Hash.readTree s!"{curr}/{dirname}" hash
where
  parseFC (fc : String) : List (Obj × String × String) :=
    (fc.splitOn (sep := "\n")).map
      (match String.splitOn · with
        | [obj, hash, fn] => (Obj.ofString obj, hash, fn)
        | _ => unreachable!)

  writeBlobs : FilePath → FilePath → List (String × String) → IO Unit
    | _, _, [] => pure ()
    | odir, curr, (hash, filename) :: blobs => do
      let (_, fc) := strip (← IO.FS.readBinFile s!"{odir}/{hash}")
      IO.FS.writeBinFile s!"{curr}/{filename}" fc;
      writeBlobs odir curr blobs

end Hash
