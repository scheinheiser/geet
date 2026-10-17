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

inductive Obj where
  | blob
  | any -- we don't care what the obj type is here

instance : BEq Obj where
  beq : Obj → Obj → Bool
    | .blob, .blob => true
    | .any, _ | _, .any => true

instance : ToString Obj where
  toString : Obj → String
    | .blob => "blob"
    | .any => "any"

private def Obj.ofString : String → Obj
  | "blob" => .blob
  | _ => .any

private def Obj.toBytes : Obj → ByteArray
  | .blob => "blob\x00".toByteArray
  | .any => .empty

private def Obj.byteLength : Obj → Nat
  | .blob => 5 -- we ensure to account for the null byte
  | .any => 0

-- strip the object prefix from an object file.
private def strip (b : ByteArray) : Obj × ByteArray :=
  let obj := Obj.ofString ∘ String.fromUTF8! ∘ List.toByteArray $ b.toList.takeWhile (· != 0)
  (obj, .mk (b.data.drop $ Obj.byteLength obj))

private def hashStr : String → String :=
  BitVec.toHex ∘ UInt64.toBitVec ∘ String.hash

protected def makeObjFile (obj : Obj) (fname : String) : IO Unit := do
  let .some dir ← Init.geetDirExists?
    | Log.err "failed to find .geet directory"

  let fc ← IO.FS.readFile s!"{dir}/{fname}";
  let obj_fn := hashStr fc;
  let fc := obj.toBytes ++ fc.toByteArray;
  IO.FS.writeBinFile s!"{dir}/{Init.objDir}/{obj_fn}" fc
  IO.println obj_fn

protected def catFile (obj : Obj) (hash_fn : String) : IO Unit := do
    let .some dir ← Init.geetDirExists?
      | Log.err "failed to find .geet directory"

    let dir := s!"{Init.pushDir dir Init.objDir}/{hash_fn}";
    let .true ← (FilePath.pathExists dir).toIO
      | Log.err "failed to find file."

    let (obj', fc) := strip (← IO.FS.readBinFile dir)
    if obj == obj'
    then IO.println $ String.fromUTF8! fc
    else Log.err "expected a(n) {obj} file, but got a(n) {obj'} file."

end Hash
