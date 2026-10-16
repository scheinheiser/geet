import Geet.Log
open Lean

namespace Init
def geetDir : String := ".geet"
def objDir : String := ".geet/objects"
def maxDepth : Nat := 10

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
  let exists? ← (FilePath.isDir ∘ flip Init.pushDir objDir $ fp).toIO
  unless exists? do
    Log.info "created .geet/objects directory."; Init.addDir fp objDir

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

private inductive Obj where
  | blob
  deriving BEq

private def Obj.toBytes : Obj → ByteArray
  | .blob => "blob".toByteArray

private def Obj.byteLength : Obj → Nat
  | .blob => 4

private def strip (o : Obj) (b : ByteArray) : ByteArray :=
  .mk (b.data.drop $ Obj.byteLength o)

private def hashStr : String → String :=
  BitVec.toHex ∘ UInt64.toBitVec ∘ String.hash

protected def makeObj (fname : String) : IO Unit := do
  let .some dir ← Init.geetDirExists?
    | Log.err "failed to find .geet directory"

  let fc ← IO.FS.readFile s!"{dir}/{fname}";
  let obj_fn := hashStr fc;
  let fc := Obj.blob.toBytes ++ fc.toByteArray;
  IO.FS.writeBinFile s!"{dir}/{Init.objDir}/{obj_fn}" fc
  IO.println obj_fn

protected def catObj (hash_fn : String) : IO Unit := do
  let .some dir ← Init.geetDirExists?
    | Log.err "failed to find .geet directory"

  let dir := s!"{Init.pushDir dir Init.objDir}/{hash_fn}";
  let .true ← (FilePath.pathExists dir).toIO
    | Log.err "failed to find file."

  let fc ← IO.FS.readBinFile dir;
  IO.println ∘ String.fromUTF8! $ strip .blob fc

end Hash
