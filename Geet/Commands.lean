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
      let exists? ← (FilePath.isDir ∘ flip Init.pushDir geetDir $ fp).toIO
      if exists?
      then pure $ .some fp
      else (
        if let .some parent := fp.parent
        then aux n parent
        else pure .none
      )

protected def addObjDir (fp : FilePath) : IO Unit := do
  let exists? ← (FilePath.isDir ∘ flip Init.pushDir objDir $ fp).toIO
  unless exists? do
    Log.info "created .objects directory."; Init.addDir fp objDir

protected def addGeetDir : IO FilePath := do
  let exists? ← Init.geetDirExists?;
  if let .some parent := exists?
  then Log.err ".geet directory already exists."; pure parent
  else do
    let curr ← IO.currentDir;
    Init.addDir curr geetDir;
    Log.info "created .geet directory"; pure curr

protected def init : IO Unit := do
  let geetAt ← Init.addGeetDir;
  Init.addObjDir geetAt

end Init

namespace Hash

open System

private def hashStr : String → String :=
  BitVec.toHex ∘ UInt64.toBitVec ∘ String.hash

protected def makeObj (fname : String) : IO Unit := do
  let .some dir ← Init.geetDirExists?
    | Log.err "failed to find .geet directory"

  let fc ← IO.FS.readFile s!"{dir}/{fname}";
  let obj_fn := hashStr fc;
  IO.FS.writeFile s!"{dir}/{Init.objDir}/{obj_fn}" fc
  IO.println obj_fn

protected def catObj (hash_fn : String) : IO Unit := do
  let .some dir ← Init.geetDirExists?
    | Log.err "failed to find .geet directory"

  let dir := s!"{Init.pushDir dir Init.objDir}/{hash_fn}";
  let .true ← (FilePath.pathExists dir).toIO
    | Log.err "failed to find file."

  IO.println (← IO.FS.readFile dir)

end Hash
