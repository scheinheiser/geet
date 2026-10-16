open Lean

namespace Log

inductive Level where
  | info : String → Level
  | warn : String → Level
  | err  : String → Level

def log : Level → IO Unit
  | .info msg => IO.println $ fmt "\x1b[1;94m[INFO]\x1b[0m" msg
  | .warn msg => IO.println $ fmt "\x1b[1;33m[WARN]\x1b[0m" msg
  | .err msg  => IO.println $ fmt "\x1b[1;91m[ERROR]\x1b[0m" msg
where
  fmt (lvl : String) (msg : String) : String :=
    s!"{lvl}: {msg}"

protected def info : String → IO Unit := log ∘ .info
protected def warn : String → IO Unit := log ∘ .warn
protected def err : String → IO Unit := log ∘ .err

end Log
