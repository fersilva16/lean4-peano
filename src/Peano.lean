inductive ℕ where
  | zero : ℕ
  | succ : ℕ -> ℕ
  deriving Repr

def sum (x y : ℕ) : ℕ :=
  match x with
    | ℕ.zero => y
    | ℕ.succ x' => sum x' (ℕ.succ y)
