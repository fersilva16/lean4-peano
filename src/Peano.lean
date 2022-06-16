inductive ℕ where
  | zero : ℕ
  | succ : ℕ -> ℕ
  deriving Repr

def sum (x y : ℕ) : ℕ :=
  match x with
    | ℕ.zero => y
    | ℕ.succ x' => sum x' (ℕ.succ y)

def sub (x y : ℕ) : ℕ :=
  match x, y with
    | ℕ.succ x', ℕ.succ y' => sub x' y' 
    | _, _ => x

def mult (x y : ℕ) : ℕ :=
  match x, y with
    | _, ℕ.succ y' => sum x (mult x y')
    | _, _  => ℕ.zero
