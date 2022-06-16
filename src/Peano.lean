inductive ℕ where
  | zero : ℕ
  | succ : ℕ -> ℕ
  deriving Repr

def fromDefaultNat : Nat -> ℕ
  | 0 => ℕ.zero
  | n + 1 => ℕ.succ (fromDefaultNat n)

instance : OfNat ℕ n where
  ofNat := fromDefaultNat n

def toDefaultNat : ℕ -> Nat
  | ℕ.zero => 0
  | ℕ.succ n => 1 + toDefaultNat n

instance : Repr ℕ where
  reprPrec n _ := repr (toDefaultNat n)

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
