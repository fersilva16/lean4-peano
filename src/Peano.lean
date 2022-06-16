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

def sum : ℕ -> ℕ -> ℕ
  | ℕ.zero, y => y
  | ℕ.succ x', y => sum x' (ℕ.succ y)

def sub : ℕ -> ℕ -> ℕ
  | ℕ.succ x', ℕ.succ y' => sub x' y'
  | x, _ => x

def mult : ℕ -> ℕ -> ℕ
  | x, ℕ.succ y' => sum x (mult x y')
  | _, _ => ℕ.zero
