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

def gt : ℕ -> ℕ -> Bool
  | ℕ.succ _, ℕ.zero => true 
  | ℕ.succ x', ℕ.succ y' => gt x' y'
  | _, _ => false

def gte : ℕ -> ℕ -> Bool
  | ℕ.zero, ℕ.succ _ => false
  | ℕ.succ x', ℕ.succ y' => gte x' y'
  | _, _ => true

def lt (x y : ℕ) : Bool := not (gte x y)

def lte (x y : ℕ) : Bool := not (gt x y)

def eq : ℕ -> ℕ -> Bool
  | ℕ.zero, ℕ.zero => true
  | ℕ.succ x', ℕ.succ y' => eq x' y'
  | _, _ => false
