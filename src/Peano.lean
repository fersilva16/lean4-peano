inductive ℕ where
  | zero : ℕ
  | succ : ℕ -> ℕ
  deriving Repr

def ℕ.fromDefaultNat : Nat -> ℕ
  | 0 => ℕ.zero
  | n + 1 => ℕ.succ (ℕ.fromDefaultNat n)

instance : OfNat ℕ n where
  ofNat := ℕ.fromDefaultNat n

def ℕ.toDefaultNat : ℕ -> Nat
  | ℕ.zero => 0
  | ℕ.succ n => 1 + ℕ.toDefaultNat n

instance : Repr ℕ where
  reprPrec n _ := repr (ℕ.toDefaultNat n)

def ℕ.sum : ℕ -> ℕ -> ℕ
  | ℕ.zero, y => y
  | ℕ.succ x', y => ℕ.sum x' (ℕ.succ y)

def ℕ.sub : ℕ -> ℕ -> ℕ
  | ℕ.succ x', ℕ.succ y' => ℕ.sub x' y'
  | x, _ => x

def ℕ.mult : ℕ -> ℕ -> ℕ
  | x, ℕ.succ y' => ℕ.sum x (ℕ.mult x y')
  | _, _ => ℕ.zero

def ℕ.div : ℕ -> ℕ -> ℕ
  | x, ℕ.succ y' => div' x y' 0 y'
  | _, ℕ.zero => ℕ.zero
  where
    div' : ℕ -> ℕ -> ℕ -> ℕ -> ℕ
      | ℕ.succ x', y, q, ℕ.zero => div' x' y (ℕ.succ q) y
      | ℕ.succ x', y, q, ℕ.succ r' => div' x' y q r'
      | ℕ.zero, _, q, _ => q

def ℕ.gt : ℕ -> ℕ -> Bool
  | ℕ.succ _, ℕ.zero => true 
  | ℕ.succ x', ℕ.succ y' => ℕ.gt x' y'
  | _, _ => false

def ℕ.gte : ℕ -> ℕ -> Bool
  | ℕ.zero, ℕ.succ _ => false
  | ℕ.succ x', ℕ.succ y' => ℕ.gte x' y'
  | _, _ => true

def ℕ.lt (x y : ℕ) : Bool := not (ℕ.gte x y)

def ℕ.lte (x y : ℕ) : Bool := not (ℕ.gt x y)

def ℕ.eq : ℕ -> ℕ -> Bool
  | ℕ.zero, ℕ.zero => true
  | ℕ.succ x', ℕ.succ y' => ℕ.eq x' y'
  | _, _ => false

infixl:60 " +' " => ℕ.sum
infixl:60 " -' " => ℕ.sub
infixl:70 " *' " => ℕ.mult
infixl:70 " /' " => ℕ.div
infix:50 " >' " => ℕ.gt
infix:50 " >=' " => ℕ.gte
infix:50 " <' " => ℕ.lt
infix:50 " <=' " => ℕ.lte
infix:50 " ==' " => ℕ.eq

#eval ℕ.succ ℕ.zero +' ℕ.succ ℕ.zero
#eval ℕ.succ ℕ.zero +' ℕ.succ ℕ.zero -' ℕ.succ ℕ.zero
#eval ℕ.succ (ℕ.succ ℕ.zero) -' ℕ.succ ℕ.zero
#eval ℕ.succ (ℕ.succ ℕ.zero) *' ℕ.succ (ℕ.succ ℕ.zero)
#eval ℕ.succ (ℕ.succ ℕ.zero) /' ℕ.succ ℕ.zero
#eval ℕ.succ (ℕ.succ ℕ.zero) >' ℕ.succ ℕ.zero
#eval ℕ.succ (ℕ.succ ℕ.zero) >=' ℕ.succ (ℕ.succ ℕ.zero)
#eval ℕ.succ ℕ.zero <' ℕ.succ (ℕ.succ ℕ.zero)
#eval ℕ.succ (ℕ.succ ℕ.zero) <=' ℕ.succ (ℕ.succ ℕ.zero)
#eval ℕ.succ (ℕ.succ ℕ.zero) ==' ℕ.succ (ℕ.succ ℕ.zero)
#eval ℕ.succ (ℕ.succ ℕ.zero) ==' 2
