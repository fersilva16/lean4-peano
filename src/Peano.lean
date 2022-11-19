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

-- ℕ.succ (ℕ.succ (ℕ.succ ℕ.zero)) == 3
-- ℕ.succ (ℕ.succ (ℕ.succ (ℕ.succ ℕ.zero))) == 4
-- ℕ.succ (ℕ.succ (ℕ.succ (ℕ.succ (ℕ.succ ℕ.zero)))) == 5

def ℕ.sum : ℕ -> ℕ -> ℕ
  | ℕ.zero, y => y
  | ℕ.succ x', y => ℕ.sum x' (ℕ.succ y)

#eval ℕ.sum 5 12 -- 17
#eval ℕ.sum 19 4 -- 23

def ℕ.sub : ℕ -> ℕ -> ℕ
  | ℕ.succ x', ℕ.succ y' => ℕ.sub x' y'
  | x, _ => x

#eval ℕ.sub 8 2 -- 6
#eval ℕ.sub 9 5 -- 4

def ℕ.mult : ℕ -> ℕ -> ℕ
  | x, ℕ.succ y' => ℕ.sum x (ℕ.mult x y')
  | _, _ => ℕ.zero

#eval ℕ.mult 9 3 -- 27
#eval ℕ.mult 6 4 -- 18

def ℕ.div : ℕ -> ℕ -> ℕ
  | x, ℕ.succ y' => div' x y' 0 y'
  | _, ℕ.zero => ℕ.zero
  where
    div' : ℕ -> ℕ -> ℕ -> ℕ -> ℕ
      | ℕ.succ x', y, q, ℕ.zero => div' x' y (ℕ.succ q) y
      | ℕ.succ x', y, q, ℕ.succ r' => div' x' y q r'
      | ℕ.zero, _, q, _ => q

#eval ℕ.div 21 7 -- 3
#eval ℕ.div 30 3 -- 10

def ℕ.eq : ℕ -> ℕ -> Bool
  | ℕ.zero, ℕ.zero => true
  | ℕ.succ x', ℕ.succ y' => ℕ.eq x' y'
  | _, _ => false

#eval ℕ.eq 7 7 -- true
#eval ℕ.eq 1 4 -- false

def ℕ.gt : ℕ -> ℕ -> Bool
  | ℕ.succ _, ℕ.zero => true 
  | ℕ.succ x', ℕ.succ y' => ℕ.gt x' y'
  | _, _ => false

#eval ℕ.gt 2 1 -- true
#eval ℕ.gt 5 5 -- false

def ℕ.lt : ℕ -> ℕ -> Bool
  | ℕ.zero, ℕ.succ _ => true
  | ℕ.succ x', ℕ.succ y' => ℕ.lt x' y'
  | _, _ => false

#eval ℕ.lt 4 6 -- true
#eval ℕ.lt 1 1 -- false

def ℕ.gte (x y : ℕ) : Bool := not (ℕ.lt x y)

#eval ℕ.gte 2 2 -- true
#eval ℕ.gte 5 7 -- false

def ℕ.lte (x y : ℕ) : Bool := not (ℕ.gt x y)

#eval ℕ.lte 1 1 -- true
#eval ℕ.lte 5 0 -- false

infixl:60 " +' " => ℕ.sum
infixl:60 " -' " => ℕ.sub
infixl:70 " *' " => ℕ.mult
infixl:70 " /' " => ℕ.div
infix:50 " >' " => ℕ.gt
infix:50 " >=' " => ℕ.gte
infix:50 " <' " => ℕ.lt
infix:50 " <=' " => ℕ.lte
infix:50 " ==' " => ℕ.eq

#eval 1 +' 1       -- 2
#eval 2 -' 1       -- 1
#eval 2 *' 2       -- 4
#eval 6 /' 2       -- 3
#eval 2 >' 1       -- true
#eval 2 >=' 2      -- true
#eval 1 <' 2       -- true
#eval 2 <=' 2      -- true
#eval 2 ==' 2      -- true
#eval 2 +' 4 -' 1  -- 5
#eval 10 *' 3 /' 2 -- 15
#eval 1 +' 2 ==' 3 -- true
