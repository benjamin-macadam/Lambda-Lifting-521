module AST where

---------------------------------------------------------------------------
------ The data type for programs to lambda lift:-------------------------
---------------------------------------
---------------
------ Programs are lists of function declarations (the first in the list is-- the main program.
-------------------------------------------------------------------------------
--
data Prog a b = Prog [Fun a b]

data Fun a b = Fun (a, [b], Exp a b)


data BExp a b   = Lt (Exp a b) (Exp a b)
                | Gt (Exp a b) (Exp a b)
                | Eq (Exp a b) (Exp a b)
                | AND (BExp a b) (BExp a b)
                | OR (BExp a b) (BExp a b)
                | NOT (BExp a b)

data Exp a b = ADD (Exp a b) (Exp a b)
             | SUB (Exp a b) (Exp a b) 
             | MUL (Exp a b) (Exp a b) 
             | DIV (Exp a b) (Exp a b) 
             | NEG (Exp a b)
             | CONST Int
             | Var b
             | COND (BExp a b) (Exp a b) (Exp a b)
             | APP a [(Exp a b)] 
             | LET [Fun a b] (Exp a b)


-- Pretty Printer
class Printer a where
    printer:: a -> String

instance Printer a => Printer [a] where
    printer [] = []
    printer (a:as) = (printer a) ++ (printer as)

instance Printer Int where
    printer n = "v"++ show(n::Int)

instance Printer Char where
    printer c = [c]

-- to print n spaces
spaces 0 = ""
spaces n = " " ++ (spaces (n-1))

show_prog :: (Printer a, Printer b) => Prog a b -> String
show_prog (Prog funs) = (concat (map (\f -> (show_fun 0 f) ++ "\n") funs))

show_fun :: (Printer a, Printer b) => Int -> (Fun a b) -> String
show_fun n (Fun (fname,a1:args,body)) = (spaces n) ++ "fun "++(printer fname)
        ++ "("++(printer a1)++(concat(map (\a ->","++(printer a)) args))
        ++") = "++ (show_exp’ n body)
show_exp:: (Printer a, Printer b) => Int -> (Exp a b) -> String
show_exp n exp = (spaces n)++(show_exp’ n exp)

show_exp’:: (Printer a, Printer b) => Int -> (Exp a b) -> String
show_exp’ n (ADD e1 e2) = (show_exp’ n e1)++"+"++(show_exp’ n e2)
show_exp’ n (MUL e1 e2) = (show_exp’ n e1)++"*"++(show_exp’ n e2)
show_exp’ n (DIV e1 e2) = (show_exp’ n e1)++"/"++(show_exp’ n e2)
show_exp’ n (SUB e1 e2) = (show_exp’ n e1)++"-"++(show_exp’ n e2)
show_exp’ n (NEG e) = "-"++(show_exp’ n e)
show_exp’ n (CONST m) = show m
show_exp’ n (VAR b) = printer b
show_exp’ n (COND b e1 e2) = "(if "++(show_bexp n b)++"\\n"
                    ++(spaces (n+3))++"then "++(show_exp’ (n+3) e1)++"\n"
                    ++(spaces (n+3))++"else "++(show_exp’ (n+3) e1)++")"
show_exp’ n (APP f (e:es)) = (printer f)++"("++(show_exp’ n e)
                ++(concat(map (\x-> ","++(show_exp’ n x)) es))++")"
show_exp’ n (LET [] e) = (show_exp’ n e)
show_exp’ n (LET (f:fs) e) = "let\n"++(show_fun (n+3) f)
    ++(concat (map (\f -> "\n"++(show_fun (n+3) f)) fs))
    ++"\n"++(spaces n)++"in  "++(show_exp’ n e)

show_bexp:: (Printer a, Printer b) => Int -> (BExp a b) -> String
show_bexp n (Lt e1 e2) = (show_exp’ n e1)++"<"++(show_exp’ n e2)
show_bexp n (Gt e1 e2) = (show_exp’ n e1)++">"++(show_exp’ n e2)
show_bexp n (Eq e1 e2) = (show_exp’ n e1)++"=="++(show_exp’ n e2)
show_bexp n (AND e1 e2) = (show_bexp n e1)++"&&"++(show_bexp n e2)
show_bexp n (OR e1 e2) = (show_bexp n e1)++"||"++(show_bexp n e2)
show_bexp n (NOT e) = "not("++(show_bexp n e)++")"


