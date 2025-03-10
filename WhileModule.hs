module WhileModule where

import Data.Char (isAlphaNum, isNumber, isAlpha, digitToInt)



{- TYPES -}
type Var = String
data AExp = VarA Var | LitA Integer | Plus AExp AExp | Minus AExp AExp -- deriving Show
data BExp = VarB Var | LitB Bool | Neg BExp | And BExp BExp | Or BExp BExp | Bigger AExp AExp | Smaller AExp AExp | Equals AExp AExp --deriving Show

data Exp = A AExp | B BExp | Var Var -- deriving Show -- One could also use this to avoid two ASSs, but meh

helperExp :: Exp -> (Either AExp BExp -> Exp ) -> Exp
helperExp (A exp) f = f (Left exp) 
helperExp (B exp) f = f (Right exp) 

data While = ASS Var Exp | SKIP | NEW While While | IF BExp While While | WHILE BExp While -- deriving Show

{- WHILE OPERATORS -}
-- Arithmetic OPs
(=+=) = Plus
(=-=) = Minus

-- Boolean OPs
(!!!) = Neg
(&&&) = And
(|||) = Or
(>>>) = Bigger
(<<<) = Smaller
(===) = Equals
---- Boolean Helpers
(>>==) a b = (!!!) (a <<< b)
(<<==) a b = (!!!) (a >>> b)
(!==) a b = (!!!) (a === b)
-- While language helpers
infixr 1 #
(#) :: While -> While -> While
(#) = NEW
(<~) :: Var -> Exp -> While
(<~) x a = ASS x a
(??) :: BExp -> While -> (While -> While)
(??) = IF
(?!) :: (While -> While) -> While -> While
(?!) = ($)

group :: Show a => a -> String -> a -> String
group l op r = "(" ++ show l ++ " " ++ op ++ " " ++ show r ++ ")"


{- SHOW -}
-- removes the single quotes around a char
showVar :: Var -> String
showVar = init.(drop 1).show 
showArray :: Show a => [a] -> String
showArray [] = ""
showArray a = init $ showArray2 a where
  showArray2 :: Show a => [a] -> String
  showArray2 [] = ""
  showArray2 (elem:elems) = show elem ++ " " ++ showArray2 elems

-- {-
instance Show AExp where
  show :: AExp -> String
  show (VarA a) = showVar a
  show (LitA n) = show n
  show (Plus l r) = group l "+" r
  show (Minus (LitA 0) (LitA r)) = "-" ++ show r
  show (Minus (LitA 0) (VarA r)) = "-" ++ showVar r 
  show (Minus (LitA 0) r) = "-(" ++ show r ++ ")"
  show (Minus l r) = group l "-" r

instance Show BExp where
  show :: BExp -> String
  show (VarB b)      = showVar b
  show (LitB b)      = if b then "true" else "false"
  show (Neg b)       = "!" ++ show b
  show (And l r)     = group l "&" r
  show (Or l r)      = group l "|" r
  show (Bigger l r)  = group l ">" r
  show (Smaller l r) = group l "<" r
  show (Equals l r)  = group l "=" r
instance Show Exp where
  show (Var name             ) = "" ++ showVar name
  show (A   (VarA arithmetic)) = "" ++ showVar arithmetic
  show (A   arithmetic       ) = "" ++ show arithmetic
  show (B   (VarB boolean)   ) = "" ++ showVar boolean
  show (B   boolean          ) = "" ++ show boolean


instance Show While where
  show :: While -> String
  show w = showIndentedProgram w 0

showIndentedProgram :: While -> Int -> String
showIndentedProgram w indent = showIndent indent ++ showProgram w where
  showIndent :: Int -> String
  showIndent 0 = ""
  showIndent n = "\t" ++ showIndent (n-1)
  showProgram :: While -> String
  showProgram (ASS x a)    = showLine (showVar x ++ " := " ++ show a)
  showProgram (SKIP)         = showLine "skip"
  showProgram (NEW w1 w2)  = showProgram w1 ++ "\n" ++ showIndentedProgram w2 indent
  showProgram (IF b w1 w2) = "if " ++ show b ++ " then\n" ++ showIndentedProgram w1 (indent+1) ++ "\n" ++ showIndent indent ++ "else\n" ++ showIndentedProgram w2 (indent+1)
  showProgram (WHILE b w)  = "while " ++ show b ++ " do\n" ++ showIndentedProgram w (indent+1)
  showLine :: String -> String
  showLine s = s ++ ";" -- alternatively: "[" ++ s ++ "];"
 -- -}
 

{- SEMANTICS -}

type Literal = Either Integer Bool
type Assignment = (Var, Literal)
type AllocationList = [Assignment]

interpretBinaryTemplate l op r interpreter allocations = do
  n <- interpreter l allocations;
  m <- interpreter r allocations;
  return (op n m)

-- arithmetic semantics
interpretArithmetic :: AExp -> AllocationList -> Either String Integer
interpretArithmetic (VarA a) ass = interpretGeneralExpression (Var a) ass >>= \val -> 
  case val of 
    Right bool -> Left $ "Error: Tried to use a variable with a boolean " ++ showVar a ++ " in a context where a number is needed. (" ++ showVar a ++ " is " ++ show bool ++ ")"
    Left  int  -> Right int
interpretArithmetic (LitA a) _ = Right a
interpretArithmetic (Plus l r) ass = interpretBinaryTemplate l (+) r interpretArithmetic ass
interpretArithmetic (Minus l r) ass = interpretBinaryTemplate l (-) r interpretArithmetic ass

-- boolean semantics
interpretBoolean :: BExp -> AllocationList -> Either String Bool
interpretBoolean (VarB b) ass = interpretGeneralExpression (Var b) ass >>= \val -> 
  case val of 
    Left  int  -> Left $ "Error: Tried to use a variable with a number " ++ showVar b ++ " in a context where a boolean is needed. (" ++ showVar b ++ " is " ++ show int ++ ")"
    Right bool -> Right bool
interpretBoolean (LitB    b  ) _   = Right b
interpretBoolean (Neg     b  ) ass = interpretBoolean b ass >>= return.not
interpretBoolean (And     l r) ass = interpretBinaryTemplate l (&&) r interpretBoolean ass
interpretBoolean (Or      l r) ass = interpretBinaryTemplate l (||) r interpretBoolean ass
interpretBoolean (Bigger  l r) ass = interpretBinaryTemplate l (>) r interpretArithmetic  ass
interpretBoolean (Smaller l r) ass = interpretBinaryTemplate l (<) r interpretArithmetic ass
interpretBoolean (Equals  l r) ass = interpretBinaryTemplate l (==) r interpretArithmetic ass

interpretGeneralExpression :: Exp -> AllocationList -> Either String (Either Integer Bool)
interpretGeneralExpression      (Var name) []                    = Left $ "Error: Uninitialized variable " ++ name ++ " was used in an expression" 
interpretGeneralExpression expr@(Var var1) ((var2, Left  a):ass) = if var1 == var2 then Right (Left  a) else interpretGeneralExpression expr ass
interpretGeneralExpression expr@(Var var1) ((var2, Right b):ass) = if var1 == var2 then Right (Right b) else interpretGeneralExpression expr ass

interpretGeneralExpression (A (VarA a)) ass = interpretGeneralExpression (Var a) ass
interpretGeneralExpression (B (VarB b)) ass = interpretGeneralExpression (Var b) ass 
interpretGeneralExpression (A a)        ass = interpretArithmetic a ass >>= return.Left
interpretGeneralExpression (B b)        ass = interpretBoolean b ass >>= Right . Right

-- while language semantics
---- structured operational semantics
allocate :: AllocationList -> Assignment -> AllocationList -- If a variable already exists, but a variable with a different type is being allocated, nothing happens 
allocate (oldAssign@(var1, Left n1):allocations) newAssign@(var2, Left n2) =  if var1 == var2 then newAssign:allocations else oldAssign:allocate allocations newAssign
allocate (oldAssign@(var1, Right n1):allocations) newAssign@(var2, Right n2) =  if var1 == var2 then newAssign:allocations else oldAssign:allocate allocations newAssign
allocate (oldAssign@(var1, Left n1):allocations) newAssign@(var2, Right n2) =  if var1 == var2 then newAssign:allocations else oldAssign:allocate allocations newAssign
allocate (oldAssign@(var1, Right n1):allocations) newAssign@(var2, Left n2) =  if var1 == var2 then newAssign:allocations else oldAssign:allocate allocations newAssign
allocate [] newAssignment = [newAssignment]


sos :: While -> Either String AllocationList -- return assignments at the end, or Nothing if a program cannot be interpreted
sos = sosrepl []
sosrepl :: AllocationList -> While -> Either String AllocationList
sosrepl ass w = rules w ass where
rules :: While -> AllocationList -> Either String AllocationList
rules (ASS var exp) ass = let value = interpretGeneralExpression exp ass in case value of --[ASS]
  Left err -> Left err
  Right a -> return $ allocate ass (var, a)
rules SKIP ass = Right ass -- [SKIP]
rules (IF exp w1 w2) ass = case interpretBoolean exp ass of
  Left err -> Left err
  Right True -> rules w1 ass -- [IF_T]
  Right False -> rules w2 ass -- [IF_F]
rules s@(WHILE exp w2) ass = case interpretBoolean exp ass of
  Left err -> Left err
  Right True -> rules (NEW w2 s) ass
  Right False -> Right ass
rules (NEW w1 w2) ass = case w1 of
  (ASS x a) -> rules w1 ass >>= rules w2 -- [COMP2 + ASS]
  SKIP -> rules w2 ass -- [COMP2 + SKIP]
  (NEW w3 w4) -> (rules w3 ass >>= rules w4) >>= rules w2 -- Interpretation of previous Subprograms
  (IF b onTrue onFalse) -> case interpretBoolean b ass of
    Left err -> Left err
    Right True -> rules onTrue ass >>= rules w2
    Right False -> rules onFalse ass >>= rules w2
  (WHILE b loop) -> case interpretBoolean b ass of
    Left err -> Left err
    Right True -> rules loop ass >>= rules w1 >>= rules w2
    Right False -> rules w2 ass

{- PARSER -}

data WhileToken = While | Do | If | Then | Else | Skip | Semicolon | AssignmentOp | Expression Exp | Indent | NewLine -- Varaibles are saved as in Expressions
data ExpToken = VarExp Var | LitExp (Either Integer Bool) | PlusOp | MinusOp | NegOp | AndOp | OrOp | BiggerRel | SmallerRel | EqualsRel | LeftBrackExp | RightBrackExp deriving (Eq)

instance Show WhileToken where
  show :: WhileToken -> String
  show While = "while "
  show Do = "do "
  show If = "if "
  show Then = " then"
  show Else = "else"
  show Skip = "skip"
  show Semicolon = ";"
  show AssignmentOp = " := "
  show (Expression e) = show e ++ ""
  show Indent = "[->]\t"
  show NewLine = "[new line]\n"

instance Show ExpToken where
  show (VarExp name)         = showVar name  
  show (LitExp (Left int))   = show (LitA int)  
  show (LitExp (Right bool)) = show (LitB bool)  
  show PlusOp                = "+"  
  show MinusOp               = "-"  
  show NegOp                 = "!"  
  show AndOp                 = "&"  
  show OrOp                  = "|"  
  show BiggerRel             = ">"  
  show SmallerRel            = "<"  
  show EqualsRel             = "="  
  show LeftBrackExp          = "("  
  show RightBrackExp          = ")"  

getText :: String -> (Int, String)
getText ""        = (0, "")
getText (' ':s1)  = (0, "")
getText ('\t':s1) = (0, "")
getText ('\n':s1) = (0, "") -- This case should never happen in a syntactically correct program, since every line needs to end with a semicolon
getText (';':s1)  = (0, "")
getText (c1:s1)   = if isAlphaNum c1 then let (len, text) = getText s1 in (len+1, c1:text) else (0, "")


lexerExpression :: String -> Either String [ExpToken]
lexerExpression "" = Right []
lexerExpression (c:s)
  | c == ' '   = lexerExpression s
  | c == '\t'  = lexerExpression s
  | c == '('   = lexerExpression s >>= return.((:) LeftBrackExp)
  | c == ')'   = lexerExpression s >>= return.((:) RightBrackExp)
  | c == '+'   = lexerExpression s >>= return.((:) PlusOp)
  | c == '-'   = lexerExpression s >>= return.((:) MinusOp)
  | c == '!'   = lexerExpression s >>= return.((:) NegOp)
  | c == '&'   = lexerExpression s >>= return.((:) AndOp)
  | c == '|'   = lexerExpression s >>= return.((:) OrOp)
  | c == '>'   = lexerExpression s >>= return.((:) BiggerRel)
  | c == '<'   = lexerExpression s >>= return.((:) SmallerRel)
  | c == '='   = lexerExpression s >>= return.((:) EqualsRel)
  | isAlpha c  = let (len, expr) = getAlphaExpToken (snd $ getText (c:s)) in lexerExpression (drop (len-1) s) >>= return.((:)expr)
  | isNumber c = let (len, expr) = getText (c:s) in lexerExpression (drop (len-1) s) >>= return.((:) (LitExp (Left $ ((read expr) :: Integer))))
  | otherwise = Left $ "Error: Found illegal character " ++ [c]
  where
    getAlphaExpToken :: String -> (Int, ExpToken)
    getAlphaExpToken "true" = (4, LitExp (Right True))
    getAlphaExpToken "false" = (5, LitExp (Right False))
    getAlphaExpToken s1 = let (len, expr) = getText s1 in (len, VarExp expr)
    

{-
Language using recursive descent:
Expression -> BooleanExpression

BooleanExpression -> Or 
Or -> And (\| And)*
And -> Neg (\& Neg)*
Neg -> \! BooleanPrimary | BooleanPrimary
BooleanPrimary ->  BooleanLiteral | Equals 
Equals -> Bigger (\= Bigger)*
Bigger -> Smaller (\> Smaller)*
Smaller -> ArithmeticExpression (\< ArithmeticExpression)*

ArithmeticExpression -> Plus
Plus -> Minus (\+ Minus)*
Minus -> Negative (\- Negative)*
Negative -> \- ArithmeticPrimary | ArithmeticPrimary
ArithmeticPrimary -> Variable | ArithmeticLiteral | \( BooleanExpression \)

-}
type Expparser a = [ExpToken] -> (Either String (Int, a))

parseExpression :: [ExpToken] -> Either String Exp
parseExpression expr = parseBooleanExpression expr >>= return.snd
  where
    -- Helpers
    errorEmptyExpression = Left $ "Error: Expected an Expression, but found none"
    isBinToken :: ExpToken -> Bool 
    isBinToken OrOp       = True
    isBinToken AndOp      = True
    isBinToken BiggerRel  = True
    isBinToken SmallerRel = True
    isBinToken EqualsRel  = True
    isBinToken PlusOp     = True
    isBinToken MinusOp    = True
    isBinToken _          = False
    binOpExpMapping :: ExpToken -> (Exp, Exp) -> Either String Exp
    binOpExpMapping token (e1,e2) = 
      let fixedExps = figureVariablesOut token e1 e2 in 
        binOpExpMappingHelper token fixedExps
      where
        figureVariablesOut :: ExpToken -> Exp -> Exp -> (Exp, Exp)
        -- I really should have made types for kinds of operators and relation...
        -- arith OP/REL arith
        figureVariablesOut PlusOp     exp1        exp2         = figureVariablesOut MinusOp    exp1         exp2 
        figureVariablesOut MinusOp    exp1        exp2         = figureVariablesOut SmallerRel exp1         exp2
        figureVariablesOut SmallerRel exp1        exp2         = figureVariablesOut BiggerRel  exp1         exp2
        figureVariablesOut BiggerRel  exp1        exp2         = figureVariablesOut EqualsRel  exp1         exp2
        figureVariablesOut EqualsRel (Var x)      exp2         = figureVariablesOut EqualsRel  (A (VarA x)) exp2
        figureVariablesOut EqualsRel (B (VarB x)) exp2         = figureVariablesOut EqualsRel  (A (VarA x)) exp2
        figureVariablesOut EqualsRel exp1         (Var x)      = figureVariablesOut EqualsRel  exp1         (A (VarA x))
        figureVariablesOut EqualsRel exp1         (B (VarB x)) = figureVariablesOut EqualsRel  exp1         (A (VarA x))
        figureVariablesOut EqualsRel exp1         exp2         = (exp1, exp2)
        -- bool OP bool
        figureVariablesOut AndOp exp1 exp2 = figureVariablesOut OrOp exp1 exp2   
        figureVariablesOut OrOp (Var x)      exp2         = figureVariablesOut OrOp (B (VarB x)) exp2
        figureVariablesOut OrOp (A (VarA x)) exp2         = figureVariablesOut OrOp (B (VarB x)) exp2
        figureVariablesOut OrOp exp1         (Var x)      = figureVariablesOut OrOp exp1         (B (VarB x))
        figureVariablesOut OrOp exp1         (A (VarA x)) = figureVariablesOut OrOp exp1         (B (VarB x))
        figureVariablesOut OrOp exp1         exp2         = (exp1, exp2)   
        binOpExpMappingHelper :: ExpToken -> (Exp, Exp) -> Either String Exp
        binOpExpMappingHelper OrOp       ((B l), (B r)) = Right $ B $ Or l r
        binOpExpMappingHelper AndOp      ((B l), (B r)) = Right $ B $ And l r
        binOpExpMappingHelper SmallerRel ((A l), (A r)) = Right $ B $ Smaller l r
        binOpExpMappingHelper BiggerRel  ((A l), (A r)) = Right $ B $ Bigger l r
        binOpExpMappingHelper EqualsRel  ((A l), (A r)) = Right $ B $ Equals l r
        binOpExpMappingHelper PlusOp     ((A l), (A r)) = Right $ A $ Plus l r   
        binOpExpMappingHelper MinusOp    ((A l), (A r)) = Right $ A $ Minus l r
        binOpExpMappingHelper a          (l, r) = Left ("Error: Used " ++ show a ++ " with " ++ show l ++ " and " ++ show r) 
    createBinaryOpParser :: (Expparser Exp) -> (Expparser (Maybe Exp)) -> ExpToken -> Expparser Exp
    createBinaryOpParser nextPrecedenceParseFunc thisLoopFunc opToken expTokens = 
      let 
        subExp1 = (nextPrecedenceParseFunc expTokens);
        subExp2 = subExp1 >>= (\(len, exp) -> thisLoopFunc (drop len expTokens)) in 
        case (subExp1, subExp2) of
          (Left err1          , Left err2          ) -> Left (err1)
          (Left err1          , _                  ) -> Left err1
          (_                  , Left err2          ) -> Left err2
          (Right (len1, expr1), Right (len2, Nothing)) -> Right (len1, expr1)
          (Right (len1, expr1), Right (len2, Just expr2)) -> binOpExpMapping opToken (expr1, expr2) >>= (\exp -> Right (len1 + 1 + len2, exp))
    createLoopParser :: ExpToken -> (Expparser Exp) -> Expparser (Maybe Exp)
    createLoopParser _      _         []            = Right (0,Nothing)
    createLoopParser opTok  _         [t]           = Left $ "Error: Tried to find an binary connective and right expression, but only found " ++ show (show t)
    createLoopParser opTok1 parseFunc (opTok2:toks) = if not (isBinToken opTok2) 
      then Left $ "Error: Wrong syntax in expression " ++ showArray (opTok2:toks) 
      else if opTok1 == opTok2 
        then parseFunc toks >>= (\(len, exp) -> return (len, Just exp)) 
        else Right (0,Nothing)


    parseBooleanExpression :: Expparser Exp
    parseBooleanExpression = parseOr
    
    parseOr :: Expparser Exp
    parseOr = createBinaryOpParser parseAnd parseOrLoop OrOp
    parseOrLoop :: Expparser (Maybe Exp)
    parseOrLoop = createLoopParser OrOp parseOr 
    
    parseAnd :: Expparser Exp
    parseAnd = createBinaryOpParser parseNeg parseAndLoop AndOp  
    parseAndLoop :: Expparser (Maybe Exp)
    parseAndLoop = createLoopParser AndOp parseAnd

    parseNeg :: Expparser Exp
    parseNeg []                = errorEmptyExpression
    parseNeg (NegOp:expTokens) = (parseBooleanPrimary expTokens) >>= (\(len, exp) -> case exp of B booleanExp -> Right (len+1, B (Neg booleanExp)); _ -> Left $ "Error: Expected a boolean expression in !(" ++ show exp ++ ")")
    parseNeg expTokens         = parseBooleanPrimary expTokens

    parseBooleanPrimary :: Expparser Exp
    parseBooleanPrimary []                                  = errorEmptyExpression
    parseBooleanPrimary [VarExp name]                       = return (1, B (VarB name))
    parseBooleanPrimary expTokens@(expToken1:expRestTokens) = case expToken1 of
      LitExp (Right b) -> Right (1, B (LitB b))
      (VarExp name)    -> let tryToGoLower = parseEquals expTokens in case tryToGoLower of 
        Right (len, A (VarA name2)) -> return (len, B (VarB name2)) -- Conversion from arithmetic Variable to boolean vaariable
        otherwise -> parseEquals expTokens
      _ -> parseEquals expTokens

    parseEquals :: Expparser Exp
    parseEquals = createBinaryOpParser parseBigger parseEqualsLoop EqualsRel
    parseEqualsLoop :: Expparser (Maybe Exp)
    parseEqualsLoop = createLoopParser EqualsRel parseEquals
    
    parseBigger :: Expparser Exp
    parseBigger = createBinaryOpParser parseSmaller parseBiggerLoop BiggerRel
    parseBiggerLoop :: Expparser (Maybe Exp)
    parseBiggerLoop = createLoopParser BiggerRel parseBigger
    
    parseSmaller :: Expparser Exp
    parseSmaller = createBinaryOpParser parseArithmeticExpression parseSmallerLoop SmallerRel
    parseSmallerLoop :: Expparser (Maybe Exp)
    parseSmallerLoop = createLoopParser SmallerRel parseSmaller
    

    parseArithmeticExpression :: Expparser Exp
    parseArithmeticExpression = parsePlus
    
    parsePlus :: Expparser Exp
    parsePlus = createBinaryOpParser parseMinus parsePlusLoop PlusOp
    parsePlusLoop :: Expparser (Maybe Exp)
    parsePlusLoop = createLoopParser PlusOp parsePlus

    parseMinus :: Expparser Exp
    parseMinus = createBinaryOpParser parseNegative parseMinusLoop MinusOp
    parseMinusLoop :: Expparser (Maybe Exp)
    parseMinusLoop = createLoopParser MinusOp parseMinus

    parseNegative :: Expparser Exp
    parseNegative []                  = errorEmptyExpression
    parseNegative (MinusOp:expTokens) = (parseArithmeticPrimary expTokens) >>= (\(len, exp) -> case exp of A arithmeticExp -> Right (len+1, A (Minus (LitA 0) arithmeticExp)); B exp -> Left $ "Error: Expected an arithmetic expression in -(" ++ show exp ++ ")")
    parseNegative expTokens           = parseArithmeticPrimary expTokens

    parseArithmeticPrimary :: Expparser Exp
    parseArithmeticPrimary []                   = errorEmptyExpression
    parseArithmeticPrimary (expToken:expTokens) = case expToken of
      LitExp (Left a) -> return (1, A (LitA a   ))
      (VarExp name)   -> return (1, A (VarA name))
      LeftBrackExp    -> do
        (len, partExpToken) <- goToRightBracket 0 expTokens
        (_, partExp) <- parseBooleanExpression partExpToken
        return (1+len+1, partExp)
      _               -> Left $ "Error: Tried to parse " ++ show expToken ++ " as an arithPrimary"

goToRightBracket :: Int -> Expparser [ExpToken]
goToRightBracket _                []                        = Left "Error: A closing bracket is missing"
goToRightBracket 0                (RightBrackExp:expTokens) = Right (0, [])
goToRightBracket openLeftBrackets (LeftBrackExp :expTokens) = goToRightBracket (openLeftBrackets+1) expTokens >>= (\(len, exp) -> return (len+1,LeftBrackExp :exp))
goToRightBracket openLeftBrackets (RightBrackExp:expTokens) = goToRightBracket (openLeftBrackets-1) expTokens >>= (\(len, exp) -> return (len+1,RightBrackExp:exp))
goToRightBracket openLeftBrackets (noBracket    :expTokens) = goToRightBracket (openLeftBrackets  ) expTokens >>= (\(len, exp) -> return (len+1,noBracket    :exp))

getExpression :: String -> Either String Exp
getExpression s = lexerExpression s >>= parseExpression

lexerProgram :: String -> Either String [WhileToken]
lexerProgram "" = return []
lexerProgram ('-':'-':s) = lexerProgram (skipLine s) where
  skipLine :: String -> String
  skipLine [] = []  
  skipLine ('\n':nextLine) = nextLine  
  skipLine (char:rest) = skipLine rest
lexerProgram (c:s)
  | c == ' ' || c == '\r' = lexerProgram s
  | c == ';' = lexerProgram s >>= return.(Semicolon:)
  | c == '\n' = lexerProgram s >>= return.(NewLine:)
  | c == '\t' = lexerProgram s >>= return.(Indent:)
  | isAlpha c = do 
    (len , token ) <- getAlphaWhileToken (c:s)
    tokens <- lexerProgram (drop (len-1) s)
    return (token:tokens)
  | c == ':' = lexerProgram (drop 1 s) >>= (\tokens -> return $ if take 1 s == "=" then AssignmentOp:tokens else tokens)
  | otherwise = do 
    (len, exp) <- tryLexingExpression (c:s) 
    tokens <- lexerProgram (drop (len-1) s)
    return (exp:tokens)
  where
    getAlphaWhileToken:: String -> Either String (Int, WhileToken) -- returns the WhileToken as well as the length of the word
    getAlphaWhileToken s1 = let (len, word) = getText s1 in case word of
      "while" -> Right (5, While)
      "do"    -> Right (2, Do   )
      "if"    -> Right (2, If   )
      "then"  -> Right (4, Then )
      "else"  -> Right (4, Else )
      "skip"  -> Right (4, Skip )
      _ -> tryLexingExpression s1 -- try to get a variable or an expression starting with a variable 
    readExpression :: String -> (Int, String)
    readExpression [] = (0,"")
    readExpression ('d':'o':cs) = (0,"")
    readExpression ('t':'h':'e':'n':cs) = (0,"")
    readExpression ('e':'l':'s':'e':cs) = (0,"")
    readExpression (':':'=':cs) = (0,"")
    readExpression (c:s)
      | c == ';'  = (0,"")
      | c == '\t' = readExpression s
      | c == '\n' = (0,"") -- specifically for an expression in an assignment at the end of a subprogram
      | otherwise = let (nextLen, nextString) = readExpression s in (nextLen+1, c:nextString)
    tryLexingExpression (c:s) = let (len, exprText) = readExpression (c:s); expr = getExpression exprText in case expr of
      Left err -> Left ("Error: No valid expression found in \"" ++ exprText ++ "\" --> " ++ err) 
      Right exp -> Right (len, Expression exp)

{- 
Grammar for While-Language:

Semicolon -> While (; While)*
While -> while Expression do While | If
If -> if Expression do While | Ass
Ass -> Expression := Expression | -- The first expression must be a variable
Skip -> skip

-}
type Whileparser a = [WhileToken] -> Either String (Int, a)
parseProgram :: [WhileToken] -> Either String While
parseProgram  w = parseSemicolon w >>= return.snd where
  createLoopParser command rest = ""

  parseSemicolon :: Whileparser While
  parseSemicolon expTokens = 
    let 
      subExp1 = (parseWhile expTokens)
      subExp2 = subExp1 >>= (\(len, exp) -> parseSemicolonLoop (drop len expTokens)) in 
      case (subExp1, subExp2) of
        (Left err1          , _                  ) -> Left err1
        (_                  , Left err2          ) -> Left err2
        (Right (len1, expr1), Right (len2, Nothing)) -> Right (len1, expr1)
        (Right (len1, expr1), Right (len2, Just expr2)) -> Right (len1 + 1 + len2, (NEW expr1 expr2))
  parseSemicolonLoop :: Whileparser (Maybe While)
  parseSemicolonLoop []                       = Right (0,Nothing)
  parseSemicolonLoop (Semicolon:[])           = Left $ "Error: Found semicolon at the end of a (sub-)program"
  parseSemicolonLoop (Semicolon:NewLine:toks) = parseSemicolon toks >>= (\(len, exp) -> return (len+1, Just exp))
  parseSemicolonLoop (Semicolon:Indent:toks) = parseSemicolon toks >>= (\(len, exp) -> return (len+1, Just exp))
  parseSemicolonLoop (Semicolon:toks) = parseSemicolon toks >>= (\(len, exp) -> return (len, Just exp))
  parseSemicolonLoop (_:toks) = Left $ "Error: Missing Semicolon before " ++ showArray toks
  parseWhile :: Whileparser While
  parseWhile [] = Left "Error: Found nothing where command was expected"
  parseWhile (While:Expression b:Do:NewLine:Indent:rest) = 
    do 
      (len, subprogramCode) <- splitSubprogram rest
      subprogram <- parseProgram subprogramCode
      b <- checkIfBoolean b
      return (5+len, WHILE b subprogram)
  parseWhile notWhile = parseIf notWhile
  parseIf :: Whileparser While
  parseIf [] = Left "Error: Found nothing where command was expected"
  parseIf (If:Expression b:Then:NewLine:Indent:rest) =
    do 
      (lenThen, subprogramCodeThen) <- splitSubprogram rest
      (lenElsePreamble, rest2) <- checkElseAfterIf (drop lenThen rest)
      (lenElse, subprogramCodeElse) <- splitSubprogram rest2
      subprogramThen <- parseProgram subprogramCodeThen
      subprogramElse <- parseProgram subprogramCodeElse
      b <- checkIfBoolean b
      return (5+lenThen+lenElsePreamble+lenElse, IF b subprogramThen subprogramElse )
  parseIf notIf = parseAss notIf
  parseAss :: Whileparser While
  parseAss (Expression x:AssignmentOp:Expression a:rest) = getNameFromExpression x >>= (\name -> return (3, ASS name a))
  parseAss notAss = parseSkip notAss
  parseSkip :: Whileparser While
  parseSkip (Skip:Semicolon:[]) = Left "Error: Found semicolon at the end of a (sub-)program" -- This line is probably unneccesary
  parseSkip (Skip:Semicolon:rest) = return (1,SKIP)
  parseSkip (Skip:[]) = return (1,SKIP)
  parseSkip (Skip:rest) = let (len, isJustTrail) = checkForTrailingEndOfProgram rest in 
    if isJustTrail 
      then return (1+len, SKIP) 
      else Left $ "Error: Command \"" ++ showArray (Skip:rest) ++ "\" does not contain correct syntax"
  parseSkip notSkip = Left $ "Error: Command \"" ++ showArray notSkip ++ "\" does not contain correct syntax"


checkForTrailingEndOfProgram :: [WhileToken] -> (Int, Bool)
checkForTrailingEndOfProgram (NewLine:rest) = let (len, isJustTrail) = checkForTrailingEndOfProgram rest in (len+1, isJustTrail) 
checkForTrailingEndOfProgram (Indent :rest) = let (len, isJustTrail) = checkForTrailingEndOfProgram rest in (len+1, isJustTrail)
checkForTrailingEndOfProgram [] = (0, True)
checkForTrailingEndOfProgram _ = (-1,False)
--removes 1 Indent in every command
splitSubprogram :: Whileparser [WhileToken]
-- beginning of new command
splitSubprogram (Semicolon:NewLine:       []          ) = Left "Error: (Sub-)Program ends with a semicolon" 
splitSubprogram (Semicolon:               []          ) = Left "Error: (Sub-)Program ends with a semicolon"
splitSubprogram (Semicolon:NewLine:Indent:rest        ) = splitSubprogram rest >>= (\(len, subprogram) -> return (len+3, Semicolon:NewLine:subprogram)) 
splitSubprogram (Semicolon:        Indent:rest        ) = splitSubprogram rest >>= (\(len, subprogram) -> return (len+2, Semicolon:        subprogram))
splitSubprogram (Semicolon:NewLine:       noIndent    ) = Right (0, []) -- the reason why 2 is used is becasue after a subprogram, we alos need to remove the "trailing" Semicolon and NewLine  
splitSubprogram (Semicolon:               noWhitespace) = Right (0, []) -- the reason why 1 is used is becasue after a subprogram, we alos need to remove the "trailing" Semicolon 
-- beginning of another subprogram
splitSubprogram (        Do       :NewLine:Indent:Indent:rest) = splitSubprogram rest >>= (\(len, subprogram) -> return (len+4, Do       :NewLine:Indent:subprogram)) 
splitSubprogram (        Then     :NewLine:Indent:Indent:rest) = splitSubprogram rest >>= (\(len, subprogram) -> return (len+4, Then     :NewLine:Indent:subprogram)) 
splitSubprogram (Indent :Else     :NewLine:Indent:Indent:rest) = splitSubprogram rest >>= (\(len, subprogram) -> return (len+5, Else     :NewLine:Indent:subprogram)) 
splitSubprogram (NewLine:Else     :NewLine:Indent:rest) = Right (0,[]) 
splitSubprogram (Do       :NewLine:Indent:rest) = Left "Error: Empty Subprgramm" 
splitSubprogram (Then     :NewLine:Indent:rest) = Left "Error: Empty Subprgramm" 
-- inside of command
splitSubprogram (inCommand:               rest) = splitSubprogram rest >>= (\(len, subprogram) -> return ((len+1, inCommand:subprogram)))
splitSubprogram []                              = Right (0, []) -- I feel like there can be some kind of mistake here
checkIfBoolean :: Exp -> Either String BExp
checkIfBoolean (B b) = Right b
checkIfBoolean (A (VarA a)) = Right (VarB a)
checkIfBoolean (Var name) = Right (VarB name)
checkIfBoolean (A a) = Left $ "Error: Expexted boolean expression, but got " ++ show a ++ " insted"
checkElseAfterIf :: Whileparser [WhileToken]
checkElseAfterIf (NewLine:Else:NewLine:Indent:rest) = Right (4,rest)
checkElseAfterIf notElse = Left $ "Error: Expected a new line with an else followed by another new line and a tab, like: \n\n\t...\nelse\n\t...\n\ninstead got " ++ showArray notElse
getNameFromExpression :: Exp -> Either String String
getNameFromExpression (A (VarA name)) = Right name
getNameFromExpression (B (VarB name)) = Right name
getNameFromExpression (Var name)      = Right name
getNameFromExpression _               = Left "Error: Did not use variable on the left side of an assignment"

getProgram :: String -> Either String While
getProgram p = lexerProgram p >>= parseProgram

{- WHILE PROGRAMS -}

mult :: AExp -> AExp -> While
mult n m =
  "multAns" <~ A (LitA 0) #
  "multN" <~ A n #
  WHILE ((VarA "multN" === LitA 0) !!!) (
    IF (VarA "multN" >>> LitA 0) (
      "multAns" <~ A (VarA "multAns" =+= m)#
      "multN" <~ A (VarA "multN" =-= LitA 1)
    ) (
      "multAns" <~ A (VarA "multAns" =-= m)#
      "multN" <~ A (VarA "multN" =+= LitA 1)
    )
  )

factorial :: AExp -> While
factorial n =
  "factorialN" <~ A (LitA 2) #
  "factorialAns" <~ A (LitA 1) #
  WHILE (VarA "factorialN" <<< n) (
    mult (VarA "factorialN") (VarA "factorialAns") #
    "factorialAns" <~ A (VarA "multAns") #
    "factorialN" <~ A (VarA "factorialN" =+= LitA 1)
  )

div :: AExp -> AExp -> While
div dividend divisor  =
  ("dividend" <~ A dividend) #
  ("divisor" <~ A divisor) #
  ("result" <~ A (LitA 1)) #
  WHILE (VarA "divisor" <<== VarA "dividend") (
    ("dividend" <~ A (VarA "dividend" =-= VarA "divisor")) #
    ("result" <~ A (VarA "result" =+= LitA 1))
  )