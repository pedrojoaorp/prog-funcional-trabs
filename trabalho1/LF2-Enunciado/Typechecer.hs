module Typechecer where

import AbsLF
import Prelude hiding (lookup)
import PrintLF

data R a = OK a | Erro String                                   
         deriving (Eq, Ord, Show, Read)

isError e = case e of
    OK _ -> Falsemsg
type TContext = [(Ident,Type)]

{-
int main ()
{
  fat (5)
}
int fat (int n)
{
  if (n) then n * fat (n - 1) else 1
}
-}

test1 = Prog [Fun Tint (Ident "main") [] (ECall (Ident "fat") [EInt 5]),Fun Tint (Ident "fat") [Dec Tint (Ident "n")] (EIf (EVar (Ident "n")) (EMul (EVar (Ident "n")) (ECall (Ident "fat") [ESub (EVar (Ident "n")) (EInt 1)])) (EInt 1))]
test2 = Prog [Fun Tint (Ident "main") [] (ECall (Ident "fat") [EAdd (EInt 5) (EInt 1)]),Fun Tint (Ident "fat") [Dec Tint (Ident "n")] (EIf (EVar (Ident "n")) (EMul (EVar (Ident "n")) (ECall (Ident "fat") [ESub (EVar (Ident "n")) (EInt 1)])) (EInt 1))]

typeCheckP :: Program  -> [R TContext]
typeCheckP (Prog fs) = let nCtx = updatecF [] fs in
                          case nCtx of
                             OK ctx -> map (typeCheckF ctx) fs
                             Erro msg -> [Erro msg]

{- TODO: na definição de "typeCheckF" abaixo,substitua "undefined" 
         pelo argumento relevante -}               
-- Observa-se que a função tke recebe como argumentos um contexto, uma expressão, e um tipo, e retorna confirmação e o novo contexto; o undefined está no lugar da expressão. Na LI2Tipada, na typeCheckF, há um uso da função tke em que está recebendo como expressão de argumento a expressão do comando de retorno; aqui, de maneira similar, a função tke deve receber o resultado da função, que é neste caso o exp entrando como argumento do typeCheckF.
typeCheckF ::  TContext -> Function -> R TContext    
typeCheckF tc (Fun tR _ decls exp) = tke (parameterTypeBindings ++ functionTypes) exp tR
                                        where parameterTypeBindings = map (\(Dec tp id) -> (id,tp)) decls
                                              functionTypes = filter (\(i,t) -> case t of 
                                                                                 TFun _  _ -> True 
                                                                                 _ -> False
                                                                      ) tc

{- "tke" é uma função que dado, um contexto de tipos, uma expressão, e um tipo,
   verifica se essa expressão tem esse tipo ou retorna um erro se a expressão- 
   for mal tipada -}                                    
tke :: TContext -> Exp -> Type -> R TContext
tke tc exp tp = let r = tinf tc exp in
                          case r of
                             OK tipo -> if (tipo == tp )
                                           then OK tc
                                           else Erro ("@typechecker: a expressao "++ printTree exp ++ " tem tipo " ++ 
                                                     printTree tipo ++ " mas o tipo esperado eh "
                                                     ++ printTree tp)
                             Erro msg -> Erro msg  


{- "tinf" é uma função que dado, um contexto de tipos e uma expressão, retorna
   o tipo dessa expressão ou um erro se a expressão for mal tipada -}                                    
tinf :: TContext -> Exp -> R Type
tinf tc x  =  case x of
    ECon exp0 exp  -> combChecks tc exp0 exp TStr
    EAdd exp0 exp  -> combChecks tc exp0 exp Tint
    ESub exp0 exp  -> combChecks tc exp0 exp Tint
    EMul exp0 exp  -> combChecks tc exp0 exp Tint
    EDiv exp0 exp  -> combChecks tc exp0 exp Tint
    EOr  exp0 exp  -> combChecks tc exp0 exp Tbool
    EAnd exp0 exp  -> combChecks tc exp0 exp Tbool
    ENot exp       -> let r = tke tc exp Tbool in 
                         case r of 
                             OK _ -> OK Tbool
                             Erro msg -> Erro msg
    EStr str       -> OK TStr  
    ETrue          -> OK Tbool 
    EFalse         -> OK Tbool  
    EInt n         -> OK Tint  
    EVar id        -> lookup tc id
{- TODO: implemente a checagem de tipo para o "EIf" abaixo:
   "exp" deve ser inteiro (Tint), e os tipos de "expT" e "expE" devem ser iguais.
   @dica: estude a estrutura da checagem de tipo do "SIf" na LI2Tipada. 
-}
-- Primeiro, verificamos se r é do tipo Int com a função tke; depois, usamos a função tinf recursivamente para avaliar o tipo de expT e depois o tipo de expE, e usamos pattern matching para comparar os dois tipos, retornando OK e o tipo das expressões caso sejam os mesmos, ou um erro se forem diferentes.
    eIf@(EIf exp expT expE) -> let r = tke tc exp Tint in 
                                  case r of 
                                      OK _ -> let r2 = tinf tc expT in
                                                case r2 of
                                                  OK _ -> let r3 = tinf tc expE in
                                                            case r3 of
                                                              r2 -> r3
                                                              OK _ -> Erro "Expressões de Then e Else não têm o mesmo tipo"
                                                              Erro msg -> Erro msg
                                                  Erro msg -> Erro msg
                                      {-OK _ -> let r2 = tinf tc expT in
                                                case r2 of
                                                  OK _ -> let r3 = tinf tc expE in
                                                            case r3 of
                                                              OK _ -> if (r2 == r3)
                                                                        then r3
                                                                        else Erro msg
                                                              Erro msg -> Erro msg
                                                  Erro msg -> Erro msg-}
                                      Erro msg -> Erro msg
{- TODO: sobre "ECall" abaixo, a lógica permanece a mesma em relação a LI2Tipada ? Por que? -}  
-- A lógica entre ECall da LF2 e da LI2 é bem parecida, as diferenças que surgem se devem apenas à mudança da linguagem em relação a parar de usar comandos e manter apenas expressões, o que causa alguns comportamentos diferentes nas funções de lookup.
    ECall id lexp   -> let r = lookup tc id in 
                        case r of 
                           OK (TFun tR pTypes) -> if (length pTypes == length lexp)
                                                    then 
                                                      if (isThereError tksArgs /= [])
                                                        then Erro " @typechecker: chamada de funcao invalida"
                                                        else OK tR
                                                      else Erro " @typechecker: tamanhos diferentes de lista de argumentos e parametros"
                                                         where tksArgs = zipWith (tke tc) lexp pTypes
                                                               isThereError l = filter (==False) 
                                                                                       (map (\e->(let r2 = e in  
                                                                                                    case r2 of
                                                                                                      OK _ -> True
                                                                                                      Erro _ -> False)) 
                                                                                         l)
                           Erro msg -> Erro msg


{- *** @dica: nao altere o codigo abaixo até o final do arquivo*** 
              mas saiba explicar o que ele faz
-}
-- combChecks: recebe um contexto de tipos, duas expressões, um tipo e retorna OK apenas se as duas expressões estão corretamente tipadas de acordo com o tipo dado
-- lookup: passa recursivamente pelo contexto completo oferecido, e compara os identificadores no contexto oferecido; retorna um OK junto do valor associado quando encontra o identificador procurado, ou um erro caso não encontre o identificador procurado.
-- updateTC: recebe um contexto, um identificador e um tipo; analisa todo o contexto e, caso não haja nenhum outro item com o identificador igual ao fornecido, retorna o contexto concatenado com o identificador e o tipo fornecidos; caso contrário, retorna um erro.
-- getFunctionType: recebe uma função e retorna o tipo de retorno da função, além dos tipos dos parâmetros que a função recebe.
-- updatecF: recebe um contexto e uma ista de funções; passa recursivamente pela lista inteira, e para cada uma das funções da lista chama updateTC para tentar adicioná-la no contexto fornecido.
                             
combChecks :: TContext -> Exp -> Exp -> Type -> R Type
combChecks tc exp1 exp2 tp = let r = tke tc exp1 tp in
                                       case r of
                                          OK _ -> let r2 = tke tc exp2 tp in
                                                     case r2 of 
                                                         OK _ -> OK tp
                                                         Erro msg -> Erro msg
                                          Erro msg -> Erro msg 
                             

lookup :: TContext -> Ident -> R Type
lookup [] id = Erro ("@typechecker: " ++ printTree id ++ " nao esta no contexto. ")
lookup ((id,value):cs) key
   | id == key = OK value
   | otherwise = lookup cs key


updateTC :: TContext -> Ident -> Type -> R TContext
updateTC [] id tp = OK [(id,tp)]
updateTC ((id,tp):idTps) idN tpN 
  | id == idN = Erro ("@typechecker: identificador" ++ printTree id ++ " nao pode ter mais de um tipo")
  | otherwise = let r = (updateTC idTps idN tpN) in       
                  case r of 
                    OK restOK -> OK ((id,tp) : restOK)    
                    Erro msg -> Erro msg 

getFunctionType :: Function -> Type
getFunctionType (Fun tipoRetorno _ decls _) = TFun tipoRetorno (map (\(Dec tp _ )-> tp) decls)

updatecF :: TContext -> [Function] -> R TContext
updatecF tc [] = OK tc
updatecF tc (f@(Fun _ nomeF _ _):fs) = let r = updateTC tc nomeF (getFunctionType f) in
                                                   case r of 
                                                     OK tcNew -> updatecF tcNew fs
                                                     Erro msg -> Erro msg
                                                     
                                                     
                                                     


