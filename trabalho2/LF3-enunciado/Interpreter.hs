module Interpreter where

import AbsLF
import AbsLFAux  -- TODO: leia agora o conteudo desse arquivo (AbsLFAux.hs) e explique por que refatoramos assim -- As novas funções definidas lidam com a nova expressão ELambda, que funciona de maneira parecida mas não igual a uma Function e por isso precisa de funções prórprias para resgatar suas informações
import Prelude hiding (lookup)
import AbsLF (Exp(EComp, EDiv, ELambda))


executeP :: Program -> Valor

executeP (Prog fs) =  eval (updatecF [] fs) (expMain fs)
    where expMain (f:xs) 
              | (getName f == (Ident "main")) =  getExp f
              | otherwise = expMain xs                                            

type RContext = [(Ident,Valor)]
   
eval :: RContext -> Exp -> Valor
eval context x = case x of
    ECon exp0 exp  -> ValorStr ( s (eval context exp0) ++  s (eval context exp) )
    EAdd exp0 exp  -> ValorInt ( i (eval context exp0)  +  i (eval context exp))
    ESub exp0 exp  -> ValorInt ( i (eval context exp0)  -  i (eval context exp)) 
    EMul exp0 exp  -> ValorInt ( i (eval context exp0)  *  i (eval context exp))
    EDiv exp0 exp  -> ValorInt ( i (eval context exp0) `div` i (eval context exp)) 
    EOr  exp0 exp  -> ValorBool ( b (eval context exp0)  || b (eval context exp))
    EAnd exp0 exp  -> ValorBool ( b (eval context exp0)  && b (eval context exp))
    ENot exp       -> ValorBool ( not (b (eval context exp)))
    EStr str       -> ValorStr str
    ETrue          -> ValorBool True
    EFalse         -> ValorBool False
    EInt n         -> ValorInt n
    EVar id        -> lookup context  id
    EIf exp expT expE -> if ( i (eval context exp) /= 0) 
                          then eval context expT
                          else eval context expE
    -- TODO: na linha abaixo, retorne um ValorFun contendo o lambda e saiba explicar a razao                            
    lambda@(ELambda params exp) -> ValorFun (ELambda params exp) -- undefined aqui; ValorFun agora contém uma expressão em vez de uma função. Estamos só colocando a expressão lambda dentro de um Valor; a expressão lambda continua intocada pois ela só é "resolvida" dentro de outras expressões como ECall ou EComp ~~~a exp dentro da expressão lambda entra num subst em vez de um eval pois as variáveis que representam os parâmetros não existem com valores no contexto, e portanto eval não seria adequado~~~
    -- TODO: em EComp abaixo, troque undefined (2 ocorrencias) pela construcao apropriada
    -- primeiro, fazemos um ECall na segunda função, com os argumentos sendo apenas os parâmetros embrulhados em expressões de variáveis, o retorno será uma expressão basicamente idêntica; segundo, o retorno da segunda função deve ser compatível com a entrada da primeira função, então colocamos o resultado desse primeiro ECall como argumento para o segundo ECall chamando a primeira função; terceiro, o resultado do segundo ECall será a primeira função com o parâmetro substituído pela segunda função onde houver, e para retornar essa função devidamente colocamos ela numa expressão lambda que recebe como parâmetros os parâmetros da segunda função, e colocamos tudo dentro de um ValorFun que é o retorno de eval
    EComp exp1 exp2 ->  let (ValorFun exp1') = eval context exp1
                            (ValorFun exp2') = eval context exp2 in -- undefined aqui; só queremos resgatar a expressão lambda da exp2, de maneira parecida à exp1
                          ValorFun(ELambda (getParamsTypesL exp2') 
                                           (ECall exp1' [ECall exp2' (getParamsExpL exp2')])) -- undefined aqui; como explicado, estamos executando a função da exp1 com a exp2 como argumento
    {- TODO: em ECall abaixo, troque undefined (3 ocorrencias) pela construcao apropriada.                           
       Dica: estude o codigo, buscando entender tambem as definicoes locais -}
    ECall exp lexp ->  if (length lexp < length parameters) 
                         then ValorFun (ELambda params' exp') -- TODO: que caso eh esse ? -- Esse é o caso de aplicação parcial da função -- dois undefined aqui; no caso de aplicação parcial, avaliação de ECall retorna uma função com os parâmetros que sobraram, e a expressão com os argumentos existentes aplicados
                         else eval (paramBindings ++ contextFunctions) exp' -- TODO: que caso eh esse ? -- Esse é o caso de aplicação total da função -- no caso de aplicação total, avaliação de ECall retorna o valor resultante da avaliação da exp dentro da expressão lambda, com os argumentos recebidos somados às funções do contexto como contexto
                        where (ValorFun lambda) = eval context exp -- undefined aqui; avaliar uma expressão lambda por si só apenas retorna a mesma expressão dentro de um valor
                              parameters = getParamsL lambda -- lista de identificadores dos parâmetros
                              paramBindings = zip parameters (map (eval context) lexp) -- lista de tuplas com identificadores pareados a valores resultantes das expressões recebidas como argumentos; formatado igual a contexto, observar que zip funciona para listas de tamanho diferente
                              params' = drop (length lexp) (getParamsTypesL lambda) -- lista de parâmetros, retirando os n primeiros, com n sendo a quantidade de argumentos recebidos; possui os argumentos não aplicados, será vazio em aplicação total
                              exp' = subst paramBindings (getExpL lambda)  -- substitui os parâmetros presentes na expressão em lambda pelos argumentos recebidos; paramBindings possui apenas os argumentos presentes na aplicação da função, e deixa parâmetros não aplicados intactos
                              contextFunctions = filter (\(i,v) -> case v of -- todas definições de funções presentes no contexto
                                                                         ValorFun _ -> True 
                                                                         _ -> False
                                                           ) 
                                                          context


-- a função "subst" gera uma nova expressao a partir dos bindings em RContext
subst :: RContext -> Exp -> Exp 
subst rc exp  = case exp of  
    EVar id        -> bind id rc -- TODO: por que eh implementado assim ? -- bind pega um identificador e retorna uma expressão, o que alinha com o retorno da função subst, ao contrário do lookup que retorna um valor 
    -- TODO: explique a implementacao da linha abaixo -- subst pega uma expressão e substitui todas as variáveis nela por seus valores; na linha abaixo, o que estamos fazendo é substituir na exp, contida na expressão lambda, todas as variáveis contidas nela, exceto as variáveis com identificador igual aos parâmetros da expressão lambda, que por serem parâmetros não possuem valor e só serão substituídos na avaliação.
    lambda@(ELambda paramsTypes exp) -> ELambda paramsTypes (subst (rc `diff` (getParamsL lambda)) exp)
    ECall exp lexp -> ECall (subst rc exp ) (map (subst rc) lexp)
    EAdd exp0 exp  -> EAdd (subst rc exp0 ) (subst rc exp )
    -- TODO: nos casos abaixo, troque cada undefined pela construcao apropriada -- undefined abaixo; Apenas repliquei os exemplos já dados com as funções abaixo
    EComp exp1 exp2 -> EComp (subst rc exp1) (subst rc exp2)
    EIf expC expT expE -> EIf (subst rc expC) (subst rc expT) (subst rc expE)
    ECon exp0 exp  -> ECon (subst rc exp0) (subst rc exp)
    ESub exp0 exp  -> ESub (subst rc exp0) (subst rc exp)
    EMul exp0 exp  -> EMul (subst rc exp0) (subst rc exp)
    EDiv exp0 exp  -> EDiv (subst rc exp0) (subst rc exp)
    EOr  exp0 exp  -> EOr  (subst rc exp0) (subst rc exp)
    EAnd exp0 exp  -> EAnd (subst rc exp0) (subst rc exp)
    ENot exp       -> ENot (subst rc exp)
    _ -> exp   -- TODO: quais sao esses casos e por que sao implementados assim ? -- Esses são os casos EInt, EStr, ETrue, e EFalse, que são valores literais e portanto não faz sentido substituir

{- TODO: 
  sobre a implementacao finalizada de subst:
  1) qual eh o caso base?
  2) como descrever o numero de casos recursivos? depende (in)diretamente de algo?
  3) qual a finalidade dos casos recursivos?
  4) por que a linha 64 (caso lambda) eh diferente dos outros casos recursivos?  
  5) numa especificacao textual intuitiva e concisa (semelhante ao comentario na linha 59),
     qual a linha mais importante entre 62-77 (EVar id ... // _ -> exp) ?
  6) Ha semelhanca de implementacao em relacao ao Optimizer.hs? Qual(is)?    
-}

{- 
1) o caso base é exp == EVar id, que é onde acontece a subtituição que é o objetivo da função
2) é igual ao número de expressões que representam a execução de alguma operação; nisto não estão incluídas expressões de variáveis e de literais
3) percorrer a expressão por completo para chegar em todos os valores base, sejam variáveis ou literais, e substituir as variáveis por seus valores
4) a expressão a ser substituída possui 
5) a linha do caso EVar id, pois é onde ocorre a substituição que é o objetivo principal da função
6) sim, principalmente na maneira que a função recursivamente percorre a expressão por completo para analisar todas as expressões contidas dentro, exceto que enquanto o Optimizer procura valores literais para resolver de adiantado, a subst procura variáveis para substituir por seus valores
 -}

-- a função "diff" faz a diferença, tirando de RContext os mapeamentos envolvendo [Ident].
diff :: RContext -> [Ident] -> RContext
rc `diff` [] = rc
[] `diff` _ = [] 
((k,v):kvs) `diff` (id:ids) 
    | k == id =  kvs `diff` ids
    | otherwise = (k,v) : ( kvs `diff` (id:ids))

-- a função bind retorna uma expressao contendo o valor do id no RContext, ou o proprio id. 
-- TODO: por que nao usamos o lookup no lugar de bind ? -- Enquanto o lookup retorna o valor encontrado, o bind retorna uma expressão contendo o valor encontrado, e por isso cumpre melhor a funcionalidade necessária no subst
bind :: Ident -> RContext -> Exp
bind id [] = EVar id  -- retorna o proprio id se ele nao esta ligado em RContext
bind id ((k,v):kvs)
    | k == id = wrapValueExpression v 
    | otherwise = bind id kvs 

-- "wrapValueExpression" empacota um valor em uma expressao 
wrapValueExpression :: Valor -> Exp 
wrapValueExpression (ValorInt i) = EInt i
wrapValueExpression (ValorStr s) = EStr s
wrapValueExpression (ValorBool True) = ETrue
wrapValueExpression (ValorBool False) = EFalse
wrapValueExpression (ValorFun exp) = exp 


data Valor = ValorInt {
               i :: Integer         
             }
            | 
             ValorFun {
               f :: Exp   --f :: Function  **NOVO TODO: Por que mudou ? -- Porque o ValorFun agora deve receber, além de uma função comum, um valor lambda também, e para isso estamos generalizando funções como expressões lambda, e a criação e chamada de funções normal vira uma especificação em cima disso
             }   
            | 
             ValorStr {
               s :: String
             } 
            |
             ValorBool {
               b :: Bool
             }

instance Show Valor where
  show (ValorBool b) = show b
  show (ValorInt i) = show i
  show (ValorStr s) = s
  show (ValorFun f) = show f  -- TODO: por que essa linha funciona ? -- Porque em AbsLF, todas as expressões, incluindo a ELambda, que é o conteúdo do ValorFun, derivam c.Show e portanto têm a função show definida
  

lookup :: RContext -> Ident -> Valor
lookup ((i,v):cs) s
   | i == s = v
   | otherwise = lookup cs s

update :: RContext -> Ident -> Valor -> RContext
update [] s v = [(s,v)]
update ((i,v):cs) s nv
  | i == s = (i,nv):cs
  | otherwise = (i,v) : update cs s nv


-- NOVO: TODO: explique a mudanca em updatecF -- A mudança se deve porque ValorFun agora contém uma expressão em vez de uma função, e como updatecF recebe uma lista de funções, precisamos gerar uma expressão lambda a partir dos parâmetros e da expressão que a função da lista contém
updatecF :: RContext -> [Function] -> RContext
updatecF c [] = c
updatecF c (f:fs) = updatecF (update c (getName f)    
                                       (ValorFun (ELambda (getParams f) (getExp f)))) 
                              fs
-- updatecF c (f:fs) = updatecF (update c (getName f) (ValorFun f)) fs
