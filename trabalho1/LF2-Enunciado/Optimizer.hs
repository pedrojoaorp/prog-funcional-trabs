module Optimizer where

import AbsLF
import Interpreter
import AbsLF (Exp(ESub))

optimizeP :: Program -> Program
optimizeP (Prog fs) = Prog (map optimizeF fs)
       
optimizeF :: Function -> Function
optimizeF (Fun tR id decls exp) = Fun tR id decls (optimizeE exp)

optimizeE :: Exp -> Exp
optimizeE exp  = case exp of
                      EStr str -> EStr str
                      ETrue    -> ETrue
                      EFalse   -> EFalse
                      EInt n   -> EInt n
                      EVar id  -> EVar id
                      ENot exp -> let optExp  = optimizeE  exp
                                      optENot = ENot optExp in
                                      if (isLiteral optExp) 
                                        then  wrapValueExpression (eval [] optENot )
                                        else  optENot
                      ECon exp0 exp -> let optExp0 = optimizeE  exp0 
                                           optExp  = optimizeE  exp   
                                           optECon = ECon optExp0 optExp in 
                                           if ((isLiteral optExp0) && (isLiteral optExp))
                                             then wrapValueExpression (eval [] optECon)
                                             else optECon
                      EAdd exp0 exp -> let optExp0 = optimizeE exp0
                                           optExp  = optimizeE exp  
                                           optEAdd = EAdd optExp0 optExp in 
                                             if ((isLiteral optExp0) && (isLiteral optExp)) 
                                               then wrapValueExpression (eval [] optEAdd)
                                               else optEAdd
{- TODO: substitua "undefined" abaixo pela otimização correspondente ao tipo de expressão.
   @dica: estude a implementação fornecida da otimização das expressões anteriores -}
-- Explicação: basicamente copiei as otimizações acima e colei embaixo, mudando apenas o nome para encaixar. O que está sendo feito na otimização é passar pelos expressões que são os argumentos(?) da expressão e aplicar a função de otimização neles caso sejam também expressões com expressões dentro; depois, ele verifica se todas as expressões resultantes nos argumentos são literais (não possuem variáveis) e, se for o caso, já aproveita e avalia o resultado da expressão direto, caso contrário só devolve a expressão otimizada.
                      ESub exp0 exp -> let optExp0 = optimizeE exp0
                                           optExp  = optimizeE exp  
                                           optESub = ESub optExp0 optExp in 
                                             if ((isLiteral optExp0) && (isLiteral optExp)) 
                                               then wrapValueExpression (eval [] optESub)
                                               else optESub
                      EMul exp0 exp -> let optExp0 = optimizeE exp0
                                           optExp  = optimizeE exp  
                                           optEMul = EMul optExp0 optExp in 
                                             if ((isLiteral optExp0) && (isLiteral optExp)) 
                                               then wrapValueExpression (eval [] optEMul)
                                               else optEMul
                      EDiv exp0 exp -> let optExp0 = optimizeE exp0
                                           optExp  = optimizeE exp  
                                           optEDiv = EDiv optExp0 optExp in 
                                             if ((isLiteral optExp0) && (isLiteral optExp)) 
                                               then wrapValueExpression (eval [] optEDiv)
                                               else optEDiv
                      EOr  exp0 exp -> let optExp0 = optimizeE exp0
                                           optExp  = optimizeE exp  
                                           optEOr  = EOr  optExp0 optExp in 
                                             if ((isLiteral optExp0) && (isLiteral optExp)) 
                                               then wrapValueExpression (eval [] optEOr )
                                               else optEOr 
                      EAnd exp0 exp -> let optExp0 = optimizeE exp0
                                           optExp  = optimizeE exp  
                                           optEAnd = EAnd optExp0 optExp in 
                                             if ((isLiteral optExp0) && (isLiteral optExp)) 
                                               then wrapValueExpression (eval [] optEAnd)
                                               else optEAnd
{- TODO: saiba explicar o motivo da otimização abaixo -}
-- Explicação: a expressão ECall busca no contexto a definição da função com nome "id" e pode ter qualquer quantidade de argumentos, então "lexp" é a lista de expressões de argumento da função. O que está sendo feito aqui é aplicar por meio do mapeamento a função de otimização em cada um dos argumentos da função chamada.
                      ECall id lexp   -> ECall id (map (\expr ->  optimizeE expr) lexp) 
{- TODO: crie um programa exemplo em que a otimização abaixo seja usada -}
-- O programa é exTODO.lf2, em que ele já no otimizador verifica que a expressão de condição dá logo um valor de int verificável e já escolhe a branch a seguir.
                      EIf exp expT expE -> let optExp  = optimizeE exp 
                                               optThen = optimizeE expT
                                               optElse = optimizeE expE 
                                               optEIf  = EIf optExp optThen optElse in 
                                                 case optExp of
                                                   EInt vExpIf -> if (vExpIf == 0)
                                                                     then optElse
                                                                     else optThen
                                                   _              -> optEIf

                        
isLiteral :: Exp -> Bool
isLiteral exp = case exp of
                        EStr  _        -> True
                        ETrue          -> True
                        EFalse         -> True
                        EInt  _        -> True
                        _              -> False

wrapValueExpression :: Valor -> Exp 
wrapValueExpression (ValorInt i) = EInt i
wrapValueExpression (ValorStr s) = EStr s
wrapValueExpression (ValorBool True) = ETrue
wrapValueExpression (ValorBool False) = EFalse



