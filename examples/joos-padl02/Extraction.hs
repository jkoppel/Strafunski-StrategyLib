------------------------------------------------------------------------------

module Extraction where

import Monad
import StrategyLib hiding (replaceFocus)
import VarsAnalyses
import Datatypes


--- Method extraction --------------------------------------------------------

                                                         
-- Main strategy

extractMethod :: (Term t, MonadPlus m) => t -> m t
extractMethod
  = applyTP 
  $ once_tdTP   -- traversal for finding scope
  $ monoTP      -- one type-specific case
  $ extrMethFromCls "newMethod"  -- operate on class scope


-- Extract method in class scope for introduction 

extrMethFromCls :: 
     MonadPlus m
  => Identifier          -- name of prospective method
  -> ClassDeclaration    -- class for introduction
  -> m ClassDeclaration
extrMethFromCls mname (ClassDecl fin nm sup fs cs ds)
  = do
       (pars,body) <- analyseExtract ds
       let newMethod = constructMethod mname pars body
       ds' <- replaceFocus mname pars (ds++[newMethod])
       return (ClassDecl fin nm sup fs cs ds')


-- Determine parameters and body; check side conditions

analyseExtract :: (Term t, MonadPlus m)
               => t -> m ([(Identifier,Type)],Statement)
analyseExtract ds
  = applyTU (once_peTU [] appendLocals analyseExtract1) ds
  where analyseExtract1 env
          = testFocus `passTU` \s ->
            ifthenTU (voidTP (isLegal env))
            ( freeUseVars env `passTU` \pars ->
              constTU (pars,s) )
        appendLocals env = op2TU appendMap (tryTU declVars) (constTU env)


-- Legality check

isLegal :: MonadPlus m => [([Char],Type)] -> TP m
isLegal env        = freeDefVars env `passTP` \env' ->
                     if null env' then notTU (once_tdTU testReturn) else failTP
                            

-- Test for return statement

testReturn :: MonadPlus m => TU (Maybe Expression) m
testReturn =  monoTU (\s -> case s of
                        (ReturnStat x) -> return x
                        _              -> mzero )


-- Replace focused statement by method invocation

replaceFocus :: (Term t, MonadPlus m)
             => Identifier -> [(Identifier,Type)]
             -> t -> m t
replaceFocus mname pars ds
  = applyTP (once_tdTP rewrite) ds
    where
      mcall = constructMethodCall mname pars
      rewrite = testFocus `passTP` \_ -> 
                 monoTP (const (return mcall))


-- Test for focus

testFocus :: MonadPlus m => TU Statement m
testFocus = monoTU ( \s -> case s of
                             (StatFocus s') -> return s'
                             _              -> mzero )


-- Construct a method from body with its free variables

constructMethod :: 
     Identifier                 -- name for new method
  -> [(Identifier,Type)]        -- free variable names and types
  -> Statement                  -- prospective body of method 
  -> MethodDeclaration
constructMethod mname pars body
  = MethodDecl Nothing mname
       (FormalParams fpars)
       (BlockStatements [] [body])
    where fpars = map (\(v,t) -> FormalParam t v) pars

-- Construct a method call with free variable names as parameters

constructMethodCall :: Identifier -> [(Identifier,Type)] -> Statement
constructMethodCall mname pars
  = MethodInvocationStat 
  $ ExpressionInvocation This mname (Arguments args)
      where args = map (\(v,t) -> Identifier v) pars

------------------------------------------------------------------------------
