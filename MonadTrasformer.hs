import Control.Lens
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative
import Data.Maybe
import qualified Data.Map as Map


type Name = String

data Exp = Lit Integer
         | Var Name
         | Plus Exp Exp
         | Abs Name Exp
         | App Exp Exp 
         deriving ( Show ) 

data Value = IntVal Integer
           | FunValue Env Name Exp 
           deriving ( Show )

type Env = Map.Map Name Value  --variable name and value

eval0 :: Env -> Exp -> Value
eval0 env ( Lit i ) = IntVal i 
eval0 env ( Var n ) = fromJust . Map.lookup n $ env
eval0 env ( Plus e1 e2 ) =  IntVal $ i1 + i2 where 
          IntVal i1 = eval0 env e1 
          IntVal i2 = eval0 env e2
eval0 env ( Abs n e ) = FunValue env n e
eval0 env ( App e1 e2 ) = 
      let 
          val1 = eval0 env e1
          val2 = eval0 env e2
      in case val1 of 
              FunValue env' n body -> eval0 ( Map.insert n val2 env' ) body

   
type Eval1 alpha = Identity alpha
runEval1 :: Eval1 alpha -> alpha 
runEval1 ev = runIdentity ev

eval1 :: Env -> Exp -> Eval1  Value
eval1 env ( Lit i ) = return . IntVal $ i 
eval1 env ( Var n ) =return . fromJust .  Map.lookup n  $ env
eval1 env ( Plus e1 e2 ) = do 
            IntVal i1 <- eval1 env e1 
            IntVal i2 <- eval1 env e2
            return . IntVal $ i1 + i2 
eval1 env ( Abs n e ) = return . FunValue env n $ e 
eval1 env ( App e1 e2 ) = do 
      val1 <- eval1 env e1
      val2 <- eval1 env e2
      case val1 of 
           FunValue env' n body -> eval1 ( Map.insert n val2 env' ) body 


type Eval2 alpha = ErrorT String Identity alpha
runEval2 :: Eval2 alpha -> Either String alpha
runEval2 ev = runIdentity . runErrorT $ ev

eval2 :: Env -> Exp -> Eval2 Value
eval2 env ( Lit i ) = return . IntVal $ i 
eval2 env ( Var n ) =return . fromJust .  Map.lookup n  $ env
eval2 env ( Plus e1 e2 ) = do 
            e1' <- eval2 env e1 
            e2' <- eval2 env e2
            case (e1' , e2' ) of 
                 ( IntVal i1 , IntVal i2 ) -> return . IntVal $ i1 + i2 
                 _ -> throwError "type error"
eval2 env ( Abs n e ) = return . FunValue env n $ e 
eval2 env ( App e1 e2 ) = do 
      val1 <- eval2 env e1
      val2 <- eval2 env e2
      case val1 of 
           FunValue env' n body -> eval2 ( Map.insert n val2 env' ) body 
           _ -> throwError "type error"

type Eval3 alpha = ReaderT Env ( ErrorT String Identity ) alpha
runEval3 :: Env -> Eval3 alpha -> Either String alpha
runEval3 env ev =    runIdentity . runErrorT . (  runReaderT  ?? env ) $  ev 

eval3 :: Exp -> Eval3 Value
eval3 ( Lit i ) = return . IntVal $ i
eval3 ( Var n ) = do
        env <- ask
        case Map.lookup n env of 
             Nothing -> throwError ( "unbound variable: " ++ n )
             Just val -> return val
eval3 ( Plus e1 e2 ) = do 
      e1' <- eval3 e1
      e2' <- eval3 e2
      case ( e1' , e2' ) of 
           ( IntVal i1 , IntVal i2 ) -> return . IntVal $ i1 + i2 
           _ -> throwError "type error in addition"
eval3 ( Abs n e ) = do 
            env <- ask
            return . FunValue env n $ e 
eval3 ( App e1 e2 ) = do 
      val1 <- eval3 e1
      val2 <- eval3 e2
      case val1 of
           FunValue env' n body -> local ( const ( Map.insert n val2 env' ) ) 
                                         ( eval3 body )
           _ -> throwError "type error in application"