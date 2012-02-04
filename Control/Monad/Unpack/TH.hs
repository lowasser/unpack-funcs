{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Control.Monad.Unpack.TH (unpack1Instance, unpackInstance, noUnpackInstance, tupleInstance) where

import Control.Monad
import Control.Monad.Unpack.Class

import Language.Haskell.TH
import System.IO.Unsafe
import Data.IORef

-- Apparently, newName still generates name conflicts in GHC 7.2?  Really, really weird.
uglyUnique :: IORef Int
uglyUnique = unsafePerformIO (newIORef 0)

-- | Unpack wrappers around primitive types, like 'Int'.
unpack1Instance :: Name -> Q [Dec]
unpack1Instance tycon = do
  TyConI dec <- reify tycon
  case dec of
    DataD cxt _ tyvars [con] _ -> unpacker1 cxt tycon tyvars con
    dec -> error ("Cannot unpack: " ++ show dec)

-- | Unpack complicated but single-constructor types.
unpackInstance :: Name -> Q [Dec]
unpackInstance tycon = do
  TyConI dec <- reify tycon
  case dec of
    DataD cxt _ tyvars [con] _ -> unpacker cxt tycon tyvars con
    NewtypeD cxt _ tyvars con _ -> unpacker cxt tycon tyvars con
    dec -> error ("Cannot unpack: " ++ show dec)

-- | Do no unpacking at all.
noUnpackInstance :: Name -> Q [Dec]
noUnpackInstance tycon = do
  TyConI dec <- reify tycon
  case dec of
    DataD cxt _ tyvars _ _ -> noUnpacker cxt tycon tyvars
    NewtypeD cxt _ tyvars _ _ -> noUnpacker cxt tycon tyvars
    dec -> error ("Cannot unpack: " ++ show dec)

conArgs :: Con -> (Name, [Type])
conArgs (NormalC conName args) = (conName, map snd args)
conArgs (RecC conName args) = (conName, [ty | (_, _, ty) <- args])
conArgs (InfixC (_, ty1) conName (_, ty2)) = (conName, [ty1, ty2])
conArgs _ = undefined

tyVarBndrName  :: TyVarBndr -> Name
tyVarBndrName (PlainTV var) = var
tyVarBndrName (KindedTV var _) = var

tupleInstance :: Int -> Q [Dec]
tupleInstance n = do
  argNames <- mapM (\ c -> newName [c]) (take n ['a'..])
  unpacker [ClassP ''Unpackable [VarT argName] | argName <- argNames]
    (tupleTypeName n)
    (map PlainTV argNames)
    (NormalC (tupleDataName n) [(NotStrict, VarT argName) | argName <- argNames])

getUnique :: Q Int
getUnique = runIO (atomicModifyIORef uglyUnique (\ n -> (n+1, n)))

unpacker1 :: Cxt -> Name -> [TyVarBndr] -> Con -> Q [Dec]
unpacker1 cxt tyCon tyArgs con = case conArgs con of
  (conName, conArgs) -> do
    argNames <- replicateM (length conArgs) (newName "arg")
    let theTy = foldl (\ t0 arg -> t0 `AppT` arg) (ConT tyCon) (map (VarT . tyVarBndrName) tyArgs)
    let inline = InlineSpec True False Nothing
    let pragmas =
	  [PragmaD $ InlineP (mkName "runUnpackedReaderT")
	    inline,
	  PragmaD $ InlineP (mkName "unpackedReaderT")
	    inline]
    u <- getUnique
    funcName <- newName $ "UnpackedReaderTCon" ++ show u
    mName <- newName "m"
    aName <- newName "z"
    fName <- newName "func"
    let decs = 
	  [NewtypeInstD [] ''UnpackedReaderT [theTy, VarT mName, VarT aName]
	    (NormalC funcName [(NotStrict, foldr (\ argTy result -> ArrowT `AppT` argTy `AppT` result)
		  (VarT mName `AppT` VarT aName) conArgs)]) []] ++ pragmas ++ [
	    FunD 'runUnpackedReaderT
	      [Clause [ConP funcName [VarP fName], ConP conName (map VarP argNames)]
		(NormalB (foldl AppE (VarE fName) (map VarE argNames))) []],
	    FunD 'unpackedReaderT
	      [Clause [VarP fName] (NormalB $ ConE funcName `AppE`
		LamE (map VarP argNames) (VarE fName `AppE` (foldl AppE (ConE conName) (map VarE argNames)))) []]]
    return [InstanceD cxt (ConT ''Unpackable `AppT` theTy) decs]

unpacker :: Cxt -> Name -> [TyVarBndr] -> Con -> Q [Dec]
unpacker cxt tyCon tyArgs con = case conArgs con of
  (conName, conArgs) -> do
    argNames <- replicateM (length conArgs) (newName "arg")
    let theTy = foldl (\ t0 arg -> t0 `AppT` arg) (ConT tyCon) (map (VarT . tyVarBndrName) tyArgs)
    let inline = InlineSpec True False Nothing
    let pragmas =
	  [PragmaD $ InlineP (mkName "runUnpackedReaderT")
	    inline,
	  PragmaD $ InlineP (mkName "unpackedReaderT")
	    inline]
    u <- getUnique
    funcName <- newName $ "UnpackedReaderT" ++ show u
    mName <- newName "m"
    aName <- newName "z"
    fName <- newName "func"
    let monadStack = foldr (\ argTy stk -> ConT ''UnpackedReaderT `AppT` argTy `AppT` stk)
	  (VarT mName) conArgs
    let decs = 
	  [NewtypeInstD [] ''UnpackedReaderT [theTy, VarT mName, VarT aName]
	    (NormalC funcName [(NotStrict, monadStack `AppT` VarT aName)]) []] ++ pragmas ++ [
	    FunD 'runUnpackedReaderT
	      [Clause [ConP funcName [VarP fName], ConP conName (map VarP argNames)]
		(NormalB (foldl (\ func arg -> InfixE (Just func) (VarE 'runUnpackedReaderT)
				  (Just arg))
		(VarE fName) (map VarE argNames))) []],
	    FunD 'unpackedReaderT
	      [Clause [VarP fName] (NormalB $ ConE funcName `AppE`
		foldr (\ argName func -> VarE 'unpackedReaderT
		  `AppE` LamE [VarP argName] func)
		  (VarE fName `AppE` (foldl AppE (ConE conName) (map VarE argNames)))
		  argNames) []]]
    return [InstanceD cxt (ConT ''Unpackable `AppT` theTy) decs]

noUnpacker :: Cxt -> Name -> [TyVarBndr] -> Q [Dec]
noUnpacker cxt tyCon tyArgs = do
    argName <- newName "arg"
    let theTy = foldl (\ t0 arg -> t0 `AppT` arg) (ConT tyCon) (map (VarT . tyVarBndrName) tyArgs)
    let inline = InlineSpec True False Nothing
    let pragmas =
	  [PragmaD $ InlineP (mkName "runUnpackedReaderT")
	    inline,
	  PragmaD $ InlineP (mkName "unpackedReaderT")
	    inline]
    u <- getUnique
    funcName <- newName $ "UnpackedReaderT" ++ show u
    mName <- newName "m"
    aName <- newName "z"
    fName <- newName "func"
    let decs = 
	  [NewtypeInstD [] ''UnpackedReaderT [theTy, VarT mName, VarT aName]
	    (NormalC funcName [(NotStrict, ArrowT `AppT` theTy `AppT` (VarT mName `AppT` VarT aName))]) []] ++ pragmas ++ [
	    FunD 'runUnpackedReaderT
	      [Clause [ConP funcName [VarP fName], VarP argName]
		(NormalB (VarE fName `AppE` VarE argName)) []],
	    FunD 'unpackedReaderT
	      [Clause [VarP fName] (NormalB $ ConE funcName `AppE` VarE fName) []]]
    return [InstanceD cxt (ConT ''Unpackable `AppT` theTy) decs]
