module Turbinado.Environment.CodeStore (
    addCodeStoreToEnvironment,
    retrieveCode,
    ) where
import Control.Monad.State
import Control.Concurrent.MVar
import Control.Exception ( catch, throwIO)
import Control.Monad ( when, foldM)
import Data.Map hiding (map)
import Data.List (isPrefixOf, intersperse)
import Data.Maybe
import Data.Typeable
import qualified Network.HTTP as HTTP
import Prelude hiding (lookup,catch)
import System.Directory
import System.FilePath
import System.IO

import GHC
import GHC.Paths
import DynFlags
import Unsafe.Coerce

-- import System.Plugins
-- import System.Plugins.Utils
import System.Time

import Config.Master

import qualified Turbinado.Server.Exception as Ex
import Turbinado.Environment.Logger
import Turbinado.Environment.Types
import Turbinado.Environment.Request
import Turbinado.Environment.Response
import Turbinado.View.Monad hiding (liftIO)
import Turbinado.View.XML
import Turbinado.Controller.Monad

-- | Create a new store for Code data
addCodeStoreToEnvironment :: (HasEnvironment m) => m ()
addCodeStoreToEnvironment = do e <- getEnvironment
                               mv <- liftIO $ newMVar $ empty
                               setEnvironment $ e {getCodeStore = Just $ CodeStore mv}

retrieveCode :: (HasEnvironment m) => CodeType -> CodeLocation -> m CodeStatus
retrieveCode ct cl' = do
    e <- getEnvironment
    let (CodeStore mv) = fromJust $ getCodeStore e
        path  = getDir ct
    cl <- do -- d <- getCurrentDirectory 
          return (addExtension (joinPath $ map normalise [{- d, -} path, dropExtension $ fst cl']) "hs", snd cl')
    debugM $ "  CodeStore : retrieveCode : loading   " ++ (fst cl) ++ " - " ++ (snd cl)
    cmap <- liftIO $ takeMVar mv
    let c= lookup cl cmap
    cmap' <- case c of
               Nothing                  -> do debugM ((fst cl) ++ " : " ++ (snd cl) ++ " : fresh load")
                                              loadCode ct cmap cl
               Just (CodeLoadFailure _) -> do debugM ((fst cl) ++ " : " ++ (snd cl) ++ " : previous failure; try load") 
                                              loadCode ct cmap cl
               _                        -> do debugM ((fst cl) ++ " : " ++ (snd cl) ++ " : checking reload") 
                                              checkReloadCode ct cmap (fromJust c) cl
    liftIO $ putMVar mv cmap'
    -- We _definitely_ have a code entry now, though it may have a MakeFailure
    let c' = lookup cl cmap'
    case c' of
        Nothing                             -> do debugM (fst cl ++ " : Not found in CodeStore") 
                                                  return  CodeLoadMissing
        Just CodeLoadMissing                -> do debugM (fst cl ++ " : Not found in CodeStore") 
                                                  return  CodeLoadMissing
        Just (CodeLoadFailure e)            -> do debugM (fst cl ++ " : CodeLoadFailure " ) 
                                                  return (CodeLoadFailure e)
        Just clc@(CodeLoadController _ _ _) -> do debugM (fst cl ++ " : CodeLoadController " ) 
                                                  return clc  
        Just clv@(CodeLoadView       _ _ _) -> do debugM (fst cl ++ " : CodeLoadView" ) 
                                                  return clv
        Just clc@(CodeLoadComponentController _ _ _) -> do debugM (fst cl ++ " : CodeLoadComponentController " ) 
                                                           return clc  
        Just clv@(CodeLoadComponentView       _ _ _) -> do debugM (fst cl ++ " : CodeLoadComponentView" ) 
                                                           return clv
        
checkReloadCode :: (HasEnvironment m) => CodeType -> CodeMap -> CodeStatus -> CodeLocation -> m CodeMap
checkReloadCode ct cmap (CodeLoadFailure e) cl = error "ERROR: checkReloadCode was called with a CodeLoadFailure"
checkReloadCode ct cmap cstat cl = do
    debugM $ "    CodeStore : checkReloadCode : loading   " ++ (fst cl) ++ " - " ++ (snd cl)
    (exists, reload) <- needReloadCode (fst cl) (getDate cstat)
    case (exists, reload) of
        (False, _)    -> do debugM $ "    CodeStore : checkReloadCode : Code missing"
                            return $ insert cl CodeLoadMissing cmap
        (True, False) -> do debugM $ "    CodeStore : checkReloadCode : No reload neeeded"
                            return cmap
        (True, True)  -> do debugM $ "    CodeStore : checkReloadCode : Need reload"
                            loadCode ct cmap cl

        
-- The beast
-- In cases of Merge, Make or Load failures leave the original files in place and log the error
loadCode :: (HasEnvironment m) => CodeType -> CodeMap -> CodeLocation -> m CodeMap
loadCode ct cmap cl = do
    debugM $ "\tCodeStore : loadCode : loading   " ++ (fst cl) ++ " - " ++ (snd cl)
    fe <- liftIO $ doesFileExist $ fst cl
    case fe of 
        False -> debugM ("\tFile not found: " ++ fst cl) >> return cmap 
        True  -> mergeCode ct cmap cl
        
mergeCode :: (HasEnvironment m) => CodeType -> CodeMap -> CodeLocation -> m CodeMap
mergeCode ct cmap cl = do
    debugM $ "\tDummy Merging " ++ (fst cl)
    -- d <- getCurrentDirectory
    --debugM $ "  stub " ++ joinPath [normalise d, normalise $ getStub ct]
    makeCode ct cmap cl ""
    -- ms <- customMergeToDir (joinPath [{-normalise d,-} normalise $ getStub ct]) (fst cl) compiledDir
    -- case ms of
    --     MergeFailure err            -> do debugM ("\tMerge error : " ++ (show err))
    --                                       return $ insert cl (CodeLoadFailure $ unlines err) cmap
    --     MergeSuccess NotReq _    _  -> do debugM ("\tMerge success (No recompilation required) : " ++ (fst cl)) 
    --                                       return cmap
    --     MergeSuccess _      args fp -> do debugM ("\tMerge success : " ++ (fst cl)) 
    --                                      makeCode ct cmap cl args fp
        
--makeCode :: (HasEnvironment m) => CodeType -> CodeMap -> CodeLocation -> [Arg] -> FilePath -> m CodeMap
makeCode :: (HasEnvironment m) => CodeType -> CodeMap -> CodeLocation -> FilePath -> m CodeMap
makeCode ct cmap cl fp = do
    -- ms <- liftIO $ makeAll fp (compileArgs++args)
    debugM ("\tDummy Making: " ++ (fp)) 
    case ct of
      CTLayout                -> _loadView ct cmap cl fp
      CTView                  -> _loadView ct cmap cl fp
      CTComponentView         -> _loadView ct cmap cl fp
      CTController            -> _loadController ct cmap cl fp
      CTComponentController   -> _loadController ct cmap cl fp


_loadView :: (HasEnvironment m) => CodeType -> CodeMap -> CodeLocation -> FilePath -> m CodeMap
_loadView ct cmap cl fp = do
    debugM ("_load : " ++ (show ct) ++ " : " ++ (fst cl) ++ " : " ++ (snd cl))
    error $ "Not implemented yet"
    -- here goes the ghc api call 
    -- ls <- liftIO $ load_ fp [compiledDir] (snd cl)
    -- case ct of
    --   CTLayout              -> return (insert cl (CodeLoadView f m t) cmap)
    --   CTView                -> return (insert cl (CodeLoadView f m t) cmap)
    --   CTComponentView       -> return (insert cl (CodeLoadComponentView f m t) cmap)
    --   _                     -> error $ "_loadView: passed an invalid CodeType (" ++ (show ct) ++ ")"


    -- case ls of 
    --     LoadFailure err -> do debugM ("LoadFailure : " ++ (show err)) 
    --                           return (insert cl (CodeLoadFailure $ unlines err) cmap)
    --     LoadSuccess m f -> do debugM ("LoadSuccess : " ++ fst cl )
    --                           liftIO $ unload m
    --                           t <- liftIO $ getClockTime
    --                           case ct of
    --                             CTLayout              -> return (insert cl (CodeLoadView f m t) cmap)
    --                             CTView                -> return (insert cl (CodeLoadView f m t) cmap)
    --                             CTComponentView       -> return (insert cl (CodeLoadComponentView f m t) cmap)
    --                             _                     -> error $ "_loadView: passed an invalid CodeType (" ++ (show ct) ++ ")"

_loadController :: (HasEnvironment m) => CodeType -> CodeMap -> CodeLocation -> FilePath -> m CodeMap
_loadController ct cmap cl fp = do
    debugM ("_load : " ++ (show ct) ++ " : " ++ (fst cl) ++ " : " ++ (snd cl))
    (f,m) <- (liftIO $ load_ (fst cl) (snd cl)) 
    t <- liftIO $ getClockTime          
    case ct of
       CTController          -> return (insert cl (CodeLoadController f m t) cmap)
       CTComponentController -> return (insert cl (CodeLoadComponentController f m t) cmap)
       _                     -> error $ "_loadController: passed an invalid CodeType (" ++ (show ct) ++ ")"
    
    
    -- ls <- liftIO $ load_ fp [compiledDir] (snd cl)
    -- case ls of 
    --     LoadFailure err -> do debugM ("LoadFailure : " ++ (show err)) 
    --                           return (insert cl (CodeLoadFailure $ unlines err) cmap)
    --     LoadSuccess m f -> do debugM ("LoadSuccess : " ++ fst cl )
    --                           liftIO $ unload m
    --                           t <- liftIO $ getClockTime
    --                           case ct of
    --                             CTController          -> return (insert cl (CodeLoadController f m t) cmap)
    --                             CTComponentController -> return (insert cl (CodeLoadComponentController f m t) cmap)
    --                             _                     -> error $ "_loadController: passed an invalid CodeType (" ++ (show ct) ++ ")"
                              

-------------------------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------------------------

--- Loads the module from the given path and given name 
--- within the GhcMonad.
--                       Path      Module   
moduleM :: GhcMonad m => String -> String -> m Module
moduleM p mod = do 
  dflags <- getSessionDynFlags
  setSessionDynFlags dflags
  target <- guessTarget p Nothing
  addTarget target
  r <- load LoadAllTargets
  case r of
    Failed -> error "Compilation failed"
    Succeeded -> do
           findModule (mkModuleName mod) Nothing

--- Loads the function in the given module 
--- within the GhcMonad.
--- Any alternative to unsafeCoerce?
functionByNameM :: GhcMonad m => String -> Module -> m (a)
functionByNameM name mod = do
  setContext [] [mod]
  f <- compileExpr (name)
  do let value' = (unsafeCoerce f) :: a
     return value'

load_ :: FilePath -> String -> IO (a,Module)
load_ fp name = 
    defaultErrorHandler defaultDynFlags $ do
       func <- runGhc (Just libdir) $ do 
                 m <- moduleM fp (dropExtension . takeFileName $ fp) -- The module
                 f <- (functionByNameM name m )::(GhcMonad m => m a) -- The function
                 return (f,m)
       return func 

-- We won't be using this.
-- -- Custom merge function because I don't want to have to use a custom
-- -- version of Plugins (with HSX enabled)
-- customMergeToDir :: (HasEnvironment m) => FilePath -> FilePath -> FilePath -> m MergeStatus
-- customMergeToDir stb src dir = do
--     src_exists <- liftIO $ doesFileExist src
--     stb_exists <- liftIO $ doesFileExist stb
--     debugM  $ "\tMerging': " ++ (show [dir, makeRelative rootDir src])
--     let src' = makeRelative rootDir src
--         outFile = joinPath [combine rootDir dir, src']
--         outDir  = joinPath $ init $ splitDirectories outFile
--         outMod  = concat $ intersperse "." $ splitDirectories $ dropExtension src'
--         outTitle = "module " ++ outMod ++ " where \n\n"    
--     case (src_exists, stb_exists) of
--         (False, _) -> return $ 
--                 MergeFailure ["Source file does not exist : "++src]
--         (_, False) -> return $ 
--                 MergeFailure ["Source file does not exist : "++stb]
--         _          -> do
--                 src_str <- liftIO $ readFile src
--                 stb_str <- liftIO $ readFile stb
--                 let (stbimps, stbdecls) = span ( not . isPrefixOf "-- SPLIT HERE") $ lines stb_str
--                     mrg_str = outTitle ++ (unlines stbimps) ++ src_str ++ (unlines stbdecls)
--                 liftIO $ createDirectoryIfMissing True outDir
--                 hdl <- liftIO $ openFile outFile WriteMode  -- overwrite!
--                 liftIO $ hPutStr hdl mrg_str 
--                 liftIO $ hClose hdl
--                 return $ MergeSuccess ReComp [] outFile -- must have recreated file
 

needReloadCode :: (HasEnvironment m) => FilePath -> CodeDate -> m (Bool, Bool)
needReloadCode fp fd = do
    fe <- liftIO $ doesFileExist fp
    case fe of
        True -> do mt <- liftIO $ getModificationTime fp    
                   return $ (True, mt > fd)
        False-> return (False, True)

getDir :: CodeType -> FilePath
getDir ct = 
    let dir = case ct of
                CTLayout         -> layoutDir
                CTController     -> controllerDir
                CTView           -> viewDir
                CTComponentController -> componentControllerDir
                CTComponentView       -> componentViewDir
    in combine rootDir dir  

getStub :: CodeType -> FilePath
getStub ct = case ct of
  CTLayout         -> layoutStub
  CTController     -> controllerStub
  CTView           -> viewStub
  CTComponentController -> controllerStub
  CTComponentView       -> viewStub

getDate (CodeLoadMissing) = error "getDate called with CodeLoadMissing"
getDate (CodeLoadFailure e) = error "getDate called with CodeLoadFailure"
getDate (CodeLoadView       _ _ d) = d
getDate (CodeLoadController _ _ d) = d
getDate (CodeLoadComponentView       _ _ d) = d
getDate (CodeLoadComponentController _ _ d) = d
