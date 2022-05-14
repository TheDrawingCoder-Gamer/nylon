module Nylon.Vcs where  

import System.Process.Typed
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Data.Void (Void(..))
import Data.ByteString.Lazy.Char8 (unpack)
data VcsData = VcsData String String
newtype Git = Git VcsData
newtype Mercury = Hg VcsData

class Vcs vcs where 
    vcsClone  :: vcs -> String -> String -> Maybe String -> Maybe String -> IO () 
    vcsUpdate :: vcs -> Bool -> IO Bool

instance Vcs Git where 
    vcsClone (Git (VcsData name executable)) cloneTo cloneFrom branch version = do 
        oldCwd <- getCurrentDirectory
        exitCode <- runProcess $ proc executable [ "clone", cloneFrom, cloneTo, "--recursive"] 
        case exitCode of 
            ExitSuccess -> do 
                setCurrentDirectory cloneTo
                case (branch, version) of 
                    (_, Just v) -> do 
                        exitCheckout <- runProcess $ proc executable [ "checkout",  "tags/" <> v]
                        setCurrentDirectory oldCwd
                        case exitCheckout of 
                            ExitSuccess ->
                                pure ()
                            ExitFailure _ -> 
                                fail "Failed to checkout version"
                    (Just b, _) -> do 
                        exitBranch <- runProcess $ proc executable ["checkout", b] 
                        setCurrentDirectory oldCwd
                        case exitBranch of 
                            ExitSuccess ->
                                pure ()
                            ExitFailure _ -> 
                                fail "Failed to checkout branch"
                    _ ->
                        setCurrentDirectory oldCwd
            ExitFailure _ -> 
                fail "Failed to clone repo"
    vcsUpdate (Git (VcsData _ executable)) confirm = do 
        exitNoCache <- runProcess $ proc executable ["diff", "--exit-code", "--no-ext-diff"] 
        exitCache   <- runProcess $ proc executable ["diff", "--cached", "--exit-code", "--no-ext-diff"]
        case (exitNoCache, exitCache) of 
            (ExitSuccess, ExitSuccess) -> 
                pure () 
            _ -> 
                if confirm then 
                    runProcess_ $ proc executable ["reset", "--hard"]
                else do
                    cwd <- getCurrentDirectory
                    fail $ "Update in " <> cwd <> "failed due to dirty tree"
        runProcess_ $ proc executable ["fetch"] 
        (exit, revU, _) <- readProcess $ proc executable ["rev-parse", "@{u}"] 
        (_,    revHEAD,_) <- readProcess $ proc executable ["rev-parse", "HEAD"]
        if revU == revHEAD then 
            pure False
        else do
            case exit of 
                ExitSuccess -> 
                    pure ()
                ExitFailure _ -> do
                    (_, raw)  <- readProcessStdout $ proc executable [ "show-branch" ] 
                    let branch = parseMaybe parseGitBranch $ unpack raw
                    let branch' = case branch of 
                                    Just b -> 
                                        b
                                    _ -> unpack raw
                    runProcess_ $ proc executable ["checkout", branch', "--force"]
            runProcess_ $ proc executable ["merge"]
            pure True
                            
                    


parseGitBranch ::  Parsec Void String String
parseGitBranch = do 
    skipManyTill (satisfy (const True)) $ do
        char '['
        whatIWant <-manyTill (satisfy (const True)) (char ']')
        _ <- many (satisfy (const True))
        pure whatIWant

        
            
            
    

                
