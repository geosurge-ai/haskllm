import Data.List (isInfixOf)
import Distribution.Simple
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, getPermissions, setOwnerExecutable, setPermissions)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { postBuild = \args flags pkg lbi -> do
          postBuild simpleUserHooks args flags pkg lbi
          installGitHooks
          checkEditorConfig
      }

installGitHooks :: IO ()
installGitHooks = do
  let sourceHook = ".git-hooks" </> "pre-push"
      targetHook = ".git" </> "hooks" </> "pre-push"

  sourceExists <- doesFileExist sourceHook
  if not sourceExists
    then hPutStrLn stderr "⚠️  .git-hooks/pre-push not found, skipping hook installation"
    else do
      -- Make source hook executable first
      sourcePerms <- getPermissions sourceHook
      setPermissions sourceHook (setOwnerExecutable True sourcePerms)

      createDirectoryIfMissing True (".git" </> "hooks")
      copyFile sourceHook targetHook

      -- Make target executable
      targetPerms <- getPermissions targetHook
      setPermissions targetHook (setOwnerExecutable True targetPerms)

      putStrLn "✅ Installed git pre-push hook"

checkEditorConfig :: IO ()
checkEditorConfig = do
  -- Check VS Code
  vscodeExists <- doesFileExist (".vscode" </> "settings.json")
  if vscodeExists
    then do
      content <- readFile (".vscode" </> "settings.json")
      let hasFourmolu = "fourmolu" `isInfixOf` content
          hasFormatOnSave = "formatOnSave" `isInfixOf` content
          hasOrganizeImports = "organizeImports" `isInfixOf` content
      if hasFourmolu && hasFormatOnSave && hasOrganizeImports
        then putStrLn "✅ VS Code: fourmolu configured with format-on-save and organize imports"
        else hPutStrLn stderr "⚠️  VS Code: fourmolu not fully configured"
    else hPutStrLn stderr "ℹ️  VS Code: settings.json not found"
