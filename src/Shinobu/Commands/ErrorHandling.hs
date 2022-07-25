module Shinobu.Commands.ErrorHandling where

import Calamity
import Calamity.Commands
import Calamity.Commands.Context
import qualified DiPolysemy as P
import Shinobu.Utils.Misc
import Shinobu.Utils.Types (ShinobuSem)

tellErrors :: ShinobuSem r
tellErrors = void $
  react @('CustomEvt (CtxCommandError FullContext)) $ \(CtxCommandError ctx e) -> void $ do
    P.info $ "Command failed with reason: " <> show e
    tellError ctx case e of
      ParseError n r ->
        "Failed to parse parameter "
          <> codeline (from n)
          <> ": "
          <> codeblock' Nothing r
      CheckError n r ->
        "The check "
          <> codeline (from n)
          <> " failed: "
          <> codeblock' Nothing r
      InvokeError n r ->
        "The command "
          <> codeline (from n)
          <> " failed: "
          <> codeblock' Nothing r
