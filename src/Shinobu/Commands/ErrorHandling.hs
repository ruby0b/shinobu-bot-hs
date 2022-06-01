module Shinobu.Commands.ErrorHandling where

import Calamity
import Calamity.Commands
import Calamity.Commands.Context
import qualified DiPolysemy as P
import Shinobu.Types (ShinobuSem)

tellErrors :: ShinobuSem r
tellErrors = void $
  react @('CustomEvt (CtxCommandError FullContext)) $ \(CtxCommandError ctx e) -> void $ do
    P.info $ "Command failed with reason: " <> show e
    tell ctx case e of
      ParseError n r ->
        "Failed to parse parameter: "
          <> codeline (from n)
          <> ", with reason: "
          <> codeblock' Nothing r
      CheckError n r ->
        "The following check failed: "
          <> codeline (from n)
          <> ", with reason: "
          <> codeblock' Nothing r
      InvokeError n r ->
        "The command: "
          <> codeline (from n)
          <> ", failed with reason: "
          <> codeblock' Nothing r
