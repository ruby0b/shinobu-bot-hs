module Shinobu.Effects.DB where

import qualified Database.SQLite.Simple as SQL
import qualified Polysemy as P

data DB m c :: P.Effect where
  Run :: (c -> m a) -> DB m c n a

P.makeSem ''DB

-- TODO use smth like (Reader Connection) everywhere and wrap locked blocks in withConnection somehow

type SQLite = DB IO SQL.Connection

runSqliteSimple :: P.Embed IO :> r => String -> P.Sem (DB IO SQL.Connection : r) a -> P.Sem r a
runSqliteSimple dbPath = P.interpret \case
  Run f -> P.embed $ SQL.withConnection dbPath f
