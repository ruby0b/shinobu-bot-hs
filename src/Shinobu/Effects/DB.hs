module Shinobu.Effects.DB where

import qualified Database.SQLite.Simple as SQL
import qualified Polysemy as P

-- TODO use smth like (Reader Connection) everywhere and wrap locked blocks in withConnection somehow

type IQuery = (SQL.Query, [SQL.SQLData])

data DB :: P.Effect where
  Query__ :: SQL.FromRow a => IQuery -> DB n [a]
  Execute :: IQuery -> DB n ()

P.makeSem ''DB

-- more convenient type application
query :: forall a r. (DB :> r, SQL.FromRow a) => IQuery -> P.Sem r [a]
query = query__

query_ :: forall a r. (DB :> r, SQL.FromRow a) => SQL.Query -> P.Sem r [a]
query_ = query . (,[])

execute_ :: DB :> r => SQL.Query -> P.Sem r ()
execute_ = execute . (,[])

runSqliteSimple :: P.Embed IO :> r => String -> P.Sem (DB : r) a -> P.Sem r a
runSqliteSimple dbPath = P.interpret \case
  Query__ (query', data') -> P.embed $
    SQL.withConnection dbPath \connection ->
      SQL.query connection query' data'
  Execute (query', data') -> P.embed $
    SQL.withConnection dbPath \connection ->
      SQL.execute connection query' data'
