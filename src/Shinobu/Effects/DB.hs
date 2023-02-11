module Shinobu.Effects.DB
  ( module Shinobu.Effects.DB,
    module Database.SQLite.Simple.Interpolate,
  )
where

import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple.Interpolate (isql)
import qualified Polysemy as P
import Shinobu.Utils.Error

-- TODO use smth like (Reader Connection) everywhere and wrap locked blocks in withConnection somehow

type IQuery = (SQL.Query, [SQL.SQLData])

data DB :: P.Effect where
  Query__ :: SQL.FromRow a => IQuery -> DB n [a]
  Execute :: IQuery -> DB n ()

P.makeSem ''DB

-- more convenient type application
query :: forall a r. (DB :> r, SQL.FromRow a) => IQuery -> P.Sem r [a]
query = query__

queryMap :: forall a b r. (DB :> r, SQL.FromRow a) => (a -> P.Sem r b) -> IQuery -> P.Sem r [b]
queryMap f = mapM f <=< query

queryVia :: forall a b r. (DB :> r, SQL.FromRow a, TryFromP a b r) => IQuery -> P.Sem r [b]
queryVia = queryMap @a tryFromP

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
