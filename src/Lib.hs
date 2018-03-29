module Lib
    ( runApp
    ) where

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Backend.SQL.Types (SqlSerial)

import Data.Text (Text)

import qualified Control.Exception as E

-- users table
data UserT f 
    = User 
    { _userId :: Columnar f (SqlSerial Int)
    , _userEmail :: Columnar f Text 
    } deriving Generic

type User = UserT Identity
deriving instance Show User
deriving instance Eq User

instance Table UserT where
    data PrimaryKey UserT f = UserId (Columnar f (SqlSerial Int)) deriving Generic
    primaryKey = UserId . _userId

type UserId = PrimaryKey UserT Identity
deriving instance Show UserId
deriving instance Eq UserId

instance Beamable (PrimaryKey UserT)
instance Beamable UserT
-- end users table

-- database
newtype MyDb f 
    = MyDb 
    { _users :: f (TableEntity UserT) 
    } deriving Generic
instance Database Postgres MyDb

myDb :: DatabaseSettings Postgres MyDb
myDb = defaultDbSettings
-- end database


runApp :: IO ()
runApp = 
    E.bracket (connectPostgreSQL "dbname=std_ableton_com") close $ \conn -> do
        let allUsers = all_ (_users myDb)
        withDatabaseDebug putStrLn conn $ do
            users <- runSelectReturningList $ select allUsers
            mapM_ (liftIO . print . show) users
