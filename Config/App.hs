module Config.App (
  applicationPath,
  applicationHost,
  AppEnvironment (..),
  newAppEnvironment,
  databaseConnection,
  Connection,
  customPreFilters,
  customPostFilters,
  logLevel
  ) where

import System.Log.Logger

-- Your favorite HDBC driver
import Database.HDBC.PostgreSQL
import Database.HDBC

----------------------------------------------------------------
-- Environment settings
----------------------------------------------------------------
applicationPath = ""
applicationHost = "localhost:8080"

data AppEnvironment = AppEnvironment
newAppEnvironment = AppEnvironment

----------------------------------------------------------------
-- Database connection
----------------------------------------------------------------
databaseConnection :: Maybe (IO Connection)
-- databaseConnection = Nothing
databaseConnection = Just $ handleSqlError 
                     $ connectPostgreSQL "host=localhost dbname=turbinado user=postgres password=12457890"

----------------------------------------------------------------
-- RequestHandler Filter List additions
----------------------------------------------------------------
customPreFilters  = []
customPostFilters = []


----------------------------------------------------------------
-- Logging
----------------------------------------------------------------
logLevel = DEBUG -- DEBUG < INFO < NOTICE < WARNING < ERROR < CRITICAL < ALERT < EMERGENCY 


