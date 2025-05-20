{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module DBase 
    ( connect_DB,
      valid,
      init_DB,
      add_reminder,
      update_reminder,
      delete_reminder,
      show_reminders,
      print_reminder,
      get_id
    ) where

import Database.HDBC (IConnection(disconnect, run, commit), toSql, quickQuery', fromSql, SqlValue)
import Database.HDBC.Sqlite3 (connectSqlite3, Connection)
import qualified Data.Text as T
import Data.Time ( UTCTime, fromGregorianValid, TimeOfDay (TimeOfDay))
import Data.Maybe (isJust)

import Data.Text.Encoding (decodeUtf8, encodeUtf8) -- поддержка русского языка в выводе сообщений


-- для удобства подключения к базе данных
connect_DB :: IO Connection
connect_DB = connectSqlite3 "dbase.db"

-- проверка на валидность данных 
valid :: Integer -> Int -> Int -> Int -> Int -> Bool
valid year month day hour minute = 
    isJust (fromGregorianValid year month day) && hour `elem` [0..23] && minute `elem` [0..59]

-- инициализация базы данных
init_DB :: IO ()
init_DB = do
    conn <- connect_DB
    run conn
        "CREATE TABLE IF NOT EXISTS dbase (\
        \ID INTEGER PRIMARY KEY AUTOINCREMENT, \
        \ID_TG INTEGER NOT NULL, \
        \Message TEXT NOT NULL, \
        \Year INTEGER NOT NULL, \
        \Month INTEGER NOT NULL, \
        \Day INTEGER NOT NULL, \
        \Hour INTEGER NOT NULL, \
        \Minute INTEGER NOT NULL)"
        []
    commit conn
    disconnect conn

-- добавление напоминания
add_reminder :: String -> T.Text -> Integer -> Int -> Int -> Int -> Int -> IO ()
add_reminder id_tg message year month day hour minute = do
    conn <- connect_DB
    run conn
        "INSERT INTO dbase (ID_TG, Message, Year, Month, Day, Hour, Minute) VALUES (?, ?, ?, ?, ?, ?, ?)"
        [toSql id_tg, toSql (encodeUtf8 message), toSql year, toSql month, toSql day, toSql hour, toSql minute]
    commit conn
    disconnect conn

-- обновление даты напоминания
update_reminder :: Int -> Integer -> Int -> Int -> Int -> Int -> IO()
update_reminder id new_year new_month new_day new_hour new_minute = do
    conn <- connect_DB
    run conn "UPDATE dbase SET Year = ?, Month = ?, Day = ?, Hour = ?, Minute = ? WHERE ID = ?"
            [toSql new_year, toSql new_month, toSql new_day, toSql new_hour, toSql new_minute, toSql id]
    commit conn
    disconnect conn

-- удаление напоминания
delete_reminder :: Int -> IO ()
delete_reminder id = do
    conn <- connect_DB
    run conn "DELETE FROM dbase WHERE ID = ?" [toSql id]
    commit conn
    disconnect conn

-- показ всех напоминаний пользователя
show_reminders :: String -> IO T.Text
show_reminders id_tg = do
    conn <- connect_DB
    rows <- quickQuery' conn "SELECT ID, Message, Year, Month, Day, Hour, Minute FROM dbase WHERE ID_TG = ?" [toSql id_tg]
    commit conn
    disconnect conn

    if null rows == False
    then return $ print_reminder rows
    else return $ T.pack "К сожалению, у вас нет ни одного напоминания :( Добавьте его, скорее!"

-- вывод напоминаний
print_reminder :: [[SqlValue]] -> T.Text
print_reminder rows = T.pack $ concat
    [ "[" ++ show (fromSql sqlId :: Int) ++ "]  \""
      ++ (T.unpack . decodeUtf8 $ fromSql sqlMessage) ++ "\"    "
      ++ show (fromSql sqlYear :: Integer) ++ "-"
      ++ show (fromSql sqlMonth :: Int) ++ "-"
      ++ show (fromSql sqlDay :: Int) ++ "    "
      ++ show (fromSql sqlHour :: Int) ++ ":"
      ++ show (fromSql sqlMinute :: Int) ++ "\n"
      | [sqlId, sqlMessage, sqlYear, sqlMonth, sqlDay, sqlHour, sqlMinute] <- rows ] 

-- вытаскивать все id задач для проверки
get_id :: IO [Int]
get_id = do
  conn <- connect_DB
  rows <- quickQuery' conn "SELECT ID FROM dbase" []
  disconnect conn

  return $ map fromSql $ concat rows
