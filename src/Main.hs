{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant ==" #-}
{-# HLINT ignore "Use camelCase" #-}

import Telegram.Bot.API (Update(updateMessage), messageText, messageFrom, User, userId, UserId (UserId))
import Telegram.Bot.API.MakingRequests (defaultTelegramClientEnv, Token(Token))
import Telegram.Bot.Simple.UpdateParser (command, parseUpdate, text)
import Telegram.Bot.Simple.Debug (traceBotDefault)
import Telegram.Bot.Simple.BotApp (startBot_)
import Telegram.Bot.Simple
    ( getEnvToken,
      replyText,
      BotApp(BotApp, botHandler, botInitialModel, botAction, botJobs),
      BotJob(BotJob),
      botJobSchedule,
      botJobTask,
      Eff, withEffect)

import qualified Data.Text as T
import Data.Time (fromGregorian, getCurrentTime, timeOfDayToTime, UTCTime(UTCTime), TimeOfDay (TimeOfDay))
import Data.List (elem)

import Control.Monad.IO.Class (liftIO)
import System.IO.Unsafe (unsafePerformIO)

import Database.HDBC (IConnection(disconnect, run, commit), toSql, quickQuery', fromSql, SqlValue)
import DBase (add_reminder, update_reminder, delete_reminder, show_reminders, init_DB, connect_DB, valid, get_id)

                                      -- НЕКОТОРЫЕ ОШИБКИ МОЖНО ИГНОРИРОВАТЬ


-- структура для модели
data Model = Model
  { todoItems :: [(T.Text, UTCTime)], -- кортеж из задачи и времени
    userID    :: Maybe UserId         -- идентификатор пользователя
  } deriving (Show)

-- действия для бота
data Action
  = DoNothing                                -- ничего не делает
  | AddItem T.Text Integer Int Int Int Int   -- добавление напоминания: сообщение, год, месяц, день, час, минута
  | ShowItems                                -- показ всех напоминаний пользователя
  | RemoveItem Int                           -- удаление напоминания: ID напоминания
  | ChangeItem Int Integer Int Int Int Int   -- изменение напоминания: ID напоминания, год, месяц, день, час, минута
  | Start Update                             -- команда /start: начало работы с ботом
  | Error                                    -- ошибка при вводе
     deriving (Show)

-- сообщение при старте
startMessage :: T.Text
startMessage =
  T.unlines
    [ T.pack "Привет! Я бот-напоминалка! ^-^",
      T.pack "Напиши мне задачу, чтобы я запомнил и добавил в твой список дел!",
      T.pack "",
      T.pack "Если хочешь добавить напоминание, введи сообщение в формате:      Message. YYYY-MM-DD HH:MM",
      T.pack "",
      T.pack " - Команда /remove <id задачи> удаляет выбранное напоминание по его номеру.",
      T.pack " - Команда /change <id задачи> <новая дата> <новое время> помогает изменить время напоминания по его номеру.",
      T.pack " - Команда /show выводит все твои напоминания.",
      T.pack "",
      T.pack "Начни прямо сейчас! :3"
    ]

-- обработчик обновлений
handleUpdate :: Model -> Update -> Maybe Action
handleUpdate model update =
  case updateMessage update of
    Just msg -> Just (processMessage (messageText msg) update model)    -- обработка сообщения
    Nothing  -> Nothing                                                 -- буквально ничего :)

-- обработка команд
processMessage :: Maybe T.Text -> Update -> Model -> Action
processMessage Nothing _ model = DoNothing
processMessage (Just message) update model =
  case T.words message of

    -- команды без данных от пользователя
    [cmd] | cmd == T.pack "/start"  -> Start update
          | cmd == T.pack "/show"   -> ShowItems

    -- команда и номер напоминания для изменения
    [cmd, taskIdStr]
      | cmd == T.pack "/remove" ->
          case read (T.unpack taskIdStr) :: Int of
            task_id -> RemoveItem task_id
            _             -> Error

    -- команда, номер напоминания, дата, время
    [cmd, task_id, date, time]
      | cmd == T.pack "/change" ->
          case T.splitOn (T.pack "-") date of
            [yearStr, monthStr, dayStr] ->
              case T.splitOn (T.pack ":") time of
                [hourStr, minuteStr] -> let year   = read (T.unpack yearStr)
                                            month  = read (T.unpack monthStr)
                                            day    = read (T.unpack dayStr)
                                            hour   = read (T.unpack hourStr)
                                            minute = read (T.unpack minuteStr)
                                            in ChangeItem (read (T.unpack task_id) :: Int) year month day hour minute
                _ -> Error
            _ -> Error

    -- парсинг сообщения и добавление напоминания
    _ -> case T.splitOn (T.pack ".") message of
      (taskText : dateStr : _) ->
        case T.splitOn (T.pack " ") (T.strip dateStr) of
        (datePart : timePart : _) ->
            case T.splitOn (T.pack "-") datePart of
            [yearStr, monthStr, dayStr] ->
              case T.splitOn (T.pack ":") timePart of
                [hourStr, minuteStr] -> let year   = read (T.unpack yearStr)
                                            month  = read (T.unpack monthStr)
                                            day    = read (T.unpack dayStr)
                                            hour   = read (T.unpack hourStr)
                                            minute = read (T.unpack minuteStr)
                                            in AddItem taskText year month day hour minute
                _ -> Error
            _ -> Error
        _ -> Error
      _ -> Error

    -- снова ничего :)
    _ -> Error

-- конвертирует Int-ы в UTCTime
makeUTCTime :: Integer -> Int -> Int -> Int -> Int -> UTCTime
makeUTCTime year month day hour minute = UTCTime date timeOfDay
  where
    date = fromGregorian year month day
    timeOfDay = timeOfDayToTime $ TimeOfDay hour minute 0  -- секунды = 0

-- проверка на правильно введенное время (не раньше, чем уже есть сейчас)
isCorrectTime :: Integer -> Int -> Int -> Int -> Int -> IO Bool
isCorrectTime year month day hour minute = do
        current_time <- getCurrentTime
        return $ (makeUTCTime year month day hour minute) > current_time

-- обработчик действий
handleAction :: Action -> Model -> Eff Action Model
handleAction action model =
  case action of

    DoNothing -> pure model

    -- команда /start, начало работы
    Start update -> do
      case updateMessage update of
        Just msg -> case messageFrom msg of    -- вытаскиваем tg-id из сообщения пользователя
          Just user -> do

            let uSERID = userId user --id пользователя
            let newModel = model { userID = Just uSERID }

            -- отправка приветственного сообщения
            withEffect (replyText startMessage) newModel
            pure newModel

          Nothing -> pure model
        Nothing -> pure model

    -- добавление задачи
    AddItem task year month day hour minute -> do
      case userID model of
        Just uSERID -> do

          -- проверка, является ли время правильным
          if (unsafePerformIO (isCorrectTime year month day hour minute)) == False || (valid year month day hour minute) == False
          then do      -- если неправильное время
            withEffect (replyText (T.pack "Несоответствующее время :( Попробуй еще раз!")) model
            pure model

          else do      -- если правильное
            -- добавление напоминания в базу данных
            withEffect (liftIO $ add_reminder (show uSERID) task year month day hour minute) model
            withEffect (replyText (T.pack "Напоминание добавлено! ^-^")) model
            pure model

        Nothing -> pure model

    -- вывод всех задач пользователя
    ShowItems -> do
      case userID model of
        Just uSERID -> do

          -- вывод напоминаний из базы данных по tg-id пользователя
          withEffect (replyText (unsafePerformIO $ show_reminders (show uSERID))) model
          pure model

        Nothing -> pure model

    -- удаление задачи
    RemoveItem task_id -> do
      -- проверка на существующий номер задачи
      if task_id >= 1 && (task_id `elem` (unsafePerformIO get_id)) == True

      then do -- если напоминание существует
              withEffect
                (liftIO $ delete_reminder task_id) model  -- удаление напоминания из базы данных
              withEffect
                (replyText (T.pack "Напоминание удалено! ^-^")) model
              pure model

      else do -- если нет
              withEffect
                (replyText (T.pack "Такого напоминания нет :( Используйте /show, чтобы увидеть созданные задачи!")) model
              pure model

    -- изменение времени задачи
    ChangeItem task_id year month day hour minute -> do
      -- проверка на существующий номер задачи
      if task_id >= 1 && (task_id `elem` (unsafePerformIO $ get_id)) == True

      then do -- если напоминание существует
        -- проверка, является ли время правильным
        if (unsafePerformIO (isCorrectTime year month day hour minute)) == False || (valid year month day hour minute) == False

        then do   -- если неправильное время
          withEffect (replyText (T.pack "Несоответствующее время :( Попробуй еще раз!")) model
          pure model

        else do   -- если правильное время
          withEffect (liftIO $ update_reminder task_id year month day hour minute) model -- изменение в базе данных
          withEffect (replyText (T.pack "Напоминание изменено! ^-^")) model
          pure model

      else do -- если нет
              withEffect
                (replyText (T.pack "Такого напоминания нет :( Используйте /show, чтобы увидеть созданные задачи!")) model
              pure model

    Error -> do
       withEffect (replyText (T.pack "Возможно, ты ввел сообщение в неверном формате :( Попробуй еще раз!")) model
       pure model

-- токен бота
token_string :: String
token_string = "TOKEN_YOUR_BOT"

-- главная функция для запуска бота
main :: IO ()
main = do
  init_DB -- инициализация базы данных
  env <- defaultTelegramClientEnv (Token (T.pack token_string))
  startBot_ (traceBotDefault bot) env -- запуск бота

-- создадим модель
bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = Model { todoItems = [], userID = Nothing },
    botAction = flip handleUpdate,
    botHandler = handleAction,
    botJobs = [] -- пустой список заданий
  }
