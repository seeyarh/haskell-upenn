{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Text.Read


parse :: String -> [LogMessage]
parse logs = [parseMessage line | line <- lines logs]

parseMessage :: String -> LogMessage
parseMessage logLine = if null logLine then Unknown logLine
                       else case parseMessageWords (words logLine) of
                            Nothing -> Unknown logLine
                            Just logMessage -> logMessage

parseMessageWords :: [String] -> Maybe LogMessage
parseMessageWords tokens = if null tokens then Nothing
                           else case parseMessageType tokens of
                                Nothing -> Nothing
                                Just (messageType, remaining) -> do
                                    case parseTimestamp remaining of
                                        Nothing -> Nothing
                                        Just (ts, remainingTs) -> Just (LogMessage messageType ts (unwords remainingTs))

parseMessageType :: [String] -> Maybe (MessageType, [String])
parseMessageType tokens = if null tokens then Nothing
                          else case head tokens of
                            "I" -> Just (Info, drop 1 tokens)
                            "W" -> Just (Warning, drop 1 tokens)
                            "E" -> parseErrorMessageType (drop 1 tokens)
                            _   -> Nothing

parseErrorMessageType :: [String] -> Maybe (MessageType, [String])
parseErrorMessageType tokens = if null tokens then Nothing
                               else
                                    case readMaybe (head tokens) of
                                        Nothing -> Nothing
                                        Just level -> Just (Error level, drop 1 tokens)

parseTimestamp :: [String] -> Maybe (TimeStamp, [String])
parseTimestamp tokens = if null tokens then Nothing
                        else case readMaybe (head tokens) of
                            Nothing -> Nothing
                            Just ts -> Just (ts, drop 1 tokens)
