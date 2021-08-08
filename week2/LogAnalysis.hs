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

insert :: LogMessage -> MessageTree -> MessageTree
insert logMessage tree = case logMessage of
                         Unknown _ -> tree
                         LogMessage _ ts _ -> do
                            case tree of
                                Leaf -> Node Leaf logMessage Leaf
                                Node left treeLogMessage right -> do
                                    case treeLogMessage of
                                        Unknown _ -> tree
                                        LogMessage treeMessageType treeTs treeMessage -> do
                                            if treeTs > ts then do
                                                let newLeft = insert logMessage left
                                                Node newLeft (LogMessage treeMessageType treeTs treeMessage) right
                                            else do
                                                let newRight = insert logMessage right
                                                Node left (LogMessage treeMessageType treeTs treeMessage) newRight

build :: [LogMessage] -> MessageTree
build logMessages = do
    let tree = Leaf
    buildHelper logMessages tree

buildHelper :: [LogMessage] -> MessageTree -> MessageTree

buildHelper logMessages tree = if null logMessages then tree
                               else do
                                let newTree = insert (head logMessages) tree
                                buildHelper (drop 1 logMessages) newTree

inOrder :: MessageTree -> [LogMessage]
inOrder tree = case tree of
                    Leaf -> []
                    Node left logMessage right -> inOrder left ++ [logMessage] ++ inOrder right

isBad :: LogMessage -> Int -> Bool
isBad logMessage threshold = case logMessage of
                Unknown _ -> False
                LogMessage messageType _ _ -> case messageType of
                    Error severity -> severity > threshold
                    _ -> False

getMessage :: LogMessage -> String
getMessage logMessage = case logMessage of
                 Unknown message -> message
                 LogMessage _ _ message -> message



whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logMessages = do
    let sortedBadLogs = inOrder(build([logMessage | logMessage <- logMessages, isBad logMessage 50]))
    [getMessage logMessage | logMessage <- sortedBadLogs]
