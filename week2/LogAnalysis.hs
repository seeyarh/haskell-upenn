{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Text.Read


parse :: String -> [LogMessage]
parse logs = [parseMessage line | line <- lines logs]

parseMessage :: String -> LogMessage
parseMessage "" = Unknown ""
parseMessage logLine = case parseMessageWords (words logLine) of
                            Nothing -> Unknown logLine
                            Just logMessage -> logMessage

parseMessageWords :: [String] -> Maybe LogMessage
parseMessageWords [] = Nothing
parseMessageWords tokens = case parseMessageType tokens of
                                Nothing -> Nothing
                                Just (messageType, remaining) -> do
                                    case parseTimestamp remaining of
                                        Nothing -> Nothing
                                        Just (ts, remainingTs) -> Just (LogMessage messageType ts (unwords remainingTs))

parseMessageType :: [String] -> Maybe (MessageType, [String])
parseMessageType [] = Nothing
parseMessageType tokens = case head tokens of
                            "I" -> Just (Info, drop 1 tokens)
                            "W" -> Just (Warning, drop 1 tokens)
                            "E" -> parseErrorMessageType (drop 1 tokens)
                            _   -> Nothing

parseErrorMessageType :: [String] -> Maybe (MessageType, [String])
parseErrorMessageType [] = Nothing
parseErrorMessageType tokens = case readMaybe (head tokens) of
                                Nothing -> Nothing
                                Just level -> Just (Error level, drop 1 tokens)

parseTimestamp :: [String] -> Maybe (TimeStamp, [String])
parseTimestamp [] = Nothing
parseTimestamp tokens = case readMaybe (head tokens) of
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

buildHelper [] tree = tree
buildHelper logMessages tree = do
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
