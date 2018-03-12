module Logging where


import Data.DateTime


data LogLevel = Debug | Info | Warning | Error | Fatal
              deriving (Show, Read, Eq, Ord, Enum, Bounded)

data EventSource = Internal { iesComponent   :: String
                            , iesCallID      :: String }
                 | External { eesURI         :: String
                            , eesDescription :: String }
                 | Combined [EventSource]
                 | Unknown
                 deriving (Show, Read, Eq, Ord)
                 -- TODO: remove Ord here after implementing Ord for LogMessage

data LogMessage = LogMessage
                { lmTimestamp  :: DateTime
                , lmSource     :: EventSource
                , lmMessage    :: String
                , lmHiddenFlag :: Bool
                , lmLogLevel   :: LogLevel
                } deriving (Show, Read, Eq, Ord)
                -- TODO: custom instance of Show and Ord (hidden, timestamp, logLevel)

data EventSourceMatcher = Exact EventSource
                        | With EventSource
                        | AllCombined [EventSource]
                        | AnyInternal
                        | AnyExternal
                        | Any
                        deriving (Show, Read, Eq)

-- | Specialized log list filter
-- TODO: implement filter function for logs with matchers, log level and hidden flag
logFilter :: [EventSourceMatcher] -> LogLevel -> Bool -> [LogMessage] -> [LogMessage]
logFilter = undefined

-- | Change log level operator
-- TODO: implement operator which changes LogLevel of LogMessage
($=) :: LogMessage -> LogLevel -> LogMessage
($=) = undefined

-- | EventSource combinator
-- TODO: implement operator which combines two EventSources (just 1 level for Combined, see tests)
(@@) :: EventSource -> EventSource -> EventSource
(@@) = undefined

-- | Matching EventSource with EventSourceMatcher operator
-- TODO: implement matching
(~~) :: EventSourceMatcher -> EventSource -> Bool
(~~) = undefined
