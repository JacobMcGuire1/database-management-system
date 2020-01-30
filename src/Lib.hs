module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Database = Database {
    dbname :: String,
    tables :: [Table]
}

data Table = Table {
    tablename :: String,
    colheaders :: [ColumnHeader],
    rows :: [Row]
}

data ColumnHeader = ColumnHeader {
    colname :: String,
    datatype :: String
}

data DataField = StringField {
    strdata :: [String]
    }
    | IntField {
    intdata :: Int
    }

data Row = Row {
    fields :: [DataField]
}