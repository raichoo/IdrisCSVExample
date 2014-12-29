module Main

import Effects
import Effect.File
import Effect.Exception
import Effect.StdIO
import Providers

import CSV

%language TypeProviders
%provide (TestSchema : Schema) with run $ readSchema "test.csv"

row : Row TestSchema
row = fromVect ["raichoo", "electro"]

processCSV : { [FILE_IO (), EXCEPTION CSVError, STDIO] } Eff ()
processCSV = do
   rows <- readCSV "test.csv" 3
   putStrLn $ lookup "name" (index 1 (row :: rows))

partial
main : IO ()
main = run processCSV
