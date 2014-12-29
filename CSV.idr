module CSV

import Effects
import Effect.File
import Effect.StdIO
import Effect.Exception
import Providers

import public Parser

%access public
%default total

data CSVError = NoSuchFile
              | ColumnMismatch
              | RowMismatch

instance Show CSVError where
  show NoSuchFile     = "NoSuchFile"
  show ColumnMismatch = "ColumnMismatch"

namespace Schema
  data Schema : Type where
    Nil  : Schema
    (::) : String -> Schema -> Schema

  length : Schema -> Nat
  length []        = 0
  length (x :: xs) = 1 + length xs

private
toSchema : (format : Format) -> Schema
toSchema (c :: k) = c :: toSchema k
toSchema []       = []

data IsElem : String -> Schema -> Type where
  Here  : IsElem x (x :: xs)
  There : IsElem x xs -> IsElem x (y :: xs)

namespace Row
  data Row : Schema -> Type where
    Nil  : Row []
    (::) : (s : (String, String))
        -> Row schema
        -> Row (fst s :: schema)

namespace CSV
  data CSV : Schema -> Nat -> Type where
    Nil  : CSV schema 0
    (::) : Row schema -> CSV schema n -> CSV schema (S n)

lookup : (f : String)
      -> Row schema
      -> {default tactics {search 100;} p : IsElem f schema}
      -> String
lookup (fst s) (s :: x) {p=Here}     = snd s
lookup f       (s :: x) {p=There p'} = lookup f x
lookup f       []       {p=Here}       impossible

index : Fin n -> CSV schema n -> Row schema
index FZ     (row :: _)    = row
index (FS i) (_   :: rows) = index i rows

readHeader : (filename : String)
          -> { [FILE_IO ()] } Eff String
readHeader filename =
  case !(open filename Read) of
       False => return ""
       True  => do
         header <- [| trim readLine |]
         close
         pure header

readSchema : (filename : String)
          -> { [FILE_IO (), STDIO] } Eff (Provider Schema)
readSchema filename = do
  header <- readHeader filename
  {-let parsed = parse header-}
  let parsed = Right $ split (== ',') header
  case parsed of
       Left  error  => do
         pure $ Error "error"
       Right format => do
         let schema = toSchema format
         pure $ Provide schema

fromVect : {schema : Schema}
        -> (vals : Vect (length schema) String)
        -> Row schema
fromVect {schema=[]}        []        = []
fromVect {schema=(x :: xs)} (y :: ys) = (x, y) :: (fromVect {schema=xs} ys)

private
processRow : (f : Format)
          -> (schema : Schema)
          -> { [EXCEPTION CSVError] } Eff (Row schema)
processRow (_ :: _) []        = raise ColumnMismatch
processRow []       (_ :: _)  = raise ColumnMismatch
processRow (t :: k) (f :: fs) = pure $ (f, t) :: !(processRow k fs)
processRow []       []        = pure []

partial
readRows : (n : Nat)
        -> (schema : Schema)
        -> { [FILE_IO (OpenFile Read), EXCEPTION CSVError] } Eff (CSV schema n)
readRows Z     _      = pure []
readRows (S n) schema =
  if !eof
     then raise RowMismatch
     else do
       row <- readLine
       if row /= ""
          then pure $ !(processRow (parse' row) schema) :: !(readRows n schema)
          else raise RowMismatch

partial
readCSV : {schema : Schema}
       -> String
       -> (n : Nat)
       -> { [FILE_IO (), EXCEPTION CSVError] } Eff (CSV schema n)
readCSV {schema} file n =
  case !(open file Read) of
       True => do
         _    <- readLine
         rows <- readRows n schema
         close
         pure rows
       False => raise NoSuchFile
