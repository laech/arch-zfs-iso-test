module Scancode (get) where


import qualified Data.Map.Strict   as Map

import           Control.Exception (Exception, throw)
import           Data.Map.Strict   (Map)
import           Data.Maybe        (fromMaybe)
import           Data.Typeable     (Typeable)
import           Numeric           (showHex)


newtype ScancodeNotFound = ScancodeNotFound { message :: String }
  deriving (Show, Typeable)

instance Exception ScancodeNotFound


get :: Char -> [String]
get char = fromMaybe
 (throw (ScancodeNotFound [char]))
 (Map.lookup char scancodes)


-- See http://www.win.tue.nl/~aeb/linux/kbd/scancodes-1.html
-- Not a complete list here
scancodes :: Map Char [String]
scancodes = Map.unions
  [ encode2 0x1e 'a' 'A'
  , encode2 0x30 'b' 'B'
  , encode2 0x2e 'c' 'C'
  , encode2 0x20 'd' 'D'
  , encode2 0x12 'e' 'E'
  , encode2 0x21 'f' 'F'
  , encode2 0x22 'g' 'G'
  , encode2 0x23 'h' 'H'
  , encode2 0x17 'i' 'I'
  , encode2 0x24 'j' 'J'
  , encode2 0x25 'k' 'K'
  , encode2 0x26 'l' 'L'
  , encode2 0x32 'm' 'M'
  , encode2 0x31 'n' 'N'
  , encode2 0x18 'o' 'O'
  , encode2 0x19 'p' 'P'
  , encode2 0x10 'q' 'Q'
  , encode2 0x13 'r' 'R'
  , encode2 0x1f 's' 'S'
  , encode2 0x14 't' 'T'
  , encode2 0x16 'u' 'U'
  , encode2 0x2f 'v' 'V'
  , encode2 0x11 'w' 'W'
  , encode2 0x2d 'x' 'X'
  , encode2 0x15 'y' 'Y'
  , encode2 0x2c 'z' 'Z'
  , encode2 0x02 '1' '!'
  , encode2 0x03 '2' '@'
  , encode2 0x04 '3' '#'
  , encode2 0x05 '4' '$'
  , encode2 0x06 '5' '%'
  , encode2 0x07 '6' '^'
  , encode2 0x08 '7' '&'
  , encode2 0x09 '8' '*'
  , encode2 0x0a '9' '('
  , encode2 0x0b '0' ')'
  , encode2 0x27 ';' ':'
  , encode2 0x0c '-' '_'
  , encode2 0x0d '=' '+'
  , encode2 0x2b '\\' '|'
  , encode2 0x29 '`' '~'
  , encode2 0x33 ',' '<'
  , encode2 0x34 '.' '>'
  , encode2 0x35 '/' '?'
  , encode2 0x28 '\'' '"'
  , encode1 0x39 ' '
  , encode1 0x1c '\n'
  ]


releaseAdder = 0x80
leftShiftPressed = 0x2a
leftShiftReleased = leftShiftPressed + releaseAdder


encode1 :: Int -> Char -> Map Char [String]
encode1 codePressed normalChar =
  encode codePressed normalChar Nothing


encode2 :: Int -> Char -> Char -> Map Char [String]
encode2 codePressed normalChar shiftedChar =
  encode codePressed normalChar (Just shiftedChar)


encode :: Int -> Char -> Maybe Char -> Map Char [String]
encode codePressed normalChar shiftedChar =
  (fmap.fmap) hex2 (Map.union normal shift)
  where
    normal = Map.singleton normalChar (keySeq codePressed)
    shift = case shiftedChar of
      Just char -> Map.singleton char (keySeqShift codePressed)
      Nothing   -> Map.empty


keySeq :: Int -> [Int]
keySeq codePressed = [codePressed, codePressed + releaseAdder]


keySeqShift :: Int -> [Int]
keySeqShift codePressed =
  leftShiftPressed :
  keySeq codePressed ++
  [leftShiftReleased]


hex2 :: Int -> String
hex2 i = hex2' (showHex i "")
  where
    hex2' [x] = '0' : [x]
    hex2' xs  = xs
