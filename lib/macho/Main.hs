module Main where

import Data.Macho
import Data.Foldable
import qualified Data.ByteString as B
import qualified Data.Map as Map
import DWARF.Basics
import DWARF.Addr2Line
import DWARF.Section.ARanges

main :: IO ()
main =
  do file <- B.readFile "/Users/emertens/Source/galua/galua-c/inplace/bin/galua-dbg.dSYM/Contents/Resources/DWARF/galua-dbg"
     let macho = parseMacho file
         [(secs,baseAddr,fileBytes)] =
                  [ ( seg_sections seg
                    , seg_vmaddr seg
                    , B.take (fromIntegral (seg_filesize seg))
                          (B.drop (fromIntegral (seg_fileoff seg)) file))
                    | LC_SEGMENT_64 seg <- m_commands macho
                    , seg_segname seg == "__DWARF"
                    ]
         rawSecs = Map.fromList
                           [ (hackName (sec_sectname sect),
                                extractSection (sec_addr sect)
                                               (sec_size sect)
                                               baseAddr
                                               fileBytes
                             )
                           | sect <- secs ]
         dwarfSecs = sections BigEndian rawSecs


     print rawSecs
     print (addr2line dwarfSecs 0x0000000103866150)

hackName ('_':'_':name) = '.':name

extractSection offset size base fileBytes
  = B.take (fromIntegral size)
  $ B.drop (fromIntegral (offset - base)) fileBytes
