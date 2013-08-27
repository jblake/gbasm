-- Copyright © 2013 Julian Blake Kongslie <jblake@jblake.org>
-- Licensed under the MIT license.

{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-type-defaults #-}

-- |Here we define the actual opcodes allowed in our assembly language.
-- There's no particularly compact way to do this, so we essentially have three big lists, one for each arity of opcode.
-- Opcode names are always expressed as lowercase strings.
-- You don't need to worry about reference or math operands; those will always be replaced by Abs by the time functions here are called.
module Language.GBAsm.Opcodes
  ( Opcode(..)
  )
where

import Data.Bits
import Data.ByteString.Lazy as BS
import Prelude as P

import Language.GBAsm.Types

-- |This is just a helper to make things a little simpler.
bs :: (Integral n) => [n] -> Maybe ByteString
bs = Just . pack . P.map fromIntegral

-- |Another helper for single-byte opcodes.
b :: (Integral n) => n -> Maybe ByteString
b = bs . (: [])

-- |Opcodes are compiled using this class, just to allow a single entry point for nullary, unary, and binary opcodes.
class Opcode f where
  opc :: String -> f

-- |Nullary opcodes.
instance Opcode (Maybe ByteString) where

  opc "nop"   = b 0x00

  opc "stop"  = b 0x10

  opc "daa"   = b 0x27
  opc "cpl"   = b 0x2f

  opc "scf"   = b 0x37
  opc "ccf"   = b 0x3f

  opc "halt"  = b 0x76

  opc "ret"   = b 0xc9

  opc "reti"  = b 0xd9

  opc "di"    = b 0xf3
  opc "ei"    = b 0xfb

  opc _       = Nothing

-- |Unary opcodes.
instance Opcode (Operand -> Maybe ByteString) where

  opc "inc"   BC            = b 0x03
  opc "inc"   B             = b 0x04
  opc "dec"   B             = b 0x05
  opc "rlc"   A             = b 0x07
  opc "dec"   BC            = b 0x0b
  opc "inc"   C             = b 0x0c
  opc "dec"   C             = b 0x0d
  opc "rrc"   A             = b 0x0f

  opc "inc"   DE            = b 0x13
  opc "inc"   D             = b 0x14
  opc "dec"   D             = b 0x15
  opc "rl"    A             = b 0x17
  opc "jr"    (Abs n)       = bs [0x18, n]
  opc "dec"   DE            = b 0x1b
  opc "inc"   E             = b 0x1c
  opc "dec"   E             = b 0x1d
  opc "rr"    A             = b 0x1f

  opc "inc"   HL            = b 0x23
  opc "inc"   H             = b 0x24
  opc "dec"   H             = b 0x25
  opc "dec"   HL            = b 0x2b
  opc "inc"   L             = b 0x2c
  opc "dec"   L             = b 0x2d

  opc "inc"   SP            = b 0x33
  opc "inc"   (Ind HL)      = b 0x34
  opc "dec"   (Ind HL)      = b 0x35
  opc "dec"   SP            = b 0x3b
  opc "inc"   A             = b 0x3c
  opc "dec"   A             = b 0x3d

  opc "ret"   NZ            = b 0xc0
  opc "pop"   BC            = b 0xc1
  opc "jp"    (Abs n)       = bs [0xc3, n, n `shiftR` 8]
  opc "push"  BC            = b 0xc5
  opc "rst"   (Abs 0x00)    = b 0xc7
  opc "ret"   Z             = b 0xc8
  opc "call"  (Abs n)       = bs [0xcd, n, n `shiftR` 8]
  opc "rst"   (Abs 0x08)    = b 0xcf

  opc "ret"   NC            = b 0xd0
  opc "pop"   DE            = b 0xd1
  opc "push"  DE            = b 0xd5
  opc "rst"   (Abs 0x10)    = b 0xd7
  opc "ret"   C             = b 0xd8
  opc "rst"   (Abs 0x18)    = b 0xdf

  opc "pop"   HL            = b 0xe1
  opc "push"  HL            = b 0xe5
  opc "rst"   (Abs 0x20)    = b 0xe7
  opc "jp"    (Ind HL)      = b 0xe9
  opc "rst"   (Abs 0x28)    = b 0xef

  opc "pop"   AF            = b 0xf1
  opc "push"  AF            = b 0xf5
  opc "rst"   (Abs 0x30)    = b 0xf7
  opc "cp"    (Abs n)       = bs [0xfe, n]
  opc "rst"   (Abs 0x38)    = b 0xff

  opc "rlc"   B             = bs [0xcb, 0x00]
  opc "rlc"   C             = bs [0xcb, 0x01]
  opc "rlc"   D             = bs [0xcb, 0x02]
  opc "rlc"   E             = bs [0xcb, 0x03]
  opc "rlc"   H             = bs [0xcb, 0x04]
  opc "rlc"   L             = bs [0xcb, 0x05]
  opc "rlc"   (Ind HL)      = bs [0xcb, 0x06]
--  opc "rlc"   A             = bs [0xcb, 0x07]
  opc "rrc"   B             = bs [0xcb, 0x08]
  opc "rrc"   C             = bs [0xcb, 0x09]
  opc "rrc"   D             = bs [0xcb, 0x0a]
  opc "rrc"   E             = bs [0xcb, 0x0b]
  opc "rrc"   H             = bs [0xcb, 0x0c]
  opc "rrc"   L             = bs [0xcb, 0x0d]
  opc "rrc"   (Ind HL)      = bs [0xcb, 0x0e]
--  opc "rrc"   A             = bs [0xcb, 0x0f]

  opc "rl"    B             = bs [0xcb, 0x10]
  opc "rl"    C             = bs [0xcb, 0x11]
  opc "rl"    D             = bs [0xcb, 0x12]
  opc "rl"    E             = bs [0xcb, 0x13]
  opc "rl"    H             = bs [0xcb, 0x14]
  opc "rl"    L             = bs [0xcb, 0x15]
  opc "rl"    (Ind HL)      = bs [0xcb, 0x16]
--  opc "rl"    A             = bs [0xcb, 0x17]
  opc "rr"    B             = bs [0xcb, 0x18]
  opc "rr"    C             = bs [0xcb, 0x19]
  opc "rr"    D             = bs [0xcb, 0x1a]
  opc "rr"    E             = bs [0xcb, 0x1b]
  opc "rr"    H             = bs [0xcb, 0x1c]
  opc "rr"    L             = bs [0xcb, 0x1d]
  opc "rr"    (Ind HL)      = bs [0xcb, 0x1e]
--  opc "rr"    A             = bs [0xcb, 0x1f]

  opc "sla"   B             = bs [0xcb, 0x20]
  opc "sla"   C             = bs [0xcb, 0x21]
  opc "sla"   D             = bs [0xcb, 0x22]
  opc "sla"   E             = bs [0xcb, 0x23]
  opc "sla"   H             = bs [0xcb, 0x24]
  opc "sla"   L             = bs [0xcb, 0x25]
  opc "sla"   (Ind HL)      = bs [0xcb, 0x26]
  opc "sla"   A             = bs [0xcb, 0x27]
  opc "sra"   B             = bs [0xcb, 0x28]
  opc "sra"   C             = bs [0xcb, 0x29]
  opc "sra"   D             = bs [0xcb, 0x2a]
  opc "sra"   E             = bs [0xcb, 0x2b]
  opc "sra"   H             = bs [0xcb, 0x2c]
  opc "sra"   L             = bs [0xcb, 0x2d]
  opc "sra"   (Ind HL)      = bs [0xcb, 0x2e]
  opc "sra"   A             = bs [0xcb, 0x2f]

  opc "swap"  B             = bs [0xcb, 0x30]
  opc "swap"  C             = bs [0xcb, 0x31]
  opc "swap"  D             = bs [0xcb, 0x32]
  opc "swap"  E             = bs [0xcb, 0x33]
  opc "swap"  H             = bs [0xcb, 0x34]
  opc "swap"  L             = bs [0xcb, 0x35]
  opc "swap"  (Ind HL)      = bs [0xcb, 0x36]
  opc "swap"  A             = bs [0xcb, 0x37]
  opc "srl"   B             = bs [0xcb, 0x38]
  opc "srl"   C             = bs [0xcb, 0x39]
  opc "srl"   D             = bs [0xcb, 0x3a]
  opc "srl"   E             = bs [0xcb, 0x3b]
  opc "srl"   H             = bs [0xcb, 0x3c]
  opc "srl"   L             = bs [0xcb, 0x3d]
  opc "srl"   (Ind HL)      = bs [0xcb, 0x3e]
  opc "srl"   A             = bs [0xcb, 0x3f]

  opc _       _             = Nothing

-- |Binary opcodes.
instance Opcode (Operand -> Operand -> Maybe ByteString) where

  opc "ld"    BC            (Abs n)       = bs [0x01, n, n `shiftR` 8]
  opc "ld"    (Ind BC)      A             = b 0x02
  opc "ld"    B             (Abs n)       = bs [0x06, n]
  opc "ld"    (Ind (Abs n)) SP            = bs [0x08, n, n `shiftR` 8]
  opc "add"   HL            BC            = b 0x09
  opc "ld"    A             (Ind BC)      = b 0x0a
  opc "ld"    C             (Abs n)       = bs [0x0e, n]

  opc "ld"    DE            (Abs n)       = bs [0x11, n, n `shiftR` 8]
  opc "ld"    (Ind DE)      A             = b 0x12
  opc "ld"    D             (Abs n)       = bs [0x16, n]
  opc "add"   HL            DE            = b 0x19
  opc "ld"    A             (Ind DE)      = b 0x1a
  opc "ld"    E             (Abs n)       = bs [0x1e, n]

  opc "jr"    NZ            (Abs n)       = bs [0x20, n]
  opc "ld"    HL            (Abs n)       = bs [0x21, n, n `shiftR` 8]
  opc "ldi"   (Ind HL)      A             = b 0x22
  opc "ld"    H             (Abs n)       = bs [0x26, n]
  opc "jr"    Z             (Abs n)       = bs [0x28, n]
  opc "add"   HL            HL            = b 0x29
  opc "ldi"   A             (Ind HL)      = b 0x2a
  opc "ld"    L             (Abs n)       = bs [0x2e, n]

  opc "jr"    NC            (Abs n)       = bs [0x30, n]
  opc "ld"    SP            (Abs n)       = bs [0x31, n, n `shiftR` 8]
  opc "ldd"   (Ind HL)      A             = b 0x32
  opc "ld"    (Ind HL)      (Abs n)       = bs [0x36, n]
  opc "jr"    C             (Abs n)       = bs [0x38, n]
  opc "add"   HL            SP            = b 0x39
  opc "ldd"   A             (Ind HL)      = b 0x3a
  opc "ld"    A             (Abs n)       = bs [0x3e, n]

  opc "ld"    B             B             = b 0x40
  opc "ld"    B             C             = b 0x41
  opc "ld"    B             D             = b 0x42
  opc "ld"    B             E             = b 0x43
  opc "ld"    B             H             = b 0x44
  opc "ld"    B             L             = b 0x45
  opc "ld"    B             (Ind HL)      = b 0x46
  opc "ld"    B             A             = b 0x47
  opc "ld"    C             B             = b 0x48
  opc "ld"    C             C             = b 0x49
  opc "ld"    C             D             = b 0x4a
  opc "ld"    C             E             = b 0x4b
  opc "ld"    C             H             = b 0x4c
  opc "ld"    C             L             = b 0x4d
  opc "ld"    C             (Ind HL)      = b 0x4e
  opc "ld"    C             A             = b 0x4f

  opc "ld"    D             B             = b 0x50
  opc "ld"    D             C             = b 0x51
  opc "ld"    D             D             = b 0x52
  opc "ld"    D             E             = b 0x53
  opc "ld"    D             H             = b 0x54
  opc "ld"    D             L             = b 0x55
  opc "ld"    D             (Ind HL)      = b 0x56
  opc "ld"    D             A             = b 0x57
  opc "ld"    E             B             = b 0x58
  opc "ld"    E             C             = b 0x59
  opc "ld"    E             D             = b 0x5a
  opc "ld"    E             E             = b 0x5b
  opc "ld"    E             H             = b 0x5c
  opc "ld"    E             L             = b 0x5d
  opc "ld"    E             (Ind HL)      = b 0x5e
  opc "ld"    E             A             = b 0x5f

  opc "ld"    H             B             = b 0x60
  opc "ld"    H             C             = b 0x61
  opc "ld"    H             D             = b 0x62
  opc "ld"    H             E             = b 0x63
  opc "ld"    H             H             = b 0x64
  opc "ld"    H             L             = b 0x65
  opc "ld"    H             (Ind HL)      = b 0x66
  opc "ld"    H             A             = b 0x67
  opc "ld"    L             B             = b 0x68
  opc "ld"    L             C             = b 0x69
  opc "ld"    L             D             = b 0x6a
  opc "ld"    L             E             = b 0x6b
  opc "ld"    L             H             = b 0x6c
  opc "ld"    L             L             = b 0x6d
  opc "ld"    L             (Ind HL)      = b 0x6e
  opc "ld"    L             A             = b 0x6f

  opc "ld"    (Ind HL)      B             = b 0x70
  opc "ld"    (Ind HL)      C             = b 0x71
  opc "ld"    (Ind HL)      D             = b 0x72
  opc "ld"    (Ind HL)      E             = b 0x73
  opc "ld"    (Ind HL)      H             = b 0x74
  opc "ld"    (Ind HL)      L             = b 0x75
  opc "ld"    (Ind HL)      A             = b 0x77
  opc "ld"    A             B             = b 0x78
  opc "ld"    A             C             = b 0x79
  opc "ld"    A             D             = b 0x7a
  opc "ld"    A             E             = b 0x7b
  opc "ld"    A             H             = b 0x7c
  opc "ld"    A             L             = b 0x7d
  opc "ld"    A             (Ind HL)      = b 0x7e
  opc "ld"    A             A             = b 0x7f

  opc "add"   A             B             = b 0x80
  opc "add"   A             C             = b 0x81
  opc "add"   A             D             = b 0x82
  opc "add"   A             E             = b 0x83
  opc "add"   A             H             = b 0x84
  opc "add"   A             L             = b 0x85
  opc "add"   A             (Ind HL)      = b 0x86
  opc "add"   A             A             = b 0x87
  opc "adc"   A             B             = b 0x88
  opc "adc"   A             C             = b 0x89
  opc "adc"   A             D             = b 0x8a
  opc "adc"   A             E             = b 0x8b
  opc "adc"   A             H             = b 0x8c
  opc "adc"   A             L             = b 0x8d
  opc "adc"   A             (Ind HL)      = b 0x8e
  opc "adc"   A             A             = b 0x8f

  opc "sub"   A             B             = b 0x90
  opc "sub"   A             C             = b 0x91
  opc "sub"   A             D             = b 0x92
  opc "sub"   A             E             = b 0x93
  opc "sub"   A             H             = b 0x94
  opc "sub"   A             L             = b 0x95
  opc "sub"   A             (Ind HL)      = b 0x96
  opc "sub"   A             A             = b 0x97
  opc "sbc"   A             B             = b 0x98
  opc "sbc"   A             C             = b 0x99
  opc "sbc"   A             D             = b 0x9a
  opc "sbc"   A             E             = b 0x9b
  opc "sbc"   A             H             = b 0x9c
  opc "sbc"   A             L             = b 0x9d
  opc "sbc"   A             (Ind HL)      = b 0x9e
  opc "sbc"   A             A             = b 0x9f

  opc "and"   A             B             = b 0xa0
  opc "and"   A             C             = b 0xa1
  opc "and"   A             D             = b 0xa2
  opc "and"   A             E             = b 0xa3
  opc "and"   A             H             = b 0xa4
  opc "and"   A             L             = b 0xa5
  opc "and"   A             (Ind HL)      = b 0xa6
  opc "and"   A             A             = b 0xa7
  opc "xor"   A             B             = b 0xa8
  opc "xor"   A             C             = b 0xa9
  opc "xor"   A             D             = b 0xaa
  opc "xor"   A             E             = b 0xab
  opc "xor"   A             H             = b 0xac
  opc "xor"   A             L             = b 0xad
  opc "xor"   A             (Ind HL)      = b 0xae
  opc "xor"   A             A             = b 0xaf

  opc "or"    A             B             = b 0xb0
  opc "or"    A             C             = b 0xb1
  opc "or"    A             D             = b 0xb2
  opc "or"    A             E             = b 0xb3
  opc "or"    A             H             = b 0xb4
  opc "or"    A             L             = b 0xb5
  opc "or"    A             (Ind HL)      = b 0xb6
  opc "or"    A             A             = b 0xb7
  opc "cp"    A             B             = b 0xb8
  opc "cp"    A             C             = b 0xb9
  opc "cp"    A             D             = b 0xba
  opc "cp"    A             E             = b 0xbb
  opc "cp"    A             H             = b 0xbc
  opc "cp"    A             L             = b 0xbd
  opc "cp"    A             (Ind HL)      = b 0xbe
  opc "cp"    A             A             = b 0xbf

  opc "jp"    NZ            (Abs n)       = bs [0xc2, n, n `shiftR` 8]
  opc "call"  NZ            (Abs n)       = bs [0xc4, n, n `shiftR` 8]
  opc "add"   A             (Abs n)       = bs [0xc6, n]
  opc "jp"    Z             (Abs n)       = bs [0xca, n, n `shiftR` 8]
  opc "call"  Z             (Abs n)       = bs [0xcc, n, n `shiftR` 8]
  opc "adc"   A             (Abs n)       = bs [0xce, n]

  opc "jp"    NC            (Abs n)       = bs [0xd2, n, n `shiftR` 8]
  opc "call"  NC            (Abs n)       = bs [0xd4, n, n `shiftR` 8]
  opc "sub"   A             (Abs n)       = bs [0xd6, n]
  opc "jp"    C             (Abs n)       = bs [0xda, n, n `shiftR` 8]
  opc "call"  C             (Abs n)       = bs [0xdc, n, n `shiftR` 8]
  opc "sbc"   A             (Abs n)       = bs [0xde, n]

  opc "ldh"   (Ind (Abs n)) A             | n >= 0xff00 = bs [0xe0, n]
  opc "ldh"   (Ind C)       A             = b 0xe2
  opc "and"   A             (Abs n)       = bs [0xe6, n]
  opc "add"   SP            (Abs n)       = bs [0xe8, n]
  opc "ld"    (Ind (Abs n)) A             = bs [0xea, n, n `shiftR` 8]
  opc "xor"   A             (Abs n)       = bs [0xee, n]

  opc "ldh"   A             (Ind (Abs n)) | n >= 0xff00 = bs [0xf0, n]
  opc "or"    A             (Abs n)       = bs [0xf6, n]
  opc "ldhl"  SP            (Ind (Abs n)) | n >= 0xff00 = bs [0xf8, n]
  opc "ld"    SP            HL            = b 0xf9
  opc "ld"    A             (Ind (Abs n)) = bs [0xfa, n, n `shiftR` 8]

  opc "bit"   (Abs 0)       B             = bs [0xcb, 0x40]
  opc "bit"   (Abs 0)       C             = bs [0xcb, 0x41]
  opc "bit"   (Abs 0)       D             = bs [0xcb, 0x42]
  opc "bit"   (Abs 0)       E             = bs [0xcb, 0x43]
  opc "bit"   (Abs 0)       H             = bs [0xcb, 0x44]
  opc "bit"   (Abs 0)       L             = bs [0xcb, 0x45]
  opc "bit"   (Abs 0)       (Ind HL)      = bs [0xcb, 0x46]
  opc "bit"   (Abs 0)       A             = bs [0xcb, 0x47]
  opc "bit"   (Abs 1)       B             = bs [0xcb, 0x48]
  opc "bit"   (Abs 1)       C             = bs [0xcb, 0x49]
  opc "bit"   (Abs 1)       D             = bs [0xcb, 0x4a]
  opc "bit"   (Abs 1)       E             = bs [0xcb, 0x4b]
  opc "bit"   (Abs 1)       H             = bs [0xcb, 0x4c]
  opc "bit"   (Abs 1)       L             = bs [0xcb, 0x4d]
  opc "bit"   (Abs 1)       (Ind HL)      = bs [0xcb, 0x4e]
  opc "bit"   (Abs 1)       A             = bs [0xcb, 0x4f]

  opc "bit"   (Abs 2)       B             = bs [0xcb, 0x50]
  opc "bit"   (Abs 2)       C             = bs [0xcb, 0x51]
  opc "bit"   (Abs 2)       D             = bs [0xcb, 0x52]
  opc "bit"   (Abs 2)       E             = bs [0xcb, 0x53]
  opc "bit"   (Abs 2)       H             = bs [0xcb, 0x54]
  opc "bit"   (Abs 2)       L             = bs [0xcb, 0x55]
  opc "bit"   (Abs 2)       (Ind HL)      = bs [0xcb, 0x56]
  opc "bit"   (Abs 2)       A             = bs [0xcb, 0x57]
  opc "bit"   (Abs 3)       B             = bs [0xcb, 0x58]
  opc "bit"   (Abs 3)       C             = bs [0xcb, 0x59]
  opc "bit"   (Abs 3)       D             = bs [0xcb, 0x5a]
  opc "bit"   (Abs 3)       E             = bs [0xcb, 0x5b]
  opc "bit"   (Abs 3)       H             = bs [0xcb, 0x5c]
  opc "bit"   (Abs 3)       L             = bs [0xcb, 0x5d]
  opc "bit"   (Abs 3)       (Ind HL)      = bs [0xcb, 0x5e]
  opc "bit"   (Abs 3)       A             = bs [0xcb, 0x5f]

  opc "bit"   (Abs 4)       B             = bs [0xcb, 0x60]
  opc "bit"   (Abs 4)       C             = bs [0xcb, 0x61]
  opc "bit"   (Abs 4)       D             = bs [0xcb, 0x62]
  opc "bit"   (Abs 4)       E             = bs [0xcb, 0x63]
  opc "bit"   (Abs 4)       H             = bs [0xcb, 0x64]
  opc "bit"   (Abs 4)       L             = bs [0xcb, 0x65]
  opc "bit"   (Abs 4)       (Ind HL)      = bs [0xcb, 0x66]
  opc "bit"   (Abs 4)       A             = bs [0xcb, 0x67]
  opc "bit"   (Abs 5)       B             = bs [0xcb, 0x68]
  opc "bit"   (Abs 5)       C             = bs [0xcb, 0x69]
  opc "bit"   (Abs 5)       D             = bs [0xcb, 0x6a]
  opc "bit"   (Abs 5)       E             = bs [0xcb, 0x6b]
  opc "bit"   (Abs 5)       H             = bs [0xcb, 0x6c]
  opc "bit"   (Abs 5)       L             = bs [0xcb, 0x6d]
  opc "bit"   (Abs 5)       (Ind HL)      = bs [0xcb, 0x6e]
  opc "bit"   (Abs 5)       A             = bs [0xcb, 0x6f]

  opc "bit"   (Abs 6)       B             = bs [0xcb, 0x70]
  opc "bit"   (Abs 6)       C             = bs [0xcb, 0x71]
  opc "bit"   (Abs 6)       D             = bs [0xcb, 0x72]
  opc "bit"   (Abs 6)       E             = bs [0xcb, 0x73]
  opc "bit"   (Abs 6)       H             = bs [0xcb, 0x74]
  opc "bit"   (Abs 6)       L             = bs [0xcb, 0x75]
  opc "bit"   (Abs 6)       (Ind HL)      = bs [0xcb, 0x76]
  opc "bit"   (Abs 6)       A             = bs [0xcb, 0x77]
  opc "bit"   (Abs 7)       B             = bs [0xcb, 0x78]
  opc "bit"   (Abs 7)       C             = bs [0xcb, 0x79]
  opc "bit"   (Abs 7)       D             = bs [0xcb, 0x7a]
  opc "bit"   (Abs 7)       E             = bs [0xcb, 0x7b]
  opc "bit"   (Abs 7)       H             = bs [0xcb, 0x7c]
  opc "bit"   (Abs 7)       L             = bs [0xcb, 0x7d]
  opc "bit"   (Abs 7)       (Ind HL)      = bs [0xcb, 0x7e]
  opc "bit"   (Abs 7)       A             = bs [0xcb, 0x7f]

  opc "res"   (Abs 0)       B             = bs [0xcb, 0x80]
  opc "res"   (Abs 0)       C             = bs [0xcb, 0x81]
  opc "res"   (Abs 0)       D             = bs [0xcb, 0x82]
  opc "res"   (Abs 0)       E             = bs [0xcb, 0x83]
  opc "res"   (Abs 0)       H             = bs [0xcb, 0x84]
  opc "res"   (Abs 0)       L             = bs [0xcb, 0x85]
  opc "res"   (Abs 0)       (Ind HL)      = bs [0xcb, 0x86]
  opc "res"   (Abs 0)       A             = bs [0xcb, 0x87]
  opc "res"   (Abs 1)       B             = bs [0xcb, 0x88]
  opc "res"   (Abs 1)       C             = bs [0xcb, 0x89]
  opc "res"   (Abs 1)       D             = bs [0xcb, 0x8a]
  opc "res"   (Abs 1)       E             = bs [0xcb, 0x8b]
  opc "res"   (Abs 1)       H             = bs [0xcb, 0x8c]
  opc "res"   (Abs 1)       L             = bs [0xcb, 0x8d]
  opc "res"   (Abs 1)       (Ind HL)      = bs [0xcb, 0x8e]
  opc "res"   (Abs 1)       A             = bs [0xcb, 0x8f]

  opc "res"   (Abs 2)       B             = bs [0xcb, 0x90]
  opc "res"   (Abs 2)       C             = bs [0xcb, 0x91]
  opc "res"   (Abs 2)       D             = bs [0xcb, 0x92]
  opc "res"   (Abs 2)       E             = bs [0xcb, 0x93]
  opc "res"   (Abs 2)       H             = bs [0xcb, 0x94]
  opc "res"   (Abs 2)       L             = bs [0xcb, 0x95]
  opc "res"   (Abs 2)       (Ind HL)      = bs [0xcb, 0x96]
  opc "res"   (Abs 2)       A             = bs [0xcb, 0x97]
  opc "res"   (Abs 3)       B             = bs [0xcb, 0x98]
  opc "res"   (Abs 3)       C             = bs [0xcb, 0x99]
  opc "res"   (Abs 3)       D             = bs [0xcb, 0x9a]
  opc "res"   (Abs 3)       E             = bs [0xcb, 0x9b]
  opc "res"   (Abs 3)       H             = bs [0xcb, 0x9c]
  opc "res"   (Abs 3)       L             = bs [0xcb, 0x9d]
  opc "res"   (Abs 3)       (Ind HL)      = bs [0xcb, 0x9e]
  opc "res"   (Abs 3)       A             = bs [0xcb, 0x9f]

  opc "res"   (Abs 4)       B             = bs [0xcb, 0xa0]
  opc "res"   (Abs 4)       C             = bs [0xcb, 0xa1]
  opc "res"   (Abs 4)       D             = bs [0xcb, 0xa2]
  opc "res"   (Abs 4)       E             = bs [0xcb, 0xa3]
  opc "res"   (Abs 4)       H             = bs [0xcb, 0xa4]
  opc "res"   (Abs 4)       L             = bs [0xcb, 0xa5]
  opc "res"   (Abs 4)       (Ind HL)      = bs [0xcb, 0xa6]
  opc "res"   (Abs 4)       A             = bs [0xcb, 0xa7]
  opc "res"   (Abs 5)       B             = bs [0xcb, 0xa8]
  opc "res"   (Abs 5)       C             = bs [0xcb, 0xa9]
  opc "res"   (Abs 5)       D             = bs [0xcb, 0xaa]
  opc "res"   (Abs 5)       E             = bs [0xcb, 0xab]
  opc "res"   (Abs 5)       H             = bs [0xcb, 0xac]
  opc "res"   (Abs 5)       L             = bs [0xcb, 0xad]
  opc "res"   (Abs 5)       (Ind HL)      = bs [0xcb, 0xae]
  opc "res"   (Abs 5)       A             = bs [0xcb, 0xaf]

  opc "res"   (Abs 6)       B             = bs [0xcb, 0xb0]
  opc "res"   (Abs 6)       C             = bs [0xcb, 0xb1]
  opc "res"   (Abs 6)       D             = bs [0xcb, 0xb2]
  opc "res"   (Abs 6)       E             = bs [0xcb, 0xb3]
  opc "res"   (Abs 6)       H             = bs [0xcb, 0xb4]
  opc "res"   (Abs 6)       L             = bs [0xcb, 0xb5]
  opc "res"   (Abs 6)       (Ind HL)      = bs [0xcb, 0xb6]
  opc "res"   (Abs 6)       A             = bs [0xcb, 0xb7]
  opc "res"   (Abs 7)       B             = bs [0xcb, 0xb8]
  opc "res"   (Abs 7)       C             = bs [0xcb, 0xb9]
  opc "res"   (Abs 7)       D             = bs [0xcb, 0xba]
  opc "res"   (Abs 7)       E             = bs [0xcb, 0xbb]
  opc "res"   (Abs 7)       H             = bs [0xcb, 0xbc]
  opc "res"   (Abs 7)       L             = bs [0xcb, 0xbd]
  opc "res"   (Abs 7)       (Ind HL)      = bs [0xcb, 0xbe]
  opc "res"   (Abs 7)       A             = bs [0xcb, 0xbf]

  opc "set"   (Abs 0)       B             = bs [0xcb, 0xc0]
  opc "set"   (Abs 0)       C             = bs [0xcb, 0xc1]
  opc "set"   (Abs 0)       D             = bs [0xcb, 0xc2]
  opc "set"   (Abs 0)       E             = bs [0xcb, 0xc3]
  opc "set"   (Abs 0)       H             = bs [0xcb, 0xc4]
  opc "set"   (Abs 0)       L             = bs [0xcb, 0xc5]
  opc "set"   (Abs 0)       (Ind HL)      = bs [0xcb, 0xc6]
  opc "set"   (Abs 0)       A             = bs [0xcb, 0xc7]
  opc "set"   (Abs 1)       B             = bs [0xcb, 0xc8]
  opc "set"   (Abs 1)       C             = bs [0xcb, 0xc9]
  opc "set"   (Abs 1)       D             = bs [0xcb, 0xca]
  opc "set"   (Abs 1)       E             = bs [0xcb, 0xcb]
  opc "set"   (Abs 1)       H             = bs [0xcb, 0xcc]
  opc "set"   (Abs 1)       L             = bs [0xcb, 0xcd]
  opc "set"   (Abs 1)       (Ind HL)      = bs [0xcb, 0xce]
  opc "set"   (Abs 1)       A             = bs [0xcb, 0xcf]

  opc "set"   (Abs 2)       B             = bs [0xcb, 0xd0]
  opc "set"   (Abs 2)       C             = bs [0xcb, 0xd1]
  opc "set"   (Abs 2)       D             = bs [0xcb, 0xd2]
  opc "set"   (Abs 2)       E             = bs [0xcb, 0xd3]
  opc "set"   (Abs 2)       H             = bs [0xcb, 0xd4]
  opc "set"   (Abs 2)       L             = bs [0xcb, 0xd5]
  opc "set"   (Abs 2)       (Ind HL)      = bs [0xcb, 0xd6]
  opc "set"   (Abs 2)       A             = bs [0xcb, 0xd7]
  opc "set"   (Abs 3)       B             = bs [0xcb, 0xd8]
  opc "set"   (Abs 3)       C             = bs [0xcb, 0xd9]
  opc "set"   (Abs 3)       D             = bs [0xcb, 0xda]
  opc "set"   (Abs 3)       E             = bs [0xcb, 0xdb]
  opc "set"   (Abs 3)       H             = bs [0xcb, 0xdc]
  opc "set"   (Abs 3)       L             = bs [0xcb, 0xdd]
  opc "set"   (Abs 3)       (Ind HL)      = bs [0xcb, 0xde]
  opc "set"   (Abs 3)       A             = bs [0xcb, 0xdf]

  opc "set"   (Abs 4)       B             = bs [0xcb, 0xe0]
  opc "set"   (Abs 4)       C             = bs [0xcb, 0xe1]
  opc "set"   (Abs 4)       D             = bs [0xcb, 0xe2]
  opc "set"   (Abs 4)       E             = bs [0xcb, 0xe3]
  opc "set"   (Abs 4)       H             = bs [0xcb, 0xe4]
  opc "set"   (Abs 4)       L             = bs [0xcb, 0xe5]
  opc "set"   (Abs 4)       (Ind HL)      = bs [0xcb, 0xe6]
  opc "set"   (Abs 4)       A             = bs [0xcb, 0xe7]
  opc "set"   (Abs 5)       B             = bs [0xcb, 0xe8]
  opc "set"   (Abs 5)       C             = bs [0xcb, 0xe9]
  opc "set"   (Abs 5)       D             = bs [0xcb, 0xea]
  opc "set"   (Abs 5)       E             = bs [0xcb, 0xeb]
  opc "set"   (Abs 5)       H             = bs [0xcb, 0xec]
  opc "set"   (Abs 5)       L             = bs [0xcb, 0xed]
  opc "set"   (Abs 5)       (Ind HL)      = bs [0xcb, 0xee]
  opc "set"   (Abs 5)       A             = bs [0xcb, 0xef]

  opc "set"   (Abs 6)       B             = bs [0xcb, 0xf0]
  opc "set"   (Abs 6)       C             = bs [0xcb, 0xf1]
  opc "set"   (Abs 6)       D             = bs [0xcb, 0xf2]
  opc "set"   (Abs 6)       E             = bs [0xcb, 0xf3]
  opc "set"   (Abs 6)       H             = bs [0xcb, 0xf4]
  opc "set"   (Abs 6)       L             = bs [0xcb, 0xf5]
  opc "set"   (Abs 6)       (Ind HL)      = bs [0xcb, 0xf6]
  opc "set"   (Abs 6)       A             = bs [0xcb, 0xf7]
  opc "set"   (Abs 7)       B             = bs [0xcb, 0xf8]
  opc "set"   (Abs 7)       C             = bs [0xcb, 0xf9]
  opc "set"   (Abs 7)       D             = bs [0xcb, 0xfa]
  opc "set"   (Abs 7)       E             = bs [0xcb, 0xfb]
  opc "set"   (Abs 7)       H             = bs [0xcb, 0xfc]
  opc "set"   (Abs 7)       L             = bs [0xcb, 0xfd]
  opc "set"   (Abs 7)       (Ind HL)      = bs [0xcb, 0xfe]
  opc "set"   (Abs 7)       A             = bs [0xcb, 0xff]

  opc _       _             _             = Nothing
