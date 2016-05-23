{-# LANGUAGE QuasiQuotes #-}
module Language.SMEIL.VHDL.Snippets
       ( header
       , clockedSignals
       , clockedMap
       , csvUtil
       , systemTypes
       )
       where

import Text.PrettyPrint
import Text.RawString.QQ

import Language.SMEIL.VHDL.Pretty

header :: Doc
header = text $ [r|library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

library sme_types;
use work.sem_types.all;

|]

clockedSignals :: Doc
clockedSignals = text [r|-- Reset signal
  "rst: std_logic;

  -- Clock signal
  clk: std_logic;
|]

clockedMap :: Doc
clockedMap = empty $+$ text "rst => rst," $+$ text "clk => clk"

-- systemTypes :: Doc
-- systemTypes = text [r|library IEEE;
-- use IEEE.STD_LOGIC_1164.ALL;
-- use IEEE.NUMERIC_STD.ALL;

-- |]
--   $+$ pp Package <+> text "SYSTEM_TYPES" <+> Is
--   $+$ indent ( pp Subtype <+> text "T_SYSTEM_BOOL" <+> 

systemTypes :: Doc
systemTypes = text [r|library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package sme_types is
  subtype bool_t is std_logic;

  subtype u8_t  is std_logic_vector(7 downto 0);
  subtype u16_t is std_logic_vector(15 downto 0);
  subtype u32_t is std_logic_vector(31 downto 0);
  subtype u64_t is std_logic_vector(63 downto 0);

  subtype i8_t  is std_logic_vector(7 downto 0);
  subtype i16_t is std_logic_vector(15 downto 0);
  subtype i32_t is std_logic_vector(31 downto 0);
  subtype i64_t is std_logic_vector(63 downto 0);
  -- converts an integer to u8
  pure function u8(v: integer) return u8_t;
  -- converts an integer to u16
  pure function u16(v: integer) return u16_t;
  -- converts an integer to u32
  pure function u32(v: integer) return u32_t;
  -- converts an integer to u64
  pure function u64(v: integer) return u64_t;

  -- converts an integer to i8
  pure function i8(v: integer) return i8_t;
  -- converts an integer to i16
  pure function i16(v: integer) return i16_t;
  -- converts an integer to i32
  pure function i32(v: integer) return i32_t;
  -- converts an integer to i64
  pure function i64(v: integer) return i64_t;


end sme_types;

package body sme_types is

  -- converts an integer to u8
  pure function u8(v: integer) return u8_t is
  begin
    return std_logic_vector(to_unsigned(v, u8_t'length));
  end u8;

  -- converts an integer to u16
  pure function u16(v: integer) return u16_t is
  begin
    return std_logic_vector(to_unsigned(v, u16_t'length));
  end u16;

  -- converts an integer to u32
  pure function u32(v: integer) return u32_t is
  begin
    return std_logic_vector(to_unsigned(v, u32_t'length));
  end u32;

  -- converts an integer to u64
  pure function u64(v: integer) return u64_t is
  begin
    return std_logic_vector(to_unsigned(v, u64_t'length));
  end u64;

  -- converts an integer to i8
  pure function i8(v: integer) return i8_t is
  begin
    return std_logic_vector(to_signed(v, i8_t'length));
  end i8;

  -- converts an integer to i16
  pure function i16(v: integer) return i16_t is
  begin
    return std_logic_vector(to_signed(v, i16_t'length));
  end i16;

  -- converts an integer to i32
  pure function i32(v: integer) return i32_t is
  begin
    return std_logic_vector(to_signed(v, i32_t'length));
  end i32;

  -- converts an integer to i64
  pure function i64(v: integer) return i64_t is
  begin
    return std_logic_vector(to_signed(v, i64_t'length));
  end i64;

end sme_types;
|]

csvUtil :: Doc
csvUtil = text [r|library ieee;

use ieee.std_ligic_1164.all;
use std.textio.all;
use ieee.std_logic_textio.all;
use std.textio.all;

package csv_util is

  constant CSV_LINE_LENGTH_MAX: integer := 256;
  subtype CSV_LINE_T is string(1 to CSV_LINE_LENGTH_MAX);

  -- Read until EOL or comma
  procedure read_csv_field(ln: inout LINE; ret: out string);

  -- Compare variable length strings
  function are_strings_equal (ln1: string; ln2: string) return boolean;

  -- Debug print text
  procedure print(text: string);

  -- converts STD_LOGIC into a string
  function str(b: STD_LOGIC) return string;

  -- converts std_logic_vector into a string
  function str(b: std_logic_vector) return string;

  -- Returns the first occurrence of a a given character
  function index_of_chr(ln: string; c: character) return integer;

  -- Returns the first occurrence of a null character
  function index_of_null(ln: string) return integer;

  -- Returns a substring, from start to finish
  function substr(ln: string; start: integer; finish: integer) return string;

  -- Trucates strings with embedded null characters
  function truncate(ln: string) return string;

end csv_util;

package body csv_util is

    procedure print(text: string) is
        variable msg: line;
    begin
        write(msg, text);
        writeline(output, msg);
    end print;

    procedure read_csv_field(ln: inout LINE; ret: out string) is
      variable return_string: CSV_LINE_T;
      variable read_char: character;
      variable read_ok: boolean := true;
      variable index: integer := 1;
    begin
      read(ln, read_char, read_ok);
      while read_ok loop
        if read_char = ',' then
          ret := return_string;
          return;
        else
          return_string(index) := read_char;
          index := index + 1;
        end if;
        read(ln, read_char, read_ok);
      end loop;

      ret := return_string;
    end;

    function index_of_chr(ln: string; c: character) return integer is
    begin
      for i in 1 to ln'length loop
        if ln(i) = c then
          return i;
        end if;
      end loop;

      return ln'length + 1;

    end;

    function index_of_null(ln: string) return integer is
    begin
      return index_of_chr(ln, NUL);
    end;

    function substr(ln: string; start: integer; finish: integer) return string is
    begin
      return ln(start to finish);
    end;

    function truncate(ln: string) return string is
    begin
      return substr(ln, 1, index_of_null(ln) - 1);
    end;

    function are_strings_equal(ln1: string; ln2: string) return boolean is
      variable lhs : string(1 to ln1'length) := ln1;
      variable rhs : string(1 to ln2'length) := ln2;
      variable maxlen : integer := ln1'length;
    begin
      if lhs'length = rhs'length and lhs'length = 0 then
        return true;
      else
        if ln2'length < maxlen then
          maxlen := ln2'length;
        end if;

        for i in 1 to maxlen loop
          if lhs(i) /= rhs(i) then
            return false;
          end if;
        end loop;

        if lhs'length > maxlen then
          if lhs(maxlen + 1) /= NUL then
            return false;
          end if;
        end if;

        if rhs'length > maxlen then
          if rhs(maxlen + 1) /= NUL then
            return false;
          end if;
        end if;

        return true;
      end if;
    end;

    -- converts STD_LOGIC into as string
    function str(b: std_logic) return string is
      variable s: string(1 to 1);
    begin
      case b is
        when 'U' => s(1):= 'U';
        when 'X' => s(1):= 'X';
        when '0' => s(1):= '0';
        when '1' => s(1):= '1';
        when 'Z' => s(1):= 'Z';
        when 'W' => s(1):= 'W';
        when 'L' => s(1):= 'L';
        when 'H' => s(1):= 'H';
        when '-' => s(1):= '-';
      end case;
      return s;
    end str;

    -- converts std_logic_vector into as string
    function str(b: std_logic_vector) return string is
      variable res: string(1 to b'length);
      variable v: std_logic_vector(1 to b'length) := b;
    begin
      if v(1) /= '1' and v(1) /= '0' then
        return  std_logic'image(v(1));
      else
        for i in 1 to b'length loop
          if v(i) = '0' then
            res(i) := '0';
          elsif v(i) = '1' then
            res(i) := '1';
          else
            res(i) := '-';
          end if;
        end loop;

        return res;
      end if;
    end str;

end package body csv_util;
|]
