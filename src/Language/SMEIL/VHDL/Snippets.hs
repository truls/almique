{-# LANGUAGE QuasiQuotes #-}
module Language.SMEIL.VHDL.Snippets
       ( header
       , clockedSignals
       , clockedMap
       , csvUtil
       , tbHeader
       , tbClockedMap
       , tbSignals
       , tbResetWait
       , typesHead
       , clkProcess
       , testerDecls
       , fieldCheck
       , valCheck
       )
       where

import Text.PrettyPrint

import Data.String.Here
--import Language.SMEIL.VHDL.Pretty

header :: Doc
header = text $ [i|library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

library sme_types;
use work.sme_types.all;

|]

tbHeader :: Doc
tbHeader = text [here|
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;

library sme_types;
use work.sme_types.all;

use work.csv_util.all;

|]

clockedSignals :: Doc
clockedSignals = text [i|-- Reset signal
  "rst: std_logic;

  -- Clock signal
  clk: std_logic;
|]

tbClockedMap :: Doc
tbClockedMap = text [i|rst => reset,
clk => clock|]

tbSignals :: Doc
tbSignals = text [i|
signal clock: std_logic;
signal stop_clock: boolean;
signal reset: std_logic;
|]

tbResetWait :: Doc
tbResetWait = text [i|
Reset <= '1';
wait for 5 ns;
reset <= '0';
|]

clkProcess :: Doc
clkProcess = text [i|clk: process
begin
  while not stop_clock loop
    clock <= '1';
    wait for 5 ns;
    clock <= '0';
    wait for 5 ns;
  end loop;
  wait;
end process;
|]

testerDecls :: Doc
testerDecls = text [i|
file F: TEXT;
variable L: LINE;
variable Status: FILE_OPEN_STATUS;
constant filename : string := "trace.csv";
variable clockcycle : integer := 0;
variable tmp : CSV_LINE_T;
variable readOK : boolean;
variable fieldno : integer := 0;
|]

fieldCheck :: Doc -> Doc
fieldCheck s = text [i|read_csv_field(L, tmp);
assert are_strings_equal(tmp, "${s}") report "Field #" & integer'image(fieldno) & " is named: " & truncate(tmp) & " but expected ${s}" severity Failure;
fieldno := fieldno + 1;
|]

valCheck :: Doc -> Doc
valCheck s = text [i|read_csv_field(L, tmp);
if not are_strings_equal(tmp, "U") then
  assert are_strings_equal(int_image(${s}), tmp) report "Unexpected value of ${s} in cycle " & integer'image(clockcycle) & ". Actual value was: " & int_image(${s}) & " but expected " & truncate(tmp) severity Error;
end if;
fieldno := fieldno + 1;
|]

clockedMap :: Doc
clockedMap = empty $+$ text "rst => rst," $+$ text "clk => clk"

-- systemTypes :: Doc
-- systemTypes = text [i|library IEEE;
-- use IEEE.STD_LOGIC_1164.ALL;
-- use IEEE.NUMERIC_STD.ALL;

-- |]
--   $+$ pp Package <+> text "SYSTEM_TYPES" <+> Is
--   $+$ indent ( pp Subtype <+> text "T_SYSTEM_BOOL" <+> 

typesHead :: Doc
typesHead = text [i|library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
|]

csvUtil :: Doc
csvUtil = text [here|
library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
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

-- converts std_logic_vector to integer to string
  function int_image(b: std_logic_vector) return string;

  -- converts std_logic_vector to integer to string
  function uint_image(b: std_logic_vector) return string;

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

    function int_image(b: std_logic_vector) return string is
      variable i: integer;
    begin
      i := to_integer(signed(b));
      return integer'image(i);
    end int_image;

    function uint_image(b: std_logic_vector) return string is
      variable i: integer;
    begin
      i := to_integer(unsigned(b));
      return integer'image(i);
    end uint_image;

end package body csv_util;
|]
