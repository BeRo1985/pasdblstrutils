(******************************************************************************
 *                               PasDblStrUtils                               *
 ******************************************************************************
 *                        Version 2021-06-04-01-33-0000                       *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2021, Benjamin Rosseaux (benjamin@rosseaux.de)          *
 *                                                                            *
 * This software is provided 'as-is', without any express or implied          *
 * warranty. In no event will the authors be held liable for any damages      *
 * arising from the use of this software.                                     *
 *                                                                            *
 * Permission is granted to anyone to use this software for any purpose,      *
 * including commercial applications, and to alter it and redistribute it     *
 * freely, subject to the following restrictions:                             *
 *                                                                            *
 * 1. The origin of this software must not be misrepresented; you must not    *
 *    claim that you wrote the original software. If you use this software    *
 *    in a product, an acknowledgement in the product documentation would be  *
 *    appreciated but is not required.                                        *
 * 2. Altered source versions must be plainly marked as such, and must not be *
 *    misrepresented as being the original software.                          *
 * 3. This notice may not be removed or altered from any source distribution. *
 *                                                                            *
 ******************************************************************************
 *                  General guidelines for code contributors                  *
 *============================================================================*
 *                                                                            *
 * 1. Make sure you are legally allowed to make a contribution under the zlib *
 *    license.                                                                *
 * 2. The zlib license header goes at the top of each source file, with       *
 *    appropriate copyright notice.                                           *
 * 3. After a pull request, check the status of your pull request on          *
      http://github.com/BeRo1985/pasdblstrutils                               *
 * 4. Write code, which is compatible with Delphi >=XE7 and FreePascal        *
 *    >= 3.0                                                                  *
 * 5. Don't use Delphi-only, FreePascal-only or Lazarus-only libraries/units, *
 *    but if needed, make it out-ifdef-able.                                  *
 * 6. No use of third-party libraries/units as possible, but if needed, make  *
 *    it out-ifdef-able.                                                      *
 * 7. Try to use const when possible.                                         *
 * 8. Make sure to comment out writeln, used while debugging.                 *
 * 9. Make sure the code compiles on 32-bit and 64-bit platforms (x86-32,     *
 *    x86-64, ARM, ARM64, etc.).                                              *
 * 10. Make sure the code runs on platforms with weak and strong memory       *
 *     models without any issues.                                             *
 *                                                                            *
 ******************************************************************************)
 unit PasDblStrUtils;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpui386}
  {$define cpu386}
 {$endif}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
 {$ifdef FPC_LITTLE_ENDIAN}
  {$define LITTLE_ENDIAN}
 {$else}
  {$ifdef FPC_BIG_ENDIAN}
   {$define BIG_ENDIAN}
  {$endif}
 {$endif}
 {-$pic off}
 {$define CanInline}
 {$ifdef FPC_HAS_TYPE_EXTENDED}
  {$define HAS_TYPE_EXTENDED}
 {$else}
  {$undef HAS_TYPE_EXTENDED}
 {$endif}
 {$ifdef FPC_HAS_TYPE_DOUBLE}
  {$define HAS_TYPE_DOUBLE}
 {$else}
  {$undef HAS_TYPE_DOUBLE}
 {$endif}
 {$ifdef FPC_HAS_TYPE_SINGLE}
  {$define HAS_TYPE_SINGLE}
 {$else}
  {$undef HAS_TYPE_SINGLE}
 {$endif}
 {$if declared(RawByteString)}
  {$define HAS_TYPE_RAWBYTESTRING}
 {$else}
  {$undef HAS_TYPE_RAWBYTESTRING}
 {$ifend}
 {$if declared(UTF8String)}
  {$define HAS_TYPE_UTF8STRING}
 {$else}
  {$undef HAS_TYPE_UTF8STRING}
 {$ifend}
 {$if declared(UnicodeString)}
  {$define HAS_TYPE_UNICODESTRING}
 {$else}
  {$undef HAS_TYPE_UNICODESTRING}
 {$ifend}
{$else}
 {$realcompatibility off}
 {$localsymbols on}
 {$define LITTLE_ENDIAN}
 {$ifndef cpu64}
  {$define cpu32}
 {$endif}
 {$define HAS_TYPE_EXTENDED}
 {$define HAS_TYPE_DOUBLE}
 {$define HAS_TYPE_SINGLE}
 {$ifdef conditionalexpressions}
  {$if declared(RawByteString)}
   {$define HAS_TYPE_RAWBYTESTRING}
  {$else}
   {$undef HAS_TYPE_RAWBYTESTRING}
  {$ifend}
  {$if declared(UTF8String)}
   {$define HAS_TYPE_UTF8STRING}
  {$else}
   {$undef HAS_TYPE_UTF8STRING}
  {$ifend}
  {$if declared(UnicodeString)}
   {$define HAS_TYPE_UNICODESTRING}
  {$else}
   {$undef HAS_TYPE_UNICODESTRING}
  {$ifend}
 {$else}
  {$undef HAS_TYPE_RAWBYTESTRING}
  {$undef HAS_TYPE_UTF8STRING}
  {$undef HAS_TYPE_UNICODESTRING}
 {$endif}
 {$ifndef BCB}
  {$ifdef ver120}
   {$define Delphi4or5}
  {$endif}
  {$ifdef ver130}
   {$define Delphi4or5}
  {$endif}
  {$ifdef ver140}
   {$define Delphi6}
  {$endif}
  {$ifdef ver150}
   {$define Delphi7}
  {$endif}
  {$ifdef ver170}
   {$define Delphi2005}
  {$endif}
 {$else}
  {$ifdef ver120}
   {$define Delphi4or5}
   {$define BCB4}
  {$endif}
  {$ifdef ver130}
   {$define Delphi4or5}
  {$endif}
 {$endif}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24}
   {$legacyifend on}
  {$ifend}
  {$if CompilerVersion>=14.0}
   {$if CompilerVersion=14.0}
    {$define Delphi6}
   {$ifend}
   {$define Delphi6AndUp}
  {$ifend}
  {$if CompilerVersion>=15.0}
   {$if CompilerVersion=15.0}
    {$define Delphi7}
   {$ifend}
   {$define Delphi7AndUp}
  {$ifend}
  {$if CompilerVersion>=17.0}
   {$if CompilerVersion=17.0}
    {$define Delphi2005}
   {$ifend}
   {$define Delphi2005AndUp}
  {$ifend}
  {$if CompilerVersion>=18.0}
   {$if CompilerVersion=18.0}
    {$define BDS2006}
    {$define Delphi2006}
   {$ifend}
   {$define Delphi2006AndUp}
  {$ifend}
  {$if CompilerVersion>=18.5}
   {$if CompilerVersion=18.5}
    {$define Delphi2007}
   {$ifend}
   {$define Delphi2007AndUp}
  {$ifend}
  {$if CompilerVersion=19.0}
   {$define Delphi2007Net}
  {$ifend}
  {$if CompilerVersion>=20.0}
   {$if CompilerVersion=20.0}
    {$define Delphi2009}
   {$ifend}
   {$define Delphi2009AndUp}
  {$ifend}
  {$if CompilerVersion>=21.0}
   {$if CompilerVersion=21.0}
    {$define Delphi2010}
   {$ifend}
   {$define Delphi2010AndUp}
  {$ifend}
  {$if CompilerVersion>=22.0}
   {$if CompilerVersion=22.0}
    {$define DelphiXE}
   {$ifend}
   {$define DelphiXEAndUp}
  {$ifend}
  {$if CompilerVersion>=23.0}
   {$if CompilerVersion=23.0}
    {$define DelphiXE2}
   {$ifend}
   {$define DelphiXE2AndUp}
  {$ifend}
  {$if CompilerVersion>=24.0}
   {$if CompilerVersion=24.0}
    {$define DelphiXE3}
   {$ifend}
   {$define DelphiXE3AndUp}
  {$ifend}
  {$if CompilerVersion>=25.0}
   {$if CompilerVersion=25.0}
    {$define DelphiXE4}
   {$ifend}
   {$define DelphiXE4AndUp}
  {$ifend}
  {$if CompilerVersion>=26.0}
   {$if CompilerVersion=26.0}
    {$define DelphiXE5}
   {$ifend}
   {$define DelphiXE5AndUp}
  {$ifend}
  {$if CompilerVersion>=27.0}
   {$if CompilerVersion=27.0}
    {$define DelphiXE6}
   {$ifend}
   {$define DelphiXE6AndUp}
  {$ifend}
  {$if CompilerVersion>=28.0}
   {$if CompilerVersion=28.0}
    {$define DelphiXE7}
   {$ifend}
   {$define DelphiXE7AndUp}
  {$ifend}
  {$if CompilerVersion>=29.0}
   {$if CompilerVersion=29.0}
    {$define DelphiXE8}
   {$ifend}
   {$define DelphiXE8AndUp}
  {$ifend}
  {$if CompilerVersion>=30.0}
   {$if CompilerVersion=30.0}
    {$define Delphi10Seattle}
   {$ifend}
   {$define Delphi10SeattleAndUp}
  {$ifend}
  {$if CompilerVersion>=31.0}
   {$if CompilerVersion=31.0}
    {$define Delphi10Berlin}
   {$ifend}
   {$define Delphi10BerlinAndUp}
  {$ifend}
 {$endif}
 {$ifndef Delphi4or5}
  {$ifndef BCB}
   {$define Delphi6AndUp}
  {$endif}
   {$ifndef Delphi6}
    {$define BCB6OrDelphi7AndUp}
    {$ifndef BCB}
     {$define Delphi7AndUp}
    {$endif}
    {$ifndef BCB}
     {$ifndef Delphi7}
      {$ifndef Delphi2005}
       {$define BDS2006AndUp}
      {$endif}
     {$endif}
    {$endif}
   {$endif}
 {$endif}
 {$ifdef Delphi6AndUp}
  {$warn symbol_platform off}
  {$warn symbol_deprecated off}
 {$endif}
{$endif}
{$ifdef win32}
 {$define windows}
{$endif}
{$ifdef win64}
 {$define windows}
{$endif}
{$ifdef wince}
 {$define windows}
{$endif}
{$ifndef HAS_TYPE_DOUBLE}
 {$error No double floating point precision}
{$endif}
{$rangechecks off}
{$extendedsyntax on}
{$writeableconst on}
{$hints off}
{$booleval off}
{$typedaddress off}
{$stackframes off}
{$varstringchecks on}
{$typeinfo on}
{$overflowchecks off}
{$longstrings on}
{$openstrings on}

interface

uses SysUtils,Math;

type PPasDblStrUtilsInt8=^TPasDblStrUtilsInt8;
     TPasDblStrUtilsInt8={$ifdef fpc}Int8{$else}ShortInt{$endif};

     PPasDblStrUtilsUInt8=^TPasDblStrUtilsUInt8;
     TPasDblStrUtilsUInt8={$ifdef fpc}UInt8{$else}Byte{$endif};

     PPasDblStrUtilsInt16=^TPasDblStrUtilsInt16;
     TPasDblStrUtilsInt16={$ifdef fpc}Int16{$else}SmallInt{$endif};

     PPasDblStrUtilsUInt16=^TPasDblStrUtilsUInt16;
     TPasDblStrUtilsUInt16={$ifdef fpc}UInt16{$else}Word{$endif};

     PPasDblStrUtilsInt32=^TPasDblStrUtilsInt32;
     TPasDblStrUtilsInt32={$ifdef fpc}Int32{$else}LongInt{$endif};

     PPasDblStrUtilsUInt32=^TPasDblStrUtilsUInt32;
     TPasDblStrUtilsUInt32={$ifdef fpc}UInt32{$else}LongWord{$endif};

     PPasDblStrUtilsInt64=^TPasDblStrUtilsInt64;
     TPasDblStrUtilsInt64=Int64;

     PPasDblStrUtilsUInt64=^TPasDblStrUtilsUInt64;
     TPasDblStrUtilsUInt64=UInt64;

     PPasDblStrUtilsDouble=^TPasDblStrUtilsDouble;
     TPasDblStrUtilsDouble=Double;

     PPasDblStrUtilsBoolean=^TPasDblStrUtilsBoolean;
     TPasDblStrUtilsBoolean=Boolean;

     PPasDblStrUtilsPtrUInt=^TPasDblStrUtilsPtrUInt;
     PPasDblStrUtilsPtrInt=^TPasDblStrUtilsPtrInt;

{$ifdef fpc}
     TPasDblStrUtilsPtrUInt=PtrUInt;
     TPasDblStrUtilsPtrInt=PtrInt;
{$else}
{$if Declared(CompilerVersion) and (CompilerVersion>=23.0)}
     TPasDblStrUtilsPtrUInt=NativeUInt;
     TPasDblStrUtilsPtrInt=NativeInt;
{$else}
{$ifdef cpu64}
     TPasDblStrUtilsPtrUInt=TPasDblStrUtilsUInt64;
     TPasDblStrUtilsPtrInt=TPasDblStrUtilsInt64;
{$else}
     TPasDblStrUtilsPtrUInt=TPasDblStrUtilsUInt32;
     TPasDblStrUtilsPtrInt=TPasDblStrUtilsInt32;
{$endif}
{$ifend}
{$endif}

     PPasDblStrUtilsNativeUInt=^TPasDblStrUtilsNativeUInt;
     PPasDblStrUtilsNativeInt=^TPasDblStrUtilsNativeInt;
     TPasDblStrUtilsNativeUInt=TPasDblStrUtilsPtrUInt;
     TPasDblStrUtilsNativeInt=TPasDblStrUtilsPtrInt;

     PPasDblStrUtilsRawByteChar=PAnsiChar;
     TPasDblStrUtilsRawByteChar=AnsiChar;

     PPasDblStrUtilsRawByteCharSet=^TPasDblStrUtilsRawByteCharSet;
     TPasDblStrUtilsRawByteCharSet=set of TPasDblStrUtilsRawByteChar;

     PPasDblStrUtilsRawByteString=^TPasDblStrUtilsRawByteString;
     TPasDblStrUtilsRawByteString={$ifdef HAS_TYPE_RAWBYTESTRING}RawByteString{$else}AnsiString{$endif};

     PPasDblStrUtilsUTF8Char=PAnsiChar;
     TPasDblStrUtilsUTF8Char=AnsiChar;

     PPasDblStrUtilsUTF8String=^TPasDblStrUtilsUTF8String;
     TPasDblStrUtilsUTF8String={$ifdef HAS_TYPE_UTF8STRING}UTF8String{$else}AnsiString{$endif};

     PPasDblStrUtilsUTF16Char={$ifdef HAS_TYPE_UNICODESTRING}{$ifdef fpc}PUnicodeChar{$else}PWideChar{$endif}{$else}PWideChar{$endif};
     TPasDblStrUtilsUTF16Char={$ifdef HAS_TYPE_UNICODESTRING}{$ifdef fpc}UnicodeChar{$else}WideChar{$endif}{$else}WideChar{$endif};

     PPasDblStrUtilsUTF16String=^TPasDblStrUtilsUTF16String;
     TPasDblStrUtilsUTF16String={$ifdef HAS_TYPE_UNICODESTRING}UnicodeString{$else}WideString{$endif};

     PPasDblStrUtilsChar=PAnsiChar;
     TPasDblStrUtilsChar=AnsiChar;

     PPasDblStrUtilsString=^TPasDblStrUtilsString;
     TPasDblStrUtilsString={$ifdef HAS_TYPE_RAWBYTESTRING}RawByteString{$else}AnsiString{$endif};

     PPasDblStrUtilsPointer=^TPasDblStrUtilsPointer;
     TPasDblStrUtilsPointer=Pointer;

     PPasDblStrUtilsRoundingMode=^TPasDblStrUtilsRoundingMode;
     TPasDblStrUtilsRoundingMode=type TFPURoundingMode;

     TPasDblStrUtilsOutputMode=(
      omStandard,
      omStandardExponential,
      omFixed,
      omExponential,
      omPrecision,
      omRadix
     );

function RyuStringToDouble(const aStringValue:TPasDblStrUtilsString;const aOK:PPasDblStrUtilsBoolean=nil;const aStrict:boolean=false):TPasDblStrUtilsDouble;

function RyuDoubleToString(const aValue:TPasDblStrUtilsDouble;const aExponential:boolean=true):TPasDblStrUtilsString;

function ConvertStringToDouble(const StringValue:TPasDblStrUtilsString;const RoundingMode:TPasDblStrUtilsRoundingMode=rmNearest;const OK:PPasDblStrUtilsBoolean=nil;const Base:TPasDblStrUtilsInt32=-1):TPasDblStrUtilsDouble;

function ConvertDoubleToString(const aValue:TPasDblStrUtilsDouble;const OutputMode:TPasDblStrUtilsOutputMode=omStandard;RequestedDigits:TPasDblStrUtilsInt32=-1):TPasDblStrUtilsString;

implementation

type PDoubleHiLo=^TDoubleHiLo;
     TDoubleHiLo=packed record
{$ifdef BIG_ENDIAN}
      Hi,Lo:TPasDblStrUtilsUInt32;
{$else}
      Lo,Hi:TPasDblStrUtilsUInt32;
{$endif}
     end;

     PDoubleBytes=^TDoubleBytes;
     TDoubleBytes=array[0..sizeof(TPasDblStrUtilsDouble)-1] of TPasDblStrUtilsUInt8;

{$ifdef cpu64}
function IsNaN(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=((PPasDblStrUtilsInt64(@AValue)^ and $7ff0000000000000)=$7ff0000000000000) and ((PPasDblStrUtilsInt64(@AValue)^ and $000fffffffffffff)<>$0000000000000000);
end;

function IsInfinite(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PPasDblStrUtilsInt64(@AValue)^ and $7fffffffffffffff)=$7ff0000000000000;
end;

function IsFinite(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PPasDblStrUtilsInt64(@AValue)^ and $7ff0000000000000)<>$7ff0000000000000;
end;

function IsPosInfinite(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=PPasDblStrUtilsInt64(@AValue)^=TPasDblStrUtilsInt64($7ff0000000000000);
end;

function IsNegInfinite(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
{$ifdef fpc}
 result:=TPasDblStrUtilsUInt64(pointer(@AValue)^)=TPasDblStrUtilsUInt64($fff0000000000000);
{$else}
 result:=PPasDblStrUtilsInt64(@AValue)^=TPasDblStrUtilsInt64($fff0000000000000);
{$endif}
end;

function IsPosZero(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=PPasDblStrUtilsInt64(@AValue)^=TPasDblStrUtilsInt64($0000000000000000);
end;

function IsNegZero(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
{$ifdef fpc}
 result:=TPasDblStrUtilsUInt64(pointer(@AValue)^)=TPasDblStrUtilsUInt64($8000000000000000);
{$else}
 result:=PPasDblStrUtilsInt64(@AValue)^=TPasDblStrUtilsInt64($8000000000000000);
{$endif}
end;

function IsZero(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
{$ifdef fpc}
 result:=(TPasDblStrUtilsUInt64(pointer(@AValue)^) and TPasDblStrUtilsUInt64($7fffffffffffffff))=TPasDblStrUtilsUInt64($0000000000000000);
{$else}
 result:=(PPasDblStrUtilsInt64(@AValue)^ and TPasDblStrUtilsInt64($7fffffffffffffff))=TPasDblStrUtilsInt64($0000000000000000);
{$endif}
end;

function IsNegative(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
{$ifdef fpc}
 result:=(TPasDblStrUtilsUInt64(pointer(@AValue)^) and TPasDblStrUtilsUInt64($8000000000000000))<>0;
{$else}
 result:=(PPasDblStrUtilsInt64(@AValue)^ shr 63)<>0;
{$endif}
end;
{$else}
{$ifdef TrickyNumberChecks}
function IsNaN(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
var l:TPasDblStrUtilsUInt32;
begin
 l:=PDoubleHiLo(@AValue)^.Lo;
 result:=(TPasDblStrUtilsUInt32($7ff00000-TPasDblStrUtilsUInt32(TPasDblStrUtilsUInt32(PDoubleHiLo(@AValue)^.Hi and $7fffffff) or ((l or (-l)) shr 31))) shr 31)<>0;
end;

function IsInfinite(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=TPasDblStrUtilsUInt32((TPasDblStrUtilsUInt32(PDoubleHiLo(@AValue)^.Hi and $7fffffff) xor $7ff00000) or PDoubleHiLo(@AValue)^.Lo)=0;
end;

function IsFinite(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(TPasDblStrUtilsUInt32((PDoubleHiLo(@AValue)^.Hi and $7fffffff)-$7ff00000) shr 31)<>0;
end;

function IsPosInfinite(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
var h:TPasDblStrUtilsUInt32;
begin
 h:=PDoubleHiLo(@AValue)^.Hi;
 result:=TPasDblStrUtilsUInt32(((TPasDblStrUtilsUInt32(h and $7fffffff) xor $7ff00000) or PDoubleHiLo(@AValue)^.Lo) or TPasDblStrUtilsUInt32(h shr 31))=0;
end;

function IsNegInfinite(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
var h:TPasDblStrUtilsUInt32;
begin
 h:=PDoubleHiLo(@AValue)^.Hi;
 result:=TPasDblStrUtilsUInt32(((TPasDblStrUtilsUInt32(h and $7fffffff) xor $7ff00000) or PDoubleHiLo(@AValue)^.Lo) or TPasDblStrUtilsUInt32(TPasDblStrUtilsUInt32(not h) shr 31))=0;
end;

function IsPosZero(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
var h:TPasDblStrUtilsUInt32;
begin
 h:=PDoubleHiLo(@AValue)^.Hi;
 result:=TPasDblStrUtilsUInt32(TPasDblStrUtilsUInt32(TPasDblStrUtilsUInt32(h and $7fffffff) or PDoubleHiLo(@AValue)^.Lo) or TPasDblStrUtilsUInt32(h shr 31))=0;
end;

function IsNegZero(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
var h:TPasDblStrUtilsUInt32;
begin
 h:=PDoubleHiLo(@AValue)^.Hi;
 result:=TPasDblStrUtilsUInt32(TPasDblStrUtilsUInt32(TPasDblStrUtilsUInt32(h and $7fffffff) or PDoubleHiLo(@AValue)^.Lo) or TPasDblStrUtilsUInt32(TPasDblStrUtilsUInt32(not h) shr 31))=0;
end;

function IsZero(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=TPasDblStrUtilsUInt32(TPasDblStrUtilsUInt32(PDoubleHiLo(@AValue)^.Hi and $7fffffff) or PDoubleHiLo(@AValue)^.Lo)=0;
end;

function IsNegative(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=TPasDblStrUtilsUInt32(PDoubleHiLo(@AValue)^.Hi and TPasDblStrUtilsUInt32($80000000))<>0;
end;
{$else}
function IsNaN(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=((PDoubleHiLo(@AValue)^.Hi and $7ff00000)=$7ff00000) and (((PDoubleHiLo(@AValue)^.Hi and $000fffff) or PDoubleHiLo(@AValue)^.Lo)<>0);
end;

function IsInfinite(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=((PDoubleHiLo(@AValue)^.Hi and $7fffffff)=$7ff00000) and (PDoubleHiLo(@AValue)^.Lo=0);
end;

function IsFinite(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PDoubleHiLo(@AValue)^.Hi and $7ff00000)<>$7ff00000;
end;

function IsPosInfinite(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PDoubleHiLo(@AValue)^.Hi=$7ff00000) and (PDoubleHiLo(@AValue)^.Lo=0);
end;

function IsNegInfinite(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PDoubleHiLo(@AValue)^.Hi=$fff00000) and (PDoubleHiLo(@AValue)^.Lo=0);
end;

function IsPosZero(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PDoubleHiLo(@AValue)^.Hi or PDoubleHiLo(@AValue)^.Lo)=0;
end;

function IsNegZero(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PDoubleHiLo(@AValue)^.Hi=$80000000) and (PDoubleHiLo(@AValue)^.Lo=0);
end;

function IsZero(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=((PDoubleHiLo(@AValue)^.Hi and $7fffffff) or PDoubleHiLo(@AValue)^.Lo)=0;
end;

function IsNegative(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PDoubleHiLo(@AValue)^.Hi and $80000000)<>0;
end;
{$endif}
{$endif}

function DoubleAbsolute(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsDouble; {$ifdef caninline}inline;{$endif}
begin
{$ifdef cpu64}
 PPasDblStrUtilsInt64(@result)^:=PPasDblStrUtilsInt64(@AValue)^ and $7fffffffffffffff;
{$else}
 PDoubleHiLo(@result)^.Hi:=PDoubleHiLo(@AValue)^.Hi and $7fffffff;
 PDoubleHiLo(@result)^.Lo:=PDoubleHiLo(@AValue)^.Lo;
{$endif}
end;

function CLZQWord(aValue:TPasDblStrUtilsUInt64):TPasDblStrUtilsInt32;
{$if declared(BSRQWord)}
begin
 if aValue=0 then begin
  result:=0;
 end else begin
  result:=63-BSRQWord(aValue);
 end;
end;
{$else}
const CLZDebruijn64Multiplicator:TPasDblStrUtilsUInt64=TPasDblStrUtilsUInt64($03f79d71b4cb0a89);
      CLZDebruijn64Shift=58;
      CLZDebruijn64Mask=63;
      CLZDebruijn64Table:array[0..63] of TPasDblStrUtilsInt32=(63,16,62,7,15,36,61,3,6,14,22,26,35,47,60,2,9,5,28,11,13,21,42,19,25,31,34,40,46,52,59,1,
                                                                    17,8,37,4,23,27,48,10,29,12,43,20,32,41,53,18,38,24,49,30,44,33,54,39,50,45,55,51,56,57,58,0);

begin
 if aValue=0 then begin
  result:=64;
 end else begin
  aValue:=aValue or (aValue shr 1);
  aValue:=aValue or (aValue shr 2);
  aValue:=aValue or (aValue shr 4);
  aValue:=aValue or (aValue shr 8);
  aValue:=aValue or (aValue shr 16);
  aValue:=aValue or (aValue shr 32);
  result:=CLZDebruijn64Table[((aValue*CLZDebruijn64Multiplicator) shr CLZDebruijn64Shift) and CLZDebruijn64Mask];
 end;
end;
{$ifend}

function CTZQWord(aValue:TPasDblStrUtilsUInt64):TPasDblStrUtilsInt32;
{$if declared(BSFQWord)}
begin
 if aValue=0 then begin
  result:=64;
 end else begin
  result:=BSFQWord(aValue);
 end;
end;
{$else}
const CTZDebruijn64Multiplicator:TPasDblStrUtilsUInt64=TPasDblStrUtilsUInt64($07edd5e59a4e28c2);
      CTZDebruijn64Shift=58;
      CTZDebruijn64Mask=63;
      CTZDebruijn64Table:array[0..63] of TPasDblStrUtilsInt32=(63,0,58,1,59,47,53,2,60,39,48,27,54,33,42,3,61,51,37,40,49,18,28,20,55,30,34,11,43,14,22,4,
                                                           62,57,46,52,38,26,32,41,50,36,17,19,29,10,13,21,56,45,25,31,35,16,9,12,44,24,15,8,23,7,6,5);
begin
 if aValue=0 then begin
  result:=64;
 end else begin
  result:=CTZDebruijn64Table[(((aValue and (-aValue))*CTZDebruijn64Multiplicator) shr CTZDebruijn64Shift) and CTZDebruijn64Mask];
 end;
end;
{$ifend}

{$if not declared(BSRQWord)}
function BSRQWord(aValue:TPasDblStrUtilsUInt64):TPasDblStrUtilsInt32;
const BSRDebruijn64Multiplicator:TPasDblStrUtilsUInt64=TPasDblStrUtilsUInt64($03f79d71b4cb0a89);
      BSRDebruijn64Shift=58;
      BSRDebruijn64Mask=63;
      BSRDebruijn64Table:array[0..63] of TPasDblStrUtilsInt32=(0,47,1,56,48,27,2,60,57,49,41,37,28,16,3,61,54,58,35,52,50,42,21,44,38,32,29,23,17,11,4,62,
                                                           46,55,26,59,40,36,15,53,34,51,20,43,31,22,10,45,25,39,14,33,19,30,9,24,13,18,8,12,7,6,5,63);


begin
 if aValue=0 then begin
  result:=255;
 end else begin
  aValue:=aValue or (aValue shr 1);
  aValue:=aValue or (aValue shr 2);
  aValue:=aValue or (aValue shr 4);
  aValue:=aValue or (aValue shr 8);
  aValue:=aValue or (aValue shr 16);
  aValue:=aValue or (aValue shr 32);
  result:=BSRDebruijn64Table[((aValue*BSRDebruijn64Multiplicator) shr BSRDebruijn64Shift) and BSRDebruijn64Mask];
 end;
end;
{$ifend}

function FloorLog2(const aValue:TPasDblStrUtilsUInt64):TPasDblStrUtilsUInt32; {$ifdef caninline}inline;{$endif}
{$if declared(BSRQWord)}
begin
 result:=BSRQWord(aValue);
end;
{$else}
begin
 result:=63-CLZQWord(aValue);
end;
{$ifend}

{$if not declared(TPasDblStrUtilsUInt128)}
function UMul128(const a,b:TPasDblStrUtilsUInt64;out aProductHi:TPasDblStrUtilsUInt64):TPasDblStrUtilsUInt64;
var u0,u1,v0,v1,t,w0,w1,w2:TPasDblStrUtilsUInt64;
begin
 u1:=a shr 32;
 u0:=a and UInt64($ffffffff);
 v1:=b shr 32;
 v0:=b and UInt64($ffffffff);
 t:=u0*v0;
 w0:=t and UInt64($ffffffff);
 t:=(u1*v0)+(t shr 32);
 w1:=t and UInt64($ffffffff);
 w2:=t shr 32;
 t:=(u0*v1)+w1;
 aProductHi:=((u1*v1)+w2)+(t shr 32);
 result:=(t shl 32)+w0;
end;

function ShiftRight128(const aLo,aHi:TPasDblStrUtilsUInt64;const aShift:TPasDblStrUtilsUInt32):TPasDblStrUtilsUInt64;
begin
 result:=(aHi shl (64-aShift)) or (aLo shr aShift);
end;
{$ifend}

type TPasDblStrUtilsUInt128=packed record
      public
       constructor Create(const aHi,aLo:TPasDblStrUtilsUInt64);
       function CountLeadingZeroBits:TPasDblStrUtilsInt32;
       function CountTrailingZeroBits:TPasDblStrUtilsInt32;
       function FloorLog2:TPasDblStrUtilsInt32;
       class operator Implicit(const a:TPasDblStrUtilsUInt64):TPasDblStrUtilsUInt128; {$ifdef caninline}inline;{$endif}
       class operator Implicit(const a:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt64; {$ifdef caninline}inline;{$endif}
       class operator Explicit(const a:TPasDblStrUtilsUInt64):TPasDblStrUtilsUInt128; {$ifdef caninline}inline;{$endif}
       class operator Explicit(const a:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt64; {$ifdef caninline}inline;{$endif}
       class operator Inc(const a:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt128; {$ifdef caninline}inline;{$endif}
       class operator Dec(const a:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt128; {$ifdef caninline}inline;{$endif}
       class operator Add(const a,b:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt128; {$ifdef caninline}inline;{$endif}
       class operator Subtract(const a,b:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt128; {$ifdef caninline}inline;{$endif}
       class operator LeftShift(const a:TPasDblStrUtilsUInt128;Shift:TPasDblStrUtilsInt32):TPasDblStrUtilsUInt128;
       class operator RightShift(const a:TPasDblStrUtilsUInt128;Shift:TPasDblStrUtilsInt32):TPasDblStrUtilsUInt128;
       class operator BitwiseAnd(const a,b:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt128; {$ifdef caninline}inline;{$endif}
       class operator BitwiseOr(const a,b:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt128; {$ifdef caninline}inline;{$endif}
       class operator BitwiseXor(const a,b:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt128; {$ifdef caninline}inline;{$endif}
       class operator LogicalNot(const a:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt128; {$ifdef caninline}inline;{$endif}
       class operator Negative(const a:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt128; {$ifdef caninline}inline;{$endif}
       class operator Positive(const a:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt128; {$ifdef caninline}inline;{$endif}
       class operator Equal(const a,b:TPasDblStrUtilsUInt128):boolean; {$ifdef caninline}inline;{$endif}
       class operator NotEqual(const a,b:TPasDblStrUtilsUInt128):boolean; {$ifdef caninline}inline;{$endif}
       class operator GreaterThan(const a,b:TPasDblStrUtilsUInt128):boolean; {$ifdef caninline}inline;{$endif}
       class operator GreaterThanOrEqual(const a,b:TPasDblStrUtilsUInt128):boolean; {$ifdef caninline}inline;{$endif}
       class operator LessThan(const a,b:TPasDblStrUtilsUInt128):boolean; {$ifdef caninline}inline;{$endif}
       class operator LessThanOrEqual(const a,b:TPasDblStrUtilsUInt128):boolean; {$ifdef caninline}inline;{$endif}
       class function Mul64(const a,b:TPasDblStrUtilsUInt64):TPasDblStrUtilsUInt128; static;
       class operator Multiply(const a,b:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt128;
       class procedure BinaryDivMod128(Dividend,Divisor:TPasDblStrUtilsUInt128;out Quotient,Remainder:TPasDblStrUtilsUInt128); static;
       class procedure BinaryDivMod64(const Dividend:TPasDblStrUtilsUInt128;const Divisor:TPasDblStrUtilsUInt64;out Quotient:TPasDblStrUtilsUInt128;out Remainder:TPasDblStrUtilsUInt64); static;
       class procedure DivMod64(const Dividend:TPasDblStrUtilsUInt128;const Divisor:TPasDblStrUtilsUInt64;out Quotient,Remainder:TPasDblStrUtilsUInt64); static;
       class procedure DivMod128Ex(Dividend,Divisor:TPasDblStrUtilsUInt128;out Quotient,Remainder:TPasDblStrUtilsUInt128); static;
       class procedure DivMod128(Dividend,Divisor:TPasDblStrUtilsUInt128;out Quotient,Remainder:TPasDblStrUtilsUInt128); static;
       class operator IntDivide(const Dividend:TPasDblStrUtilsUInt128;const Divisor:TPasDblStrUtilsUInt64):TPasDblStrUtilsUInt128;
       class operator IntDivide(const Dividend,Divisor:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt128;
       class operator Modulus(const Dividend:TPasDblStrUtilsUInt128;const Divisor:TPasDblStrUtilsUInt64):TPasDblStrUtilsUInt128;
       class operator Modulus(const Dividend,Divisor:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt128;
{$ifdef BIG_ENDIAN}
       case byte of
        0:(
         Hi,Lo:TPasDblStrUtilsUInt64;
        );
        1:(
         Q3,Q2,Q1,Q0:TPasDblStrUtilsUInt32;
        );
{$else}
       case byte of
        0:(
         Lo,Hi:TPasDblStrUtilsUInt64;
        );
        1:(
         Q0,Q1,Q2,Q3:TPasDblStrUtilsUInt32;
        );
 {$endif}
     end;

constructor TPasDblStrUtilsUInt128.Create(const aHi,aLo:TPasDblStrUtilsUInt64);
begin
 Hi:=aHi;
 Lo:=aLo;
end;

function TPasDblStrUtilsUInt128.CountLeadingZeroBits:TPasDblStrUtilsInt32;
begin
 if Hi=0 then begin
  if Lo=0 then begin
   result:=64;
  end else begin
   result:=CLZQWord(Lo)+64;
  end;
 end else begin
  result:=CLZQWord(Hi);
 end;
end;

function TPasDblStrUtilsUInt128.CountTrailingZeroBits:TPasDblStrUtilsInt32;
begin
 if Lo=0 then begin
  result:=CTZQWord(Hi)+64;
 end else begin
  result:=CTZQWord(Lo);
 end;
end;

function TPasDblStrUtilsUInt128.FloorLog2:TPasDblStrUtilsInt32;
begin
 result:=127-CountLeadingZeroBits;
end;

class operator TPasDblStrUtilsUInt128.Implicit(const a:TPasDblStrUtilsUInt64):TPasDblStrUtilsUInt128;
begin
 result.Hi:=0;
 result.Lo:=a;
end;

class operator TPasDblStrUtilsUInt128.Implicit(const a:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt64;
begin
 result:=a.Lo;
end;

class operator TPasDblStrUtilsUInt128.Explicit(const a:TPasDblStrUtilsUInt64):TPasDblStrUtilsUInt128;
begin
 result.Hi:=0;
 result.Lo:=a;
end;

class operator TPasDblStrUtilsUInt128.Explicit(const a:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt64;
begin
 result:=a.Lo;
end;

class operator TPasDblStrUtilsUInt128.Inc(const a:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt128;
begin
 result.Lo:=a.Lo+1;
 result.Hi:=a.Hi+(((a.Lo xor result.Lo) and a.Lo) shr 63);
end;

class operator TPasDblStrUtilsUInt128.Dec(const a:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt128;
begin
 result.Lo:=a.Lo-1;
 result.Hi:=a.Hi-(((result.Lo xor a.Lo) and result.Lo) shr 63);
end;

class operator TPasDblStrUtilsUInt128.Add(const a,b:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt128;
begin
 result.Hi:=a.Hi+b.Hi+((((a.Lo and b.Lo) and 1)+(a.Lo shr 1)+(b.Lo shr 1)) shr 63);
 result.Lo:=a.Lo+b.Lo;
end;

class operator TPasDblStrUtilsUInt128.Subtract(const a,b:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt128;
begin
 result.Lo:=a.Lo-b.Lo;
 result.Hi:=a.Hi-(b.Hi+((((result.Lo and b.Lo) and 1)+(b.Lo shr 1)+(result.Lo shr 1)) shr 63));
end;

class operator TPasDblStrUtilsUInt128.LeftShift(const a:TPasDblStrUtilsUInt128;Shift:TPasDblStrUtilsInt32):TPasDblStrUtilsUInt128;
var m0,m1:TPasDblStrUtilsUInt64;
begin
 Shift:=Shift and 127;
 m0:=((((Shift+127) or Shift) and 64) shr 6)-1;
 m1:=(Shift shr 6)-1;
 Shift:=Shift and 63;
 result.Hi:=(a.Lo shl Shift) and not m1;
 result.Lo:=(a.Lo shl Shift) and m1;
 result.Hi:=result.Hi or (((a.Hi shl Shift) or ((a.Lo shr (64-Shift)) and m0)) and m1);
end;

class operator TPasDblStrUtilsUInt128.RightShift(const a:TPasDblStrUtilsUInt128;Shift:TPasDblStrUtilsInt32):TPasDblStrUtilsUInt128;
var m0,m1:TPasDblStrUtilsUInt64;
begin
 Shift:=Shift and 127;
 m0:=((((Shift+127) or Shift) and 64) shr 6)-1;
 m1:=(Shift shr 6)-1;
 Shift:=Shift and 63;
 result.Lo:=(a.Hi shr Shift) and not m1;
 result.Hi:=(a.Hi shr Shift) and m1;
 result.Lo:=result.Lo or (((a.Lo shr Shift) or ((a.Hi shl (64-Shift)) and m0)) and m1);
end;

class operator TPasDblStrUtilsUInt128.BitwiseAnd(const a,b:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt128;
begin
 result.Hi:=a.Hi and b.Hi;
 result.Lo:=a.Lo and b.Lo;
end;

class operator TPasDblStrUtilsUInt128.BitwiseOr(const a,b:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt128;
begin
 result.Hi:=a.Hi or b.Hi;
 result.Lo:=a.Lo or b.Lo;
end;

class operator TPasDblStrUtilsUInt128.BitwiseXor(const a,b:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt128;
begin
 result.Hi:=a.Hi xor b.Hi;
 result.Lo:=a.Lo xor b.Lo;
end;

class operator TPasDblStrUtilsUInt128.LogicalNot(const a:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt128;
begin
 result.Hi:=not a.Hi;
 result.Lo:=not a.Lo;
end;

class operator TPasDblStrUtilsUInt128.Negative(const a:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt128;
var Temporary:TPasDblStrUtilsUInt128;
begin
 Temporary.Hi:=not a.Hi;
 Temporary.Lo:=not a.Lo;
 result.Lo:=Temporary.Lo+1;
 result.Hi:=Temporary.Hi+(((Temporary.Lo xor result.Lo) and Temporary.Lo) shr 63);
end;

class operator TPasDblStrUtilsUInt128.Positive(const a:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt128;
begin
 result.Hi:=a.Hi;
 result.Lo:=a.Lo;
end;

class operator TPasDblStrUtilsUInt128.Equal(const a,b:TPasDblStrUtilsUInt128):boolean;
begin
 result:=(a.Hi=b.Hi) and (a.Lo=b.Lo);
end;

class operator TPasDblStrUtilsUInt128.NotEqual(const a,b:TPasDblStrUtilsUInt128):boolean;
begin
 result:=(a.Hi<>b.Hi) or (a.Lo<>b.Lo);
end;

class operator TPasDblStrUtilsUInt128.GreaterThan(const a,b:TPasDblStrUtilsUInt128):boolean;
begin
 result:=(a.Hi>b.Hi) or ((a.Hi=b.Hi) and (a.Lo>b.Lo));
end;

class operator TPasDblStrUtilsUInt128.GreaterThanOrEqual(const a,b:TPasDblStrUtilsUInt128):boolean;
begin
 result:=(a.Hi>b.Hi) or ((a.Hi=b.Hi) and (a.Lo>=b.Lo));
end;

class operator TPasDblStrUtilsUInt128.LessThan(const a,b:TPasDblStrUtilsUInt128):boolean;
begin
 result:=(a.Hi<b.Hi) or ((a.Hi=b.Hi) and (a.Lo<b.Lo));
end;

class operator TPasDblStrUtilsUInt128.LessThanOrEqual(const a,b:TPasDblStrUtilsUInt128):boolean;
begin
 result:=(a.Hi<=b.Hi) or ((a.Hi=b.Hi) and (a.Lo<=b.Lo));
end;

class function TPasDblStrUtilsUInt128.Mul64(const a,b:TPasDblStrUtilsUInt64):TPasDblStrUtilsUInt128;
var u0,u1,v0,v1,t,w0,w1,w2:TPasDblStrUtilsUInt64;
begin
 u1:=a shr 32;
 u0:=a and TPasDblStrUtilsUInt64($ffffffff);
 v1:=b shr 32;
 v0:=b and TPasDblStrUtilsUInt64($ffffffff);
 t:=u0*v0;
 w0:=t and TPasDblStrUtilsUInt64($ffffffff);
 t:=(u1*v0)+(t shr 32);
 w1:=t and TPasDblStrUtilsUInt64($ffffffff);
 w2:=t shr 32;
 t:=(u0*v1)+w1;
 result.Hi:=((u1*v1)+w2)+(t shr 32);
 result.Lo:=(t shl 32)+w0;
end;

class operator TPasDblStrUtilsUInt128.Multiply(const a,b:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt128;
begin
 result:=Mul64(a.Lo,b.Lo);
 inc(result.Hi,(a.Hi*b.Lo)+(a.Lo*b.Hi));
end;

class procedure TPasDblStrUtilsUInt128.BinaryDivMod128(Dividend,Divisor:TPasDblStrUtilsUInt128;out Quotient,Remainder:TPasDblStrUtilsUInt128);
var Bit,Shift:TPasDblStrUtilsInt32;
begin
 Quotient:=0;
 Shift:=Divisor.CountLeadingZeroBits-Dividend.CountLeadingZeroBits;
 Divisor:=Divisor shl Shift;
 for Bit:=0 to Shift do begin
  Quotient:=Quotient shl 1;
  if Dividend>=Divisor then begin
   Dividend:=Dividend-Divisor;
   Quotient.Lo:=Quotient.Lo or 1;
  end;
  Divisor:=Divisor shr 1;
 end;
 Remainder:=Dividend;
end;

class procedure TPasDblStrUtilsUInt128.BinaryDivMod64(const Dividend:TPasDblStrUtilsUInt128;const Divisor:TPasDblStrUtilsUInt64;out Quotient:TPasDblStrUtilsUInt128;out Remainder:TPasDblStrUtilsUInt64);
var Bit:TPasDblStrUtilsUInt32;
begin
 Quotient:=Dividend;
 Remainder:=0;
 for Bit:=1 to 128 do begin
  Remainder:=(Remainder shl 1) or (ord((Quotient.Hi and $8000000000000000)<>0) and 1);
  Quotient.Hi:=(Quotient.Hi shl 1) or (Quotient.Lo shr 63);
  Quotient.Lo:=Quotient.Lo shl 1;
  if (TPasDblStrUtilsUInt32(Remainder shr 32)>TPasDblStrUtilsUInt32(Divisor shr 32)) or
     ((TPasDblStrUtilsUInt32(Remainder shr 32)=TPasDblStrUtilsUInt32(Divisor shr 32)) and (TPasDblStrUtilsUInt32(Remainder and $ffffffff)>=TPasDblStrUtilsUInt32(Divisor and $ffffffff))) then begin
   dec(Remainder,Divisor);
   Quotient.Lo:=Quotient.Lo or 1;
  end;
 end;
end;

class procedure TPasDblStrUtilsUInt128.DivMod64(const Dividend:TPasDblStrUtilsUInt128;const Divisor:TPasDblStrUtilsUInt64;out Quotient,Remainder:TPasDblStrUtilsUInt64);
const b=TPasDblStrUtilsUInt64(1) shl 32;
var u0,u1,v,un1,un0,vn1,vn0,q1,q0,un32,un21,un10,rhat,left,right:TPasDblStrUtilsUInt64;
    s:NativeInt;
begin
 u0:=Dividend.Lo;
 u1:=Dividend.Hi;
 v:=Divisor;
 s:=0;
 while (v and (TPasDblStrUtilsUInt64(1) shl 63))=0 do begin
  inc(s);
  v:=v shl 1;
 end;
 v:=Divisor shl s;
 vn1:=v shr 32;
 vn0:=v and $ffffffff;
 if s>0 then begin
  un32:=(u1 shl s) or (u0 shr (64-s));
  un10:=u0 shl s;
 end else begin
  un32:=u1;
  un10:=u0;
 end;
 un1:=un10 shr 32;
 un0:=un10 and $ffffffff;
 q1:=un32 div vn1;
 rhat:=un32 mod vn1;
 left:=q1*vn0;
 right:=(rhat shl 32)+un1;
 repeat
  if (q1>=b) or (left>right) then begin
   dec(q1);
   inc(rhat,vn1);
   if rhat<b then begin
    dec(left,vn0);
    right:=(rhat shl 32) or un1;
    continue;
   end;
  end;
  break;
 until false;
 un21:=(un32 shl 32)+(un1-(q1*v));
 q0:=un21 div vn1;
 rhat:=un21 mod vn1;
 left:=q0*vn0;
 right:=(rhat shl 32) or un0;
 repeat
  if (q0>=b) or (left>right) then begin
   dec(q0);
   inc(rhat,vn1);
   if rhat<b then begin
    dec(left,vn0);
    right:=(rhat shl 32) or un0;
    continue;
   end;
  end;
  break;
 until false;
 Remainder:=((un21 shl 32)+(un0-(q0*v))) shr s;
 Quotient:=(q1 shl 32) or q0;
end;

class procedure TPasDblStrUtilsUInt128.DivMod128Ex(Dividend,Divisor:TPasDblStrUtilsUInt128;out Quotient,Remainder:TPasDblStrUtilsUInt128);
var DivisorLeadingZeroBits:TPasDblStrUtilsInt32;
    v,u,q:TPasDblStrUtilsUInt128;
begin
 if Divisor.Hi=0 then begin
  if Dividend.Hi<Divisor.Lo then begin
   Quotient.Hi:=0;
   Remainder.Hi:=0;
	 DivMod64(Dividend,Divisor.Lo,Quotient.Lo,Remainder.Lo);
  end else begin
   Quotient.Hi:=Dividend.Hi div Divisor.Lo;
   Dividend.Hi:=Dividend.Hi mod Divisor.Lo;
	 DivMod64(Dividend,Divisor.Lo,Quotient.Lo,Remainder.Lo);
   Remainder.Hi:=0;
  end;
 end else begin
  DivisorLeadingZeroBits:=Divisor.CountLeadingZeroBits;
  v:=Divisor shl DivisorLeadingZeroBits;
  u:=Dividend shr 1;
  DivMod64(u,v.Hi,q.Lo,q.Hi);
  q.Hi:=0;
  q:=q shr (63-DivisorLeadingZeroBits);
  if (q.Hi or q.Lo)<>0 then begin
   dec(q);
  end;
	Quotient:=q*Divisor;
  Remainder:=Dividend-q;
	if Remainder>=Divisor then begin
   inc(Quotient);
   Remainder:=Remainder-Divisor;
  end;
 end;
end;

class procedure TPasDblStrUtilsUInt128.DivMod128(Dividend,Divisor:TPasDblStrUtilsUInt128;out Quotient,Remainder:TPasDblStrUtilsUInt128);
var DivisorLeadingZeroBits,DividendLeadingZeroBits,DivisorTrailingZeroBits:TPasDblStrUtilsInt32;
begin
 DivisorLeadingZeroBits:=Divisor.CountLeadingZeroBits;
 DividendLeadingZeroBits:=Dividend.CountLeadingZeroBits;
 DivisorTrailingZeroBits:=Divisor.CountTrailingZeroBits;
 if DivisorLeadingZeroBits=128 then begin
  Assert(false);
  Quotient.Hi:=0;
  Quotient.Lo:=0;
	Remainder.Hi:=0;
	Remainder.Lo:=0;
 end else if (Dividend.Hi or Divisor.Hi)=0 then begin
  Quotient.Hi:=0;
  Remainder.Hi:=0;
  Quotient.Lo:=Dividend.Lo div Divisor.Lo;
  Remainder.Lo:=Dividend.Lo mod Divisor.Lo;
 end else if DivisorLeadingZeroBits=127 then begin
 	Quotient:=Dividend;
  Remainder.Hi:=0;
  Remainder.Lo:=0;
 end else if (DivisorTrailingZeroBits+DivisorLeadingZeroBits)=127 then begin
  Quotient:=Dividend shr DivisorTrailingZeroBits;
  dec(Divisor);
  Remainder:=Divisor and Dividend;
 end else if Dividend<Divisor then begin
  Quotient.Hi:=0;
  Quotient.Lo:=0;
	Remainder:=Dividend;
 end else if Dividend=Divisor then begin
  Quotient.Hi:=0;
  Quotient.Lo:=0;
	Remainder.Hi:=0;
	Remainder.Lo:=1;
 end else if (DivisorLeadingZeroBits-DividendLeadingZeroBits)>5 then begin
  DivMod128Ex(Dividend,Divisor,Quotient,Remainder);
 end else begin
  BinaryDivMod128(Dividend,Divisor,Quotient,Remainder);
 end;
end;

class operator TPasDblStrUtilsUInt128.IntDivide(const Dividend:TPasDblStrUtilsUInt128;const Divisor:TPasDblStrUtilsUInt64):TPasDblStrUtilsUInt128;
var Quotient:TPasDblStrUtilsUInt128;
    Remainder:TPasDblStrUtilsUInt64;
    Bit:TPasDblStrUtilsUInt32;
begin
 if Dividend.Hi=0 then begin
  result.Hi:=0;
  if Dividend<Divisor then begin
   result.Lo:=0;
  end else begin
   result.Lo:=Dividend.Lo div Divisor;
  end;
 end else begin
  Quotient:=Dividend;
  Remainder:=0;
  for Bit:=1 to 128 do begin
   Remainder:=(Remainder shl 1) or (ord((Quotient.Hi and $8000000000000000)<>0) and 1);
   Quotient.Hi:=(Quotient.Hi shl 1) or (Quotient.Lo shr 63);
   Quotient.Lo:=Quotient.Lo shl 1;
   if (TPasDblStrUtilsUInt32(Remainder shr 32)>TPasDblStrUtilsUInt32(Divisor shr 32)) or
      ((TPasDblStrUtilsUInt32(Remainder shr 32)=TPasDblStrUtilsUInt32(Divisor shr 32)) and (TPasDblStrUtilsUInt32(Remainder and $ffffffff)>=TPasDblStrUtilsUInt32(Divisor and $ffffffff))) then begin
    dec(Remainder,Divisor);
    Quotient.Lo:=Quotient.Lo or 1;
   end;
  end;
  result:=Quotient;
 end;
end;

class operator TPasDblStrUtilsUInt128.IntDivide(const Dividend,Divisor:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt128;
var Remainder:TPasDblStrUtilsUInt128;
begin
 TPasDblStrUtilsUInt128.DivMod128(Dividend,Divisor,result,Remainder);
end;

class operator TPasDblStrUtilsUInt128.Modulus(const Dividend:TPasDblStrUtilsUInt128;const Divisor:TPasDblStrUtilsUInt64):TPasDblStrUtilsUInt128;
var Quotient:TPasDblStrUtilsUInt128;
    Remainder:TPasDblStrUtilsUInt64;
    Bit:TPasDblStrUtilsUInt32;
begin
 if Dividend.Hi=0 then begin
  result.Hi:=0;
  if Dividend<Divisor then begin
   result.Lo:=Dividend.Lo;
  end else begin
   result.Lo:=Dividend.Lo mod Divisor;
  end;
 end else begin
  Quotient:=Dividend;
  Remainder:=0;
  for Bit:=1 to 128 do begin
   Remainder:=(Remainder shl 1) or (ord((Quotient.Hi and $8000000000000000)<>0) and 1);
   Quotient.Hi:=(Quotient.Hi shl 1) or (Quotient.Lo shr 63);
   Quotient.Lo:=Quotient.Lo shl 1;
   if (TPasDblStrUtilsUInt32(Remainder shr 32)>TPasDblStrUtilsUInt32(Divisor shr 32)) or
      ((TPasDblStrUtilsUInt32(Remainder shr 32)=TPasDblStrUtilsUInt32(Divisor shr 32)) and (TPasDblStrUtilsUInt32(Remainder and $ffffffff)>=TPasDblStrUtilsUInt32(Divisor and $ffffffff))) then begin
    dec(Remainder,Divisor);
    Quotient.Lo:=Quotient.Lo or 1;
   end;
  end;
  result:=Remainder;
 end;
end;

class operator TPasDblStrUtilsUInt128.Modulus(const Dividend,Divisor:TPasDblStrUtilsUInt128):TPasDblStrUtilsUInt128;
var Quotient:TPasDblStrUtilsUInt128;
begin
 TPasDblStrUtilsUInt128.DivMod128(Dividend,Divisor,Quotient,result);
end;

{$if defined(CPU64) or defined(CPUx64) or defined(CPUx8664) or defined(CPUx86_64) or defined(CPUAArch64)}
function Div5(const x:TPasDblStrUtilsUInt64):TPasDblStrUtilsUInt64; {$ifdef caninline}inline;{$endif}
begin
 result:=x div 5;
end;

function Div10(const x:TPasDblStrUtilsUInt64):TPasDblStrUtilsUInt64; {$ifdef caninline}inline;{$endif}
begin
 result:=x div 10;
end;

function RoundDiv10(const x:TPasDblStrUtilsUInt64):TPasDblStrUtilsUInt64; {$ifdef caninline}inline;{$endif}
begin
 result:=(x div 10)+(ord((x mod 10)>=5) and 1);
end;

function Div100(const x:TPasDblStrUtilsUInt64):TPasDblStrUtilsUInt64; {$ifdef caninline}inline;{$endif}
begin
 result:=x div 100;
end;

function Div1e8(const x:TPasDblStrUtilsUInt64):TPasDblStrUtilsUInt64; {$ifdef caninline}inline;{$endif}
begin
 result:=x div TPasDblStrUtilsUInt64(100000000);
end;

function Div1e9(const x:TPasDblStrUtilsUInt64):TPasDblStrUtilsUInt64; {$ifdef caninline}inline;{$endif}
begin
 result:=x div TPasDblStrUtilsUInt64(1000000000);
end;

function Mod1e9(const x:TPasDblStrUtilsUInt64):TPasDblStrUtilsUInt64; {$ifdef caninline}inline;{$endif}
begin
 result:=TPasDblStrUtilsUInt32((x-(1000000000*Div1e9(x))) and $ffffffff);
end;
{$else}
function UMulH(const a,b:TPasDblStrUtilsUInt64):TPasDblStrUtilsUInt64;
var u0,u1,v0,v1,t,w1,w2:TPasDblStrUtilsUInt64;
begin
 u1:=a shr 32;
 u0:=a and UInt64($ffffffff);
 v1:=b shr 32;
 v0:=b and UInt64($ffffffff);
 t:=u0*v0;
 t:=(u1*v0)+(t shr 32);
 w1:=t and UInt64($ffffffff);
 w2:=t shr 32;
 t:=(u0*v1)+w1;
 result:=((u1*v1)+w2)+(t shr 32);
end;

function Div5(const x:TPasDblStrUtilsUInt64):TPasDblStrUtilsUInt64;
begin
 result:=UMulH(x,TPasDblStrUtilsUInt64($cccccccccccccccd)) shr 2;
end;

function Div10(const x:TPasDblStrUtilsUInt64):TPasDblStrUtilsUInt64;
begin
 result:=UMulH(x,TPasDblStrUtilsUInt64($cccccccccccccccd)) shr 3;
end;

function RoundDiv10(const x:TPasDblStrUtilsUInt64):TPasDblStrUtilsUInt64; {$ifdef caninline}inline;{$endif}
begin
 result:=UMulH(x,TPasDblStrUtilsUInt64($cccccccccccccccd)) shr 3;
 inc(result,ord((x-(10*result))>=5) and 1);
end;

function Div100(const x:TPasDblStrUtilsUInt64):TPasDblStrUtilsUInt64;
begin
 result:=UMulH(x shr 2,TPasDblStrUtilsUInt64($28f5c28f5c28f5c3)) shr 2;
end;

function Div1e8(const x:TPasDblStrUtilsUInt64):TPasDblStrUtilsUInt64;
begin
 result:=UMulH(x,TPasDblStrUtilsUInt64($abcc77118461cefd)) shr 26;
end;

function Div1e9(const x:TPasDblStrUtilsUInt64):TPasDblStrUtilsUInt64;
begin
 result:=UMulH(x shr 9,TPasDblStrUtilsUInt64($44b82fa09b5a53)) shr 11;
end;

function Mod1e9(const x:TPasDblStrUtilsUInt64):TPasDblStrUtilsUInt64;
begin
 result:=TPasDblStrUtilsUInt32(x and $ffffffff)-TPasDblStrUtilsUInt32(1000000000*TPasDblStrUtilsUInt32(Div1e9(x) and $ffffffff));
end;
{$ifend}

function Pow5Factor(aValue:TPasDblStrUtilsUInt64):TPasDblStrUtilsUInt32;
const Inv5:TPasDblStrUtilsUInt64=TPasDblStrUtilsUInt64(14757395258967641293);
      nDiv5:TPasDblStrUtilsUInt64=TPasDblStrUtilsUInt64(3689348814741910323);
begin
 result:=0;
 repeat
  Assert(aValue<>0);
  aValue:=aValue*Inv5;
  if aValue>nDiv5 then begin
   break;
  end else begin
   inc(result);
  end;
 until false;
end;

function MultipleOfPowerOf5(const aValue:TPasDblStrUtilsUInt64;const aP:TPasDblStrUtilsUInt32):boolean;
begin
 result:=Pow5Factor(aValue)>=aP;
end;

function MultipleOfPowerOf2(const aValue:TPasDblStrUtilsUInt64;const aP:TPasDblStrUtilsUInt32):boolean;
begin
 Assert(aValue<>0);
 Assert(aP<64);
 result:=(aValue and ((TPasDblStrUtilsUInt64(1) shl aP)-1))=0;
end;

function MulShift64(const aM:TPasDblStrUtilsUInt64;const aMul:PPasDblStrUtilsUInt64;const aJ:TPasDblStrUtilsInt32):TPasDblStrUtilsUInt64;
type TPasDblStrUtilsUInt64s=array[0..1] of TPasDblStrUtilsUInt64;
     PPasDblStrUtilsUInt64s=^TPasDblStrUtilsUInt64s;
{$if declared(TPasDblStrUtilsUInt128)}
begin
 result:=TPasDblStrUtilsUInt64(((TPasDblStrUtilsUInt128.Mul64(aM,PPasDblStrUtilsUInt64s(aMul)^[0]) shr 64)+
                                TPasDblStrUtilsUInt128.Mul64(aM,PPasDblStrUtilsUInt64s(aMul)^[1])
                               ) shr (aJ-64));
end;
{$else}
var High0,High1,Low1,Sum:TPasDblStrUtilsUInt64;
begin
 Low1:=UMul128(aM,PPasDblStrUtilsUInt64s(aMul)^[1],High1);
 UMul128(aM,PPasDblStrUtilsUInt64s(aMul)^[0],High0);
 Sum:=High0+Low1;
 if Sum<High0 then begin
  inc(High1);
 end;
 result:=ShiftRight128(Sum,High1,aJ-64);
end;
{$ifend}

function MulShiftAll64(const aM:TPasDblStrUtilsUInt64;const aMul:PPasDblStrUtilsUInt64;const aJ:TPasDblStrUtilsInt32;out aVP,aVM:TPasDblStrUtilsUInt64;const aMMShift:TPasDblStrUtilsUInt32):TPasDblStrUtilsUInt64;
begin
 aVP:=MulShift64((4*aM)+2,aMul,aJ);
 aVM:=MulShift64(((4*aM)-1)-aMMShift,aMul,aJ);
 result:=MulShift64(4*aM,aMul,aJ);
end;

function Log10Pow2(const e:TPasDblStrUtilsInt32):TPasDblStrUtilsInt32;
begin
 Assert(e>=0);
 Assert(e<=32768);
 result:=TPasDblStrUtilsUInt32((TPasDblStrUtilsUInt64(e)*TPasDblStrUtilsUInt64(169464822037455)) shr 49);
end;

function Log10Pow5(const e:TPasDblStrUtilsInt32):TPasDblStrUtilsInt32;
begin
 Assert(e>=0);
 Assert(e<=32768);
 result:=TPasDblStrUtilsUInt32((TPasDblStrUtilsUInt64(e)*TPasDblStrUtilsUInt64(196742565691928)) shr 48);
end;

function Pow5Bits(const e:TPasDblStrUtilsInt32):TPasDblStrUtilsInt32;
begin
 Assert(e>=0);
 Assert(e<=32768);
 result:=TPasDblStrUtilsUInt32((TPasDblStrUtilsUInt64(e)*TPasDblStrUtilsUInt64(163391164108059)) shr 46)+1;
end;

function Log2Pow5(const e:TPasDblStrUtilsInt32):TPasDblStrUtilsInt32;
begin
 Assert(e>=0);
 Assert(e<=3528);
 result:=TPasDblStrUtilsInt32(TPasDblStrUtilsUInt32(TPasDblStrUtilsUInt32(TPasDblStrUtilsUInt32(e)*1217359) shr 19));
end;

function CeilLog2Pow5(const e:TPasDblStrUtilsInt32):TPasDblStrUtilsInt32;
begin
 result:=Log2Pow5(e)+1;
end;

const DOUBLE_POW5_INV_BITCOUNT=125;
      DOUBLE_POW5_BITCOUNT=125;
      DOUBLE_POW5_INV_TABLE_SIZE=342;
      DOUBLE_POW5_TABLE_SIZE=326;
      DOUBLE_POW5_INV_SPLIT:array[0..DOUBLE_POW5_INV_TABLE_SIZE-1,0..1] of TPasDblStrUtilsUInt64=
       (
        (TPasDblStrUtilsUInt64(1),TPasDblStrUtilsUInt64(2305843009213693952)),(TPasDblStrUtilsUInt64(11068046444225730970),TPasDblStrUtilsUInt64(1844674407370955161)),
        (TPasDblStrUtilsUInt64(5165088340638674453),TPasDblStrUtilsUInt64(1475739525896764129)),(TPasDblStrUtilsUInt64(7821419487252849886),TPasDblStrUtilsUInt64(1180591620717411303)),
        (TPasDblStrUtilsUInt64(8824922364862649494),TPasDblStrUtilsUInt64(1888946593147858085)),(TPasDblStrUtilsUInt64(7059937891890119595),TPasDblStrUtilsUInt64(1511157274518286468)),
        (TPasDblStrUtilsUInt64(13026647942995916322),TPasDblStrUtilsUInt64(1208925819614629174)),(TPasDblStrUtilsUInt64(9774590264567735146),TPasDblStrUtilsUInt64(1934281311383406679)),
        (TPasDblStrUtilsUInt64(11509021026396098440),TPasDblStrUtilsUInt64(1547425049106725343)),(TPasDblStrUtilsUInt64(16585914450600699399),TPasDblStrUtilsUInt64(1237940039285380274)),
        (TPasDblStrUtilsUInt64(15469416676735388068),TPasDblStrUtilsUInt64(1980704062856608439)),(TPasDblStrUtilsUInt64(16064882156130220778),TPasDblStrUtilsUInt64(1584563250285286751)),
        (TPasDblStrUtilsUInt64(9162556910162266299),TPasDblStrUtilsUInt64(1267650600228229401)),(TPasDblStrUtilsUInt64(7281393426775805432),TPasDblStrUtilsUInt64(2028240960365167042)),
        (TPasDblStrUtilsUInt64(16893161185646375315),TPasDblStrUtilsUInt64(1622592768292133633)),(TPasDblStrUtilsUInt64(2446482504291369283),TPasDblStrUtilsUInt64(1298074214633706907)),
        (TPasDblStrUtilsUInt64(7603720821608101175),TPasDblStrUtilsUInt64(2076918743413931051)),(TPasDblStrUtilsUInt64(2393627842544570617),TPasDblStrUtilsUInt64(1661534994731144841)),
        (TPasDblStrUtilsUInt64(16672297533003297786),TPasDblStrUtilsUInt64(1329227995784915872)),(TPasDblStrUtilsUInt64(11918280793837635165),TPasDblStrUtilsUInt64(2126764793255865396)),
        (TPasDblStrUtilsUInt64(5845275820328197809),TPasDblStrUtilsUInt64(1701411834604692317)),(TPasDblStrUtilsUInt64(15744267100488289217),TPasDblStrUtilsUInt64(1361129467683753853)),
        (TPasDblStrUtilsUInt64(3054734472329800808),TPasDblStrUtilsUInt64(2177807148294006166)),(TPasDblStrUtilsUInt64(17201182836831481939),TPasDblStrUtilsUInt64(1742245718635204932)),
        (TPasDblStrUtilsUInt64(6382248639981364905),TPasDblStrUtilsUInt64(1393796574908163946)),(TPasDblStrUtilsUInt64(2832900194486363201),TPasDblStrUtilsUInt64(2230074519853062314)),
        (TPasDblStrUtilsUInt64(5955668970331000884),TPasDblStrUtilsUInt64(1784059615882449851)),(TPasDblStrUtilsUInt64(1075186361522890384),TPasDblStrUtilsUInt64(1427247692705959881)),
        (TPasDblStrUtilsUInt64(12788344622662355584),TPasDblStrUtilsUInt64(2283596308329535809)),(TPasDblStrUtilsUInt64(13920024512871794791),TPasDblStrUtilsUInt64(1826877046663628647)),
        (TPasDblStrUtilsUInt64(3757321980813615186),TPasDblStrUtilsUInt64(1461501637330902918)),(TPasDblStrUtilsUInt64(10384555214134712795),TPasDblStrUtilsUInt64(1169201309864722334)),
        (TPasDblStrUtilsUInt64(5547241898389809503),TPasDblStrUtilsUInt64(1870722095783555735)),(TPasDblStrUtilsUInt64(4437793518711847602),TPasDblStrUtilsUInt64(1496577676626844588)),
        (TPasDblStrUtilsUInt64(10928932444453298728),TPasDblStrUtilsUInt64(1197262141301475670)),(TPasDblStrUtilsUInt64(17486291911125277965),TPasDblStrUtilsUInt64(1915619426082361072)),
        (TPasDblStrUtilsUInt64(6610335899416401726),TPasDblStrUtilsUInt64(1532495540865888858)),(TPasDblStrUtilsUInt64(12666966349016942027),TPasDblStrUtilsUInt64(1225996432692711086)),
        (TPasDblStrUtilsUInt64(12888448528943286597),TPasDblStrUtilsUInt64(1961594292308337738)),(TPasDblStrUtilsUInt64(17689456452638449924),TPasDblStrUtilsUInt64(1569275433846670190)),
        (TPasDblStrUtilsUInt64(14151565162110759939),TPasDblStrUtilsUInt64(1255420347077336152)),(TPasDblStrUtilsUInt64(7885109000409574610),TPasDblStrUtilsUInt64(2008672555323737844)),
        (TPasDblStrUtilsUInt64(9997436015069570011),TPasDblStrUtilsUInt64(1606938044258990275)),(TPasDblStrUtilsUInt64(7997948812055656009),TPasDblStrUtilsUInt64(1285550435407192220)),
        (TPasDblStrUtilsUInt64(12796718099289049614),TPasDblStrUtilsUInt64(2056880696651507552)),(TPasDblStrUtilsUInt64(2858676849947419045),TPasDblStrUtilsUInt64(1645504557321206042)),
        (TPasDblStrUtilsUInt64(13354987924183666206),TPasDblStrUtilsUInt64(1316403645856964833)),(TPasDblStrUtilsUInt64(17678631863951955605),TPasDblStrUtilsUInt64(2106245833371143733)),
        (TPasDblStrUtilsUInt64(3074859046935833515),TPasDblStrUtilsUInt64(1684996666696914987)),(TPasDblStrUtilsUInt64(13527933681774397782),TPasDblStrUtilsUInt64(1347997333357531989)),
        (TPasDblStrUtilsUInt64(10576647446613305481),TPasDblStrUtilsUInt64(2156795733372051183)),(TPasDblStrUtilsUInt64(15840015586774465031),TPasDblStrUtilsUInt64(1725436586697640946)),
        (TPasDblStrUtilsUInt64(8982663654677661702),TPasDblStrUtilsUInt64(1380349269358112757)),(TPasDblStrUtilsUInt64(18061610662226169046),TPasDblStrUtilsUInt64(2208558830972980411)),
        (TPasDblStrUtilsUInt64(10759939715039024913),TPasDblStrUtilsUInt64(1766847064778384329)),(TPasDblStrUtilsUInt64(12297300586773130254),TPasDblStrUtilsUInt64(1413477651822707463)),
        (TPasDblStrUtilsUInt64(15986332124095098083),TPasDblStrUtilsUInt64(2261564242916331941)),(TPasDblStrUtilsUInt64(9099716884534168143),TPasDblStrUtilsUInt64(1809251394333065553)),
        (TPasDblStrUtilsUInt64(14658471137111155161),TPasDblStrUtilsUInt64(1447401115466452442)),(TPasDblStrUtilsUInt64(4348079280205103483),TPasDblStrUtilsUInt64(1157920892373161954)),
        (TPasDblStrUtilsUInt64(14335624477811986218),TPasDblStrUtilsUInt64(1852673427797059126)),(TPasDblStrUtilsUInt64(7779150767507678651),TPasDblStrUtilsUInt64(1482138742237647301)),
        (TPasDblStrUtilsUInt64(2533971799264232598),TPasDblStrUtilsUInt64(1185710993790117841)),(TPasDblStrUtilsUInt64(15122401323048503126),TPasDblStrUtilsUInt64(1897137590064188545)),
        (TPasDblStrUtilsUInt64(12097921058438802501),TPasDblStrUtilsUInt64(1517710072051350836)),(TPasDblStrUtilsUInt64(5988988032009131678),TPasDblStrUtilsUInt64(1214168057641080669)),
        (TPasDblStrUtilsUInt64(16961078480698431330),TPasDblStrUtilsUInt64(1942668892225729070)),(TPasDblStrUtilsUInt64(13568862784558745064),TPasDblStrUtilsUInt64(1554135113780583256)),
        (TPasDblStrUtilsUInt64(7165741412905085728),TPasDblStrUtilsUInt64(1243308091024466605)),(TPasDblStrUtilsUInt64(11465186260648137165),TPasDblStrUtilsUInt64(1989292945639146568)),
        (TPasDblStrUtilsUInt64(16550846638002330379),TPasDblStrUtilsUInt64(1591434356511317254)),(TPasDblStrUtilsUInt64(16930026125143774626),TPasDblStrUtilsUInt64(1273147485209053803)),
        (TPasDblStrUtilsUInt64(4951948911778577463),TPasDblStrUtilsUInt64(2037035976334486086)),(TPasDblStrUtilsUInt64(272210314680951647),TPasDblStrUtilsUInt64(1629628781067588869)),
        (TPasDblStrUtilsUInt64(3907117066486671641),TPasDblStrUtilsUInt64(1303703024854071095)),(TPasDblStrUtilsUInt64(6251387306378674625),TPasDblStrUtilsUInt64(2085924839766513752)),
        (TPasDblStrUtilsUInt64(16069156289328670670),TPasDblStrUtilsUInt64(1668739871813211001)),(TPasDblStrUtilsUInt64(9165976216721026213),TPasDblStrUtilsUInt64(1334991897450568801)),
        (TPasDblStrUtilsUInt64(7286864317269821294),TPasDblStrUtilsUInt64(2135987035920910082)),(TPasDblStrUtilsUInt64(16897537898041588005),TPasDblStrUtilsUInt64(1708789628736728065)),
        (TPasDblStrUtilsUInt64(13518030318433270404),TPasDblStrUtilsUInt64(1367031702989382452)),(TPasDblStrUtilsUInt64(6871453250525591353),TPasDblStrUtilsUInt64(2187250724783011924)),
        (TPasDblStrUtilsUInt64(9186511415162383406),TPasDblStrUtilsUInt64(1749800579826409539)),(TPasDblStrUtilsUInt64(11038557946871817048),TPasDblStrUtilsUInt64(1399840463861127631)),
        (TPasDblStrUtilsUInt64(10282995085511086630),TPasDblStrUtilsUInt64(2239744742177804210)),(TPasDblStrUtilsUInt64(8226396068408869304),TPasDblStrUtilsUInt64(1791795793742243368)),
        (TPasDblStrUtilsUInt64(13959814484210916090),TPasDblStrUtilsUInt64(1433436634993794694)),(TPasDblStrUtilsUInt64(11267656730511734774),TPasDblStrUtilsUInt64(2293498615990071511)),
        (TPasDblStrUtilsUInt64(5324776569667477496),TPasDblStrUtilsUInt64(1834798892792057209)),(TPasDblStrUtilsUInt64(7949170070475892320),TPasDblStrUtilsUInt64(1467839114233645767)),
        (TPasDblStrUtilsUInt64(17427382500606444826),TPasDblStrUtilsUInt64(1174271291386916613)),(TPasDblStrUtilsUInt64(5747719112518849781),TPasDblStrUtilsUInt64(1878834066219066582)),
        (TPasDblStrUtilsUInt64(15666221734240810795),TPasDblStrUtilsUInt64(1503067252975253265)),(TPasDblStrUtilsUInt64(12532977387392648636),TPasDblStrUtilsUInt64(1202453802380202612)),
        (TPasDblStrUtilsUInt64(5295368560860596524),TPasDblStrUtilsUInt64(1923926083808324180)),(TPasDblStrUtilsUInt64(4236294848688477220),TPasDblStrUtilsUInt64(1539140867046659344)),
        (TPasDblStrUtilsUInt64(7078384693692692099),TPasDblStrUtilsUInt64(1231312693637327475)),(TPasDblStrUtilsUInt64(11325415509908307358),TPasDblStrUtilsUInt64(1970100309819723960)),
        (TPasDblStrUtilsUInt64(9060332407926645887),TPasDblStrUtilsUInt64(1576080247855779168)),(TPasDblStrUtilsUInt64(14626963555825137356),TPasDblStrUtilsUInt64(1260864198284623334)),
        (TPasDblStrUtilsUInt64(12335095245094488799),TPasDblStrUtilsUInt64(2017382717255397335)),(TPasDblStrUtilsUInt64(9868076196075591040),TPasDblStrUtilsUInt64(1613906173804317868)),
        (TPasDblStrUtilsUInt64(15273158586344293478),TPasDblStrUtilsUInt64(1291124939043454294)),(TPasDblStrUtilsUInt64(13369007293925138595),TPasDblStrUtilsUInt64(2065799902469526871)),
        (TPasDblStrUtilsUInt64(7005857020398200553),TPasDblStrUtilsUInt64(1652639921975621497)),(TPasDblStrUtilsUInt64(16672732060544291412),TPasDblStrUtilsUInt64(1322111937580497197)),
        (TPasDblStrUtilsUInt64(11918976037903224966),TPasDblStrUtilsUInt64(2115379100128795516)),(TPasDblStrUtilsUInt64(5845832015580669650),TPasDblStrUtilsUInt64(1692303280103036413)),
        (TPasDblStrUtilsUInt64(12055363241948356366),TPasDblStrUtilsUInt64(1353842624082429130)),(TPasDblStrUtilsUInt64(841837113407818570),TPasDblStrUtilsUInt64(2166148198531886609)),
        (TPasDblStrUtilsUInt64(4362818505468165179),TPasDblStrUtilsUInt64(1732918558825509287)),(TPasDblStrUtilsUInt64(14558301248600263113),TPasDblStrUtilsUInt64(1386334847060407429)),
        (TPasDblStrUtilsUInt64(12225235553534690011),TPasDblStrUtilsUInt64(2218135755296651887)),(TPasDblStrUtilsUInt64(2401490813343931363),TPasDblStrUtilsUInt64(1774508604237321510)),
        (TPasDblStrUtilsUInt64(1921192650675145090),TPasDblStrUtilsUInt64(1419606883389857208)),(TPasDblStrUtilsUInt64(17831303500047873437),TPasDblStrUtilsUInt64(2271371013423771532)),
        (TPasDblStrUtilsUInt64(6886345170554478103),TPasDblStrUtilsUInt64(1817096810739017226)),(TPasDblStrUtilsUInt64(1819727321701672159),TPasDblStrUtilsUInt64(1453677448591213781)),
        (TPasDblStrUtilsUInt64(16213177116328979020),TPasDblStrUtilsUInt64(1162941958872971024)),(TPasDblStrUtilsUInt64(14873036941900635463),TPasDblStrUtilsUInt64(1860707134196753639)),
        (TPasDblStrUtilsUInt64(15587778368262418694),TPasDblStrUtilsUInt64(1488565707357402911)),(TPasDblStrUtilsUInt64(8780873879868024632),TPasDblStrUtilsUInt64(1190852565885922329)),
        (TPasDblStrUtilsUInt64(2981351763563108441),TPasDblStrUtilsUInt64(1905364105417475727)),(TPasDblStrUtilsUInt64(13453127855076217722),TPasDblStrUtilsUInt64(1524291284333980581)),
        (TPasDblStrUtilsUInt64(7073153469319063855),TPasDblStrUtilsUInt64(1219433027467184465)),(TPasDblStrUtilsUInt64(11317045550910502167),TPasDblStrUtilsUInt64(1951092843947495144)),
        (TPasDblStrUtilsUInt64(12742985255470312057),TPasDblStrUtilsUInt64(1560874275157996115)),(TPasDblStrUtilsUInt64(10194388204376249646),TPasDblStrUtilsUInt64(1248699420126396892)),
        (TPasDblStrUtilsUInt64(1553625868034358140),TPasDblStrUtilsUInt64(1997919072202235028)),(TPasDblStrUtilsUInt64(8621598323911307159),TPasDblStrUtilsUInt64(1598335257761788022)),
        (TPasDblStrUtilsUInt64(17965325103354776697),TPasDblStrUtilsUInt64(1278668206209430417)),(TPasDblStrUtilsUInt64(13987124906400001422),TPasDblStrUtilsUInt64(2045869129935088668)),
        (TPasDblStrUtilsUInt64(121653480894270168),TPasDblStrUtilsUInt64(1636695303948070935)),(TPasDblStrUtilsUInt64(97322784715416134),TPasDblStrUtilsUInt64(1309356243158456748)),
        (TPasDblStrUtilsUInt64(14913111714512307107),TPasDblStrUtilsUInt64(2094969989053530796)),(TPasDblStrUtilsUInt64(8241140556867935363),TPasDblStrUtilsUInt64(1675975991242824637)),
        (TPasDblStrUtilsUInt64(17660958889720079260),TPasDblStrUtilsUInt64(1340780792994259709)),(TPasDblStrUtilsUInt64(17189487779326395846),TPasDblStrUtilsUInt64(2145249268790815535)),
        (TPasDblStrUtilsUInt64(13751590223461116677),TPasDblStrUtilsUInt64(1716199415032652428)),(TPasDblStrUtilsUInt64(18379969808252713988),TPasDblStrUtilsUInt64(1372959532026121942)),
        (TPasDblStrUtilsUInt64(14650556434236701088),TPasDblStrUtilsUInt64(2196735251241795108)),(TPasDblStrUtilsUInt64(652398703163629901),TPasDblStrUtilsUInt64(1757388200993436087)),
        (TPasDblStrUtilsUInt64(11589965406756634890),TPasDblStrUtilsUInt64(1405910560794748869)),(TPasDblStrUtilsUInt64(7475898206584884855),TPasDblStrUtilsUInt64(2249456897271598191)),
        (TPasDblStrUtilsUInt64(2291369750525997561),TPasDblStrUtilsUInt64(1799565517817278553)),(TPasDblStrUtilsUInt64(9211793429904618695),TPasDblStrUtilsUInt64(1439652414253822842)),
        (TPasDblStrUtilsUInt64(18428218302589300235),TPasDblStrUtilsUInt64(2303443862806116547)),(TPasDblStrUtilsUInt64(7363877012587619542),TPasDblStrUtilsUInt64(1842755090244893238)),
        (TPasDblStrUtilsUInt64(13269799239553916280),TPasDblStrUtilsUInt64(1474204072195914590)),(TPasDblStrUtilsUInt64(10615839391643133024),TPasDblStrUtilsUInt64(1179363257756731672)),
        (TPasDblStrUtilsUInt64(2227947767661371545),TPasDblStrUtilsUInt64(1886981212410770676)),(TPasDblStrUtilsUInt64(16539753473096738529),TPasDblStrUtilsUInt64(1509584969928616540)),
        (TPasDblStrUtilsUInt64(13231802778477390823),TPasDblStrUtilsUInt64(1207667975942893232)),(TPasDblStrUtilsUInt64(6413489186596184024),TPasDblStrUtilsUInt64(1932268761508629172)),
        (TPasDblStrUtilsUInt64(16198837793502678189),TPasDblStrUtilsUInt64(1545815009206903337)),(TPasDblStrUtilsUInt64(5580372605318321905),TPasDblStrUtilsUInt64(1236652007365522670)),
        (TPasDblStrUtilsUInt64(8928596168509315048),TPasDblStrUtilsUInt64(1978643211784836272)),(TPasDblStrUtilsUInt64(18210923379033183008),TPasDblStrUtilsUInt64(1582914569427869017)),
        (TPasDblStrUtilsUInt64(7190041073742725760),TPasDblStrUtilsUInt64(1266331655542295214)),(TPasDblStrUtilsUInt64(436019273762630246),TPasDblStrUtilsUInt64(2026130648867672343)),
        (TPasDblStrUtilsUInt64(7727513048493924843),TPasDblStrUtilsUInt64(1620904519094137874)),(TPasDblStrUtilsUInt64(9871359253537050198),TPasDblStrUtilsUInt64(1296723615275310299)),
        (TPasDblStrUtilsUInt64(4726128361433549347),TPasDblStrUtilsUInt64(2074757784440496479)),(TPasDblStrUtilsUInt64(7470251503888749801),TPasDblStrUtilsUInt64(1659806227552397183)),
        (TPasDblStrUtilsUInt64(13354898832594820487),TPasDblStrUtilsUInt64(1327844982041917746)),(TPasDblStrUtilsUInt64(13989140502667892133),TPasDblStrUtilsUInt64(2124551971267068394)),
        (TPasDblStrUtilsUInt64(14880661216876224029),TPasDblStrUtilsUInt64(1699641577013654715)),(TPasDblStrUtilsUInt64(11904528973500979224),TPasDblStrUtilsUInt64(1359713261610923772)),
        (TPasDblStrUtilsUInt64(4289851098633925465),TPasDblStrUtilsUInt64(2175541218577478036)),(TPasDblStrUtilsUInt64(18189276137874781665),TPasDblStrUtilsUInt64(1740432974861982428)),
        (TPasDblStrUtilsUInt64(3483374466074094362),TPasDblStrUtilsUInt64(1392346379889585943)),(TPasDblStrUtilsUInt64(1884050330976640656),TPasDblStrUtilsUInt64(2227754207823337509)),
        (TPasDblStrUtilsUInt64(5196589079523222848),TPasDblStrUtilsUInt64(1782203366258670007)),(TPasDblStrUtilsUInt64(15225317707844309248),TPasDblStrUtilsUInt64(1425762693006936005)),
        (TPasDblStrUtilsUInt64(5913764258841343181),TPasDblStrUtilsUInt64(2281220308811097609)),(TPasDblStrUtilsUInt64(8420360221814984868),TPasDblStrUtilsUInt64(1824976247048878087)),
        (TPasDblStrUtilsUInt64(17804334621677718864),TPasDblStrUtilsUInt64(1459980997639102469)),(TPasDblStrUtilsUInt64(17932816512084085415),TPasDblStrUtilsUInt64(1167984798111281975)),
        (TPasDblStrUtilsUInt64(10245762345624985047),TPasDblStrUtilsUInt64(1868775676978051161)),(TPasDblStrUtilsUInt64(4507261061758077715),TPasDblStrUtilsUInt64(1495020541582440929)),
        (TPasDblStrUtilsUInt64(7295157664148372495),TPasDblStrUtilsUInt64(1196016433265952743)),(TPasDblStrUtilsUInt64(7982903447895485668),TPasDblStrUtilsUInt64(1913626293225524389)),
        (TPasDblStrUtilsUInt64(10075671573058298858),TPasDblStrUtilsUInt64(1530901034580419511)),(TPasDblStrUtilsUInt64(4371188443704728763),TPasDblStrUtilsUInt64(1224720827664335609)),
        (TPasDblStrUtilsUInt64(14372599139411386667),TPasDblStrUtilsUInt64(1959553324262936974)),(TPasDblStrUtilsUInt64(15187428126271019657),TPasDblStrUtilsUInt64(1567642659410349579)),
        (TPasDblStrUtilsUInt64(15839291315758726049),TPasDblStrUtilsUInt64(1254114127528279663)),(TPasDblStrUtilsUInt64(3206773216762499739),TPasDblStrUtilsUInt64(2006582604045247462)),
        (TPasDblStrUtilsUInt64(13633465017635730761),TPasDblStrUtilsUInt64(1605266083236197969)),(TPasDblStrUtilsUInt64(14596120828850494932),TPasDblStrUtilsUInt64(1284212866588958375)),
        (TPasDblStrUtilsUInt64(4907049252451240275),TPasDblStrUtilsUInt64(2054740586542333401)),(TPasDblStrUtilsUInt64(236290587219081897),TPasDblStrUtilsUInt64(1643792469233866721)),
        (TPasDblStrUtilsUInt64(14946427728742906810),TPasDblStrUtilsUInt64(1315033975387093376)),(TPasDblStrUtilsUInt64(16535586736504830250),TPasDblStrUtilsUInt64(2104054360619349402)),
        (TPasDblStrUtilsUInt64(5849771759720043554),TPasDblStrUtilsUInt64(1683243488495479522)),(TPasDblStrUtilsUInt64(15747863852001765813),TPasDblStrUtilsUInt64(1346594790796383617)),
        (TPasDblStrUtilsUInt64(10439186904235184007),TPasDblStrUtilsUInt64(2154551665274213788)),(TPasDblStrUtilsUInt64(15730047152871967852),TPasDblStrUtilsUInt64(1723641332219371030)),
        (TPasDblStrUtilsUInt64(12584037722297574282),TPasDblStrUtilsUInt64(1378913065775496824)),(TPasDblStrUtilsUInt64(9066413911450387881),TPasDblStrUtilsUInt64(2206260905240794919)),
        (TPasDblStrUtilsUInt64(10942479943902220628),TPasDblStrUtilsUInt64(1765008724192635935)),(TPasDblStrUtilsUInt64(8753983955121776503),TPasDblStrUtilsUInt64(1412006979354108748)),
        (TPasDblStrUtilsUInt64(10317025513452932081),TPasDblStrUtilsUInt64(2259211166966573997)),(TPasDblStrUtilsUInt64(874922781278525018),TPasDblStrUtilsUInt64(1807368933573259198)),
        (TPasDblStrUtilsUInt64(8078635854506640661),TPasDblStrUtilsUInt64(1445895146858607358)),(TPasDblStrUtilsUInt64(13841606313089133175),TPasDblStrUtilsUInt64(1156716117486885886)),
        (TPasDblStrUtilsUInt64(14767872471458792434),TPasDblStrUtilsUInt64(1850745787979017418)),(TPasDblStrUtilsUInt64(746251532941302978),TPasDblStrUtilsUInt64(1480596630383213935)),
        (TPasDblStrUtilsUInt64(597001226353042382),TPasDblStrUtilsUInt64(1184477304306571148)),(TPasDblStrUtilsUInt64(15712597221132509104),TPasDblStrUtilsUInt64(1895163686890513836)),
        (TPasDblStrUtilsUInt64(8880728962164096960),TPasDblStrUtilsUInt64(1516130949512411069)),(TPasDblStrUtilsUInt64(10793931984473187891),TPasDblStrUtilsUInt64(1212904759609928855)),
        (TPasDblStrUtilsUInt64(17270291175157100626),TPasDblStrUtilsUInt64(1940647615375886168)),(TPasDblStrUtilsUInt64(2748186495899949531),TPasDblStrUtilsUInt64(1552518092300708935)),
        (TPasDblStrUtilsUInt64(2198549196719959625),TPasDblStrUtilsUInt64(1242014473840567148)),(TPasDblStrUtilsUInt64(18275073973719576693),TPasDblStrUtilsUInt64(1987223158144907436)),
        (TPasDblStrUtilsUInt64(10930710364233751031),TPasDblStrUtilsUInt64(1589778526515925949)),(TPasDblStrUtilsUInt64(12433917106128911148),TPasDblStrUtilsUInt64(1271822821212740759)),
        (TPasDblStrUtilsUInt64(8826220925580526867),TPasDblStrUtilsUInt64(2034916513940385215)),(TPasDblStrUtilsUInt64(7060976740464421494),TPasDblStrUtilsUInt64(1627933211152308172)),
        (TPasDblStrUtilsUInt64(16716827836597268165),TPasDblStrUtilsUInt64(1302346568921846537)),(TPasDblStrUtilsUInt64(11989529279587987770),TPasDblStrUtilsUInt64(2083754510274954460)),
        (TPasDblStrUtilsUInt64(9591623423670390216),TPasDblStrUtilsUInt64(1667003608219963568)),(TPasDblStrUtilsUInt64(15051996368420132820),TPasDblStrUtilsUInt64(1333602886575970854)),
        (TPasDblStrUtilsUInt64(13015147745246481542),TPasDblStrUtilsUInt64(2133764618521553367)),(TPasDblStrUtilsUInt64(3033420566713364587),TPasDblStrUtilsUInt64(1707011694817242694)),
        (TPasDblStrUtilsUInt64(6116085268112601993),TPasDblStrUtilsUInt64(1365609355853794155)),(TPasDblStrUtilsUInt64(9785736428980163188),TPasDblStrUtilsUInt64(2184974969366070648)),
        (TPasDblStrUtilsUInt64(15207286772667951197),TPasDblStrUtilsUInt64(1747979975492856518)),(TPasDblStrUtilsUInt64(1097782973908629988),TPasDblStrUtilsUInt64(1398383980394285215)),
        (TPasDblStrUtilsUInt64(1756452758253807981),TPasDblStrUtilsUInt64(2237414368630856344)),(TPasDblStrUtilsUInt64(5094511021344956708),TPasDblStrUtilsUInt64(1789931494904685075)),
        (TPasDblStrUtilsUInt64(4075608817075965366),TPasDblStrUtilsUInt64(1431945195923748060)),(TPasDblStrUtilsUInt64(6520974107321544586),TPasDblStrUtilsUInt64(2291112313477996896)),
        (TPasDblStrUtilsUInt64(1527430471115325346),TPasDblStrUtilsUInt64(1832889850782397517)),(TPasDblStrUtilsUInt64(12289990821117991246),TPasDblStrUtilsUInt64(1466311880625918013)),
        (TPasDblStrUtilsUInt64(17210690286378213644),TPasDblStrUtilsUInt64(1173049504500734410)),(TPasDblStrUtilsUInt64(9090360384495590213),TPasDblStrUtilsUInt64(1876879207201175057)),
        (TPasDblStrUtilsUInt64(18340334751822203140),TPasDblStrUtilsUInt64(1501503365760940045)),(TPasDblStrUtilsUInt64(14672267801457762512),TPasDblStrUtilsUInt64(1201202692608752036)),
        (TPasDblStrUtilsUInt64(16096930852848599373),TPasDblStrUtilsUInt64(1921924308174003258)),(TPasDblStrUtilsUInt64(1809498238053148529),TPasDblStrUtilsUInt64(1537539446539202607)),
        (TPasDblStrUtilsUInt64(12515645034668249793),TPasDblStrUtilsUInt64(1230031557231362085)),(TPasDblStrUtilsUInt64(1578287981759648052),TPasDblStrUtilsUInt64(1968050491570179337)),
        (TPasDblStrUtilsUInt64(12330676829633449412),TPasDblStrUtilsUInt64(1574440393256143469)),(TPasDblStrUtilsUInt64(13553890278448669853),TPasDblStrUtilsUInt64(1259552314604914775)),
        (TPasDblStrUtilsUInt64(3239480371808320148),TPasDblStrUtilsUInt64(2015283703367863641)),(TPasDblStrUtilsUInt64(17348979556414297411),TPasDblStrUtilsUInt64(1612226962694290912)),
        (TPasDblStrUtilsUInt64(6500486015647617283),TPasDblStrUtilsUInt64(1289781570155432730)),(TPasDblStrUtilsUInt64(10400777625036187652),TPasDblStrUtilsUInt64(2063650512248692368)),
        (TPasDblStrUtilsUInt64(15699319729512770768),TPasDblStrUtilsUInt64(1650920409798953894)),(TPasDblStrUtilsUInt64(16248804598352126938),TPasDblStrUtilsUInt64(1320736327839163115)),
        (TPasDblStrUtilsUInt64(7551343283653851484),TPasDblStrUtilsUInt64(2113178124542660985)),(TPasDblStrUtilsUInt64(6041074626923081187),TPasDblStrUtilsUInt64(1690542499634128788)),
        (TPasDblStrUtilsUInt64(12211557331022285596),TPasDblStrUtilsUInt64(1352433999707303030)),(TPasDblStrUtilsUInt64(1091747655926105338),TPasDblStrUtilsUInt64(2163894399531684849)),
        (TPasDblStrUtilsUInt64(4562746939482794594),TPasDblStrUtilsUInt64(1731115519625347879)),(TPasDblStrUtilsUInt64(7339546366328145998),TPasDblStrUtilsUInt64(1384892415700278303)),
        (TPasDblStrUtilsUInt64(8053925371383123274),TPasDblStrUtilsUInt64(2215827865120445285)),(TPasDblStrUtilsUInt64(6443140297106498619),TPasDblStrUtilsUInt64(1772662292096356228)),
        (TPasDblStrUtilsUInt64(12533209867169019542),TPasDblStrUtilsUInt64(1418129833677084982)),(TPasDblStrUtilsUInt64(5295740528502789974),TPasDblStrUtilsUInt64(2269007733883335972)),
        (TPasDblStrUtilsUInt64(15304638867027962949),TPasDblStrUtilsUInt64(1815206187106668777)),(TPasDblStrUtilsUInt64(4865013464138549713),TPasDblStrUtilsUInt64(1452164949685335022)),
        (TPasDblStrUtilsUInt64(14960057215536570740),TPasDblStrUtilsUInt64(1161731959748268017)),(TPasDblStrUtilsUInt64(9178696285890871890),TPasDblStrUtilsUInt64(1858771135597228828)),
        (TPasDblStrUtilsUInt64(14721654658196518159),TPasDblStrUtilsUInt64(1487016908477783062)),(TPasDblStrUtilsUInt64(4398626097073393881),TPasDblStrUtilsUInt64(1189613526782226450)),
        (TPasDblStrUtilsUInt64(7037801755317430209),TPasDblStrUtilsUInt64(1903381642851562320)),(TPasDblStrUtilsUInt64(5630241404253944167),TPasDblStrUtilsUInt64(1522705314281249856)),
        (TPasDblStrUtilsUInt64(814844308661245011),TPasDblStrUtilsUInt64(1218164251424999885)),(TPasDblStrUtilsUInt64(1303750893857992017),TPasDblStrUtilsUInt64(1949062802279999816)),
        (TPasDblStrUtilsUInt64(15800395974054034906),TPasDblStrUtilsUInt64(1559250241823999852)),(TPasDblStrUtilsUInt64(5261619149759407279),TPasDblStrUtilsUInt64(1247400193459199882)),
        (TPasDblStrUtilsUInt64(12107939454356961969),TPasDblStrUtilsUInt64(1995840309534719811)),(TPasDblStrUtilsUInt64(5997002748743659252),TPasDblStrUtilsUInt64(1596672247627775849)),
        (TPasDblStrUtilsUInt64(8486951013736837725),TPasDblStrUtilsUInt64(1277337798102220679)),(TPasDblStrUtilsUInt64(2511075177753209390),TPasDblStrUtilsUInt64(2043740476963553087)),
        (TPasDblStrUtilsUInt64(13076906586428298482),TPasDblStrUtilsUInt64(1634992381570842469)),(TPasDblStrUtilsUInt64(14150874083884549109),TPasDblStrUtilsUInt64(1307993905256673975)),
        (TPasDblStrUtilsUInt64(4194654460505726958),TPasDblStrUtilsUInt64(2092790248410678361)),(TPasDblStrUtilsUInt64(18113118827372222859),TPasDblStrUtilsUInt64(1674232198728542688)),
        (TPasDblStrUtilsUInt64(3422448617672047318),TPasDblStrUtilsUInt64(1339385758982834151)),(TPasDblStrUtilsUInt64(16543964232501006678),TPasDblStrUtilsUInt64(2143017214372534641)),
        (TPasDblStrUtilsUInt64(9545822571258895019),TPasDblStrUtilsUInt64(1714413771498027713)),(TPasDblStrUtilsUInt64(15015355686490936662),TPasDblStrUtilsUInt64(1371531017198422170)),
        (TPasDblStrUtilsUInt64(5577825024675947042),TPasDblStrUtilsUInt64(2194449627517475473)),(TPasDblStrUtilsUInt64(11840957649224578280),TPasDblStrUtilsUInt64(1755559702013980378)),
        (TPasDblStrUtilsUInt64(16851463748863483271),TPasDblStrUtilsUInt64(1404447761611184302)),(TPasDblStrUtilsUInt64(12204946739213931940),TPasDblStrUtilsUInt64(2247116418577894884)),
        (TPasDblStrUtilsUInt64(13453306206113055875),TPasDblStrUtilsUInt64(1797693134862315907)),(TPasDblStrUtilsUInt64(3383947335406624054),TPasDblStrUtilsUInt64(1438154507889852726)),
        (TPasDblStrUtilsUInt64(16482362180876329456),TPasDblStrUtilsUInt64(2301047212623764361)),(TPasDblStrUtilsUInt64(9496540929959153242),TPasDblStrUtilsUInt64(1840837770099011489)),
        (TPasDblStrUtilsUInt64(11286581558709232917),TPasDblStrUtilsUInt64(1472670216079209191)),(TPasDblStrUtilsUInt64(5339916432225476010),TPasDblStrUtilsUInt64(1178136172863367353)),
        (TPasDblStrUtilsUInt64(4854517476818851293),TPasDblStrUtilsUInt64(1885017876581387765)),(TPasDblStrUtilsUInt64(3883613981455081034),TPasDblStrUtilsUInt64(1508014301265110212)),
        (TPasDblStrUtilsUInt64(14174937629389795797),TPasDblStrUtilsUInt64(1206411441012088169)),(TPasDblStrUtilsUInt64(11611853762797942306),TPasDblStrUtilsUInt64(1930258305619341071)),
        (TPasDblStrUtilsUInt64(5600134195496443521),TPasDblStrUtilsUInt64(1544206644495472857)),(TPasDblStrUtilsUInt64(15548153800622885787),TPasDblStrUtilsUInt64(1235365315596378285)),
        (TPasDblStrUtilsUInt64(6430302007287065643),TPasDblStrUtilsUInt64(1976584504954205257)),(TPasDblStrUtilsUInt64(16212288050055383484),TPasDblStrUtilsUInt64(1581267603963364205)),
        (TPasDblStrUtilsUInt64(12969830440044306787),TPasDblStrUtilsUInt64(1265014083170691364)),(TPasDblStrUtilsUInt64(9683682259845159889),TPasDblStrUtilsUInt64(2024022533073106183)),
        (TPasDblStrUtilsUInt64(15125643437359948558),TPasDblStrUtilsUInt64(1619218026458484946)),(TPasDblStrUtilsUInt64(8411165935146048523),TPasDblStrUtilsUInt64(1295374421166787957)),
        (TPasDblStrUtilsUInt64(17147214310975587960),TPasDblStrUtilsUInt64(2072599073866860731)),(TPasDblStrUtilsUInt64(10028422634038560045),TPasDblStrUtilsUInt64(1658079259093488585)),
        (TPasDblStrUtilsUInt64(8022738107230848036),TPasDblStrUtilsUInt64(1326463407274790868)),(TPasDblStrUtilsUInt64(9147032156827446534),TPasDblStrUtilsUInt64(2122341451639665389)),
        (TPasDblStrUtilsUInt64(11006974540203867551),TPasDblStrUtilsUInt64(1697873161311732311)),(TPasDblStrUtilsUInt64(5116230817421183718),TPasDblStrUtilsUInt64(1358298529049385849)),
        (TPasDblStrUtilsUInt64(15564666937357714594),TPasDblStrUtilsUInt64(2173277646479017358)),(TPasDblStrUtilsUInt64(1383687105660440706),TPasDblStrUtilsUInt64(1738622117183213887)),
        (TPasDblStrUtilsUInt64(12174996128754083534),TPasDblStrUtilsUInt64(1390897693746571109)),(TPasDblStrUtilsUInt64(8411947361780802685),TPasDblStrUtilsUInt64(2225436309994513775)),
        (TPasDblStrUtilsUInt64(6729557889424642148),TPasDblStrUtilsUInt64(1780349047995611020)),(TPasDblStrUtilsUInt64(5383646311539713719),TPasDblStrUtilsUInt64(1424279238396488816)),
        (TPasDblStrUtilsUInt64(1235136468979721303),TPasDblStrUtilsUInt64(2278846781434382106)),(TPasDblStrUtilsUInt64(15745504434151418335),TPasDblStrUtilsUInt64(1823077425147505684)),
        (TPasDblStrUtilsUInt64(16285752362063044992),TPasDblStrUtilsUInt64(1458461940118004547)),(TPasDblStrUtilsUInt64(5649904260166615347),TPasDblStrUtilsUInt64(1166769552094403638)),
        (TPasDblStrUtilsUInt64(5350498001524674232),TPasDblStrUtilsUInt64(1866831283351045821)),(TPasDblStrUtilsUInt64(591049586477829062),TPasDblStrUtilsUInt64(1493465026680836657)),
        (TPasDblStrUtilsUInt64(11540886113407994219),TPasDblStrUtilsUInt64(1194772021344669325)),(TPasDblStrUtilsUInt64(18673707743239135),TPasDblStrUtilsUInt64(1911635234151470921)),
        (TPasDblStrUtilsUInt64(14772334225162232601),TPasDblStrUtilsUInt64(1529308187321176736)),(TPasDblStrUtilsUInt64(8128518565387875758),TPasDblStrUtilsUInt64(1223446549856941389)),
        (TPasDblStrUtilsUInt64(1937583260394870242),TPasDblStrUtilsUInt64(1957514479771106223)),(TPasDblStrUtilsUInt64(8928764237799716840),TPasDblStrUtilsUInt64(1566011583816884978)),
        (TPasDblStrUtilsUInt64(14521709019723594119),TPasDblStrUtilsUInt64(1252809267053507982)),(TPasDblStrUtilsUInt64(8477339172590109297),TPasDblStrUtilsUInt64(2004494827285612772)),
        (TPasDblStrUtilsUInt64(17849917782297818407),TPasDblStrUtilsUInt64(1603595861828490217)),(TPasDblStrUtilsUInt64(6901236596354434079),TPasDblStrUtilsUInt64(1282876689462792174)),
        (TPasDblStrUtilsUInt64(18420676183650915173),TPasDblStrUtilsUInt64(2052602703140467478)),(TPasDblStrUtilsUInt64(3668494502695001169),TPasDblStrUtilsUInt64(1642082162512373983)),
        (TPasDblStrUtilsUInt64(10313493231639821582),TPasDblStrUtilsUInt64(1313665730009899186)),(TPasDblStrUtilsUInt64(9122891541139893884),TPasDblStrUtilsUInt64(2101865168015838698)),
        (TPasDblStrUtilsUInt64(14677010862395735754),TPasDblStrUtilsUInt64(1681492134412670958)),(TPasDblStrUtilsUInt64(673562245690857633),TPasDblStrUtilsUInt64(1345193707530136767))
       );
      DOUBLE_POW5_SPLIT:array[0..DOUBLE_POW5_TABLE_SIZE-1,0..1] of TPasDblStrUtilsUInt64=
       (
        (TPasDblStrUtilsUInt64(0),TPasDblStrUtilsUInt64(1152921504606846976)),(TPasDblStrUtilsUInt64(0),TPasDblStrUtilsUInt64(1441151880758558720)),
        (TPasDblStrUtilsUInt64(0),TPasDblStrUtilsUInt64(1801439850948198400)),(TPasDblStrUtilsUInt64(0),TPasDblStrUtilsUInt64(2251799813685248000)),
        (TPasDblStrUtilsUInt64(0),TPasDblStrUtilsUInt64(1407374883553280000)),(TPasDblStrUtilsUInt64(0),TPasDblStrUtilsUInt64(1759218604441600000)),
        (TPasDblStrUtilsUInt64(0),TPasDblStrUtilsUInt64(2199023255552000000)),(TPasDblStrUtilsUInt64(0),TPasDblStrUtilsUInt64(1374389534720000000)),
        (TPasDblStrUtilsUInt64(0),TPasDblStrUtilsUInt64(1717986918400000000)),(TPasDblStrUtilsUInt64(0),TPasDblStrUtilsUInt64(2147483648000000000)),
        (TPasDblStrUtilsUInt64(0),TPasDblStrUtilsUInt64(1342177280000000000)),(TPasDblStrUtilsUInt64(0),TPasDblStrUtilsUInt64(1677721600000000000)),
        (TPasDblStrUtilsUInt64(0),TPasDblStrUtilsUInt64(2097152000000000000)),(TPasDblStrUtilsUInt64(0),TPasDblStrUtilsUInt64(1310720000000000000)),
        (TPasDblStrUtilsUInt64(0),TPasDblStrUtilsUInt64(1638400000000000000)),(TPasDblStrUtilsUInt64(0),TPasDblStrUtilsUInt64(2048000000000000000)),
        (TPasDblStrUtilsUInt64(0),TPasDblStrUtilsUInt64(1280000000000000000)),(TPasDblStrUtilsUInt64(0),TPasDblStrUtilsUInt64(1600000000000000000)),
        (TPasDblStrUtilsUInt64(0),TPasDblStrUtilsUInt64(2000000000000000000)),(TPasDblStrUtilsUInt64(0),TPasDblStrUtilsUInt64(1250000000000000000)),
        (TPasDblStrUtilsUInt64(0),TPasDblStrUtilsUInt64(1562500000000000000)),(TPasDblStrUtilsUInt64(0),TPasDblStrUtilsUInt64(1953125000000000000)),
        (TPasDblStrUtilsUInt64(0),TPasDblStrUtilsUInt64(1220703125000000000)),(TPasDblStrUtilsUInt64(0),TPasDblStrUtilsUInt64(1525878906250000000)),
        (TPasDblStrUtilsUInt64(0),TPasDblStrUtilsUInt64(1907348632812500000)),(TPasDblStrUtilsUInt64(0),TPasDblStrUtilsUInt64(1192092895507812500)),
        (TPasDblStrUtilsUInt64(0),TPasDblStrUtilsUInt64(1490116119384765625)),(TPasDblStrUtilsUInt64(4611686018427387904),TPasDblStrUtilsUInt64(1862645149230957031)),
        (TPasDblStrUtilsUInt64(9799832789158199296),TPasDblStrUtilsUInt64(1164153218269348144)),(TPasDblStrUtilsUInt64(12249790986447749120),TPasDblStrUtilsUInt64(1455191522836685180)),
        (TPasDblStrUtilsUInt64(15312238733059686400),TPasDblStrUtilsUInt64(1818989403545856475)),(TPasDblStrUtilsUInt64(14528612397897220096),TPasDblStrUtilsUInt64(2273736754432320594)),
        (TPasDblStrUtilsUInt64(13692068767113150464),TPasDblStrUtilsUInt64(1421085471520200371)),(TPasDblStrUtilsUInt64(12503399940464050176),TPasDblStrUtilsUInt64(1776356839400250464)),
        (TPasDblStrUtilsUInt64(15629249925580062720),TPasDblStrUtilsUInt64(2220446049250313080)),(TPasDblStrUtilsUInt64(9768281203487539200),TPasDblStrUtilsUInt64(1387778780781445675)),
        (TPasDblStrUtilsUInt64(7598665485932036096),TPasDblStrUtilsUInt64(1734723475976807094)),(TPasDblStrUtilsUInt64(274959820560269312),TPasDblStrUtilsUInt64(2168404344971008868)),
        (TPasDblStrUtilsUInt64(9395221924704944128),TPasDblStrUtilsUInt64(1355252715606880542)),(TPasDblStrUtilsUInt64(2520655369026404352),TPasDblStrUtilsUInt64(1694065894508600678)),
        (TPasDblStrUtilsUInt64(12374191248137781248),TPasDblStrUtilsUInt64(2117582368135750847)),(TPasDblStrUtilsUInt64(14651398557727195136),TPasDblStrUtilsUInt64(1323488980084844279)),
        (TPasDblStrUtilsUInt64(13702562178731606016),TPasDblStrUtilsUInt64(1654361225106055349)),(TPasDblStrUtilsUInt64(3293144668132343808),TPasDblStrUtilsUInt64(2067951531382569187)),
        (TPasDblStrUtilsUInt64(18199116482078572544),TPasDblStrUtilsUInt64(1292469707114105741)),(TPasDblStrUtilsUInt64(8913837547316051968),TPasDblStrUtilsUInt64(1615587133892632177)),
        (TPasDblStrUtilsUInt64(15753982952572452864),TPasDblStrUtilsUInt64(2019483917365790221)),(TPasDblStrUtilsUInt64(12152082354571476992),TPasDblStrUtilsUInt64(1262177448353618888)),
        (TPasDblStrUtilsUInt64(15190102943214346240),TPasDblStrUtilsUInt64(1577721810442023610)),(TPasDblStrUtilsUInt64(9764256642163156992),TPasDblStrUtilsUInt64(1972152263052529513)),
        (TPasDblStrUtilsUInt64(17631875447420442880),TPasDblStrUtilsUInt64(1232595164407830945)),(TPasDblStrUtilsUInt64(8204786253993389888),TPasDblStrUtilsUInt64(1540743955509788682)),
        (TPasDblStrUtilsUInt64(1032610780636961552),TPasDblStrUtilsUInt64(1925929944387235853)),(TPasDblStrUtilsUInt64(2951224747111794922),TPasDblStrUtilsUInt64(1203706215242022408)),
        (TPasDblStrUtilsUInt64(3689030933889743652),TPasDblStrUtilsUInt64(1504632769052528010)),(TPasDblStrUtilsUInt64(13834660704216955373),TPasDblStrUtilsUInt64(1880790961315660012)),
        (TPasDblStrUtilsUInt64(17870034976990372916),TPasDblStrUtilsUInt64(1175494350822287507)),(TPasDblStrUtilsUInt64(17725857702810578241),TPasDblStrUtilsUInt64(1469367938527859384)),
        (TPasDblStrUtilsUInt64(3710578054803671186),TPasDblStrUtilsUInt64(1836709923159824231)),(TPasDblStrUtilsUInt64(26536550077201078),TPasDblStrUtilsUInt64(2295887403949780289)),
        (TPasDblStrUtilsUInt64(11545800389866720434),TPasDblStrUtilsUInt64(1434929627468612680)),(TPasDblStrUtilsUInt64(14432250487333400542),TPasDblStrUtilsUInt64(1793662034335765850)),
        (TPasDblStrUtilsUInt64(8816941072311974870),TPasDblStrUtilsUInt64(2242077542919707313)),(TPasDblStrUtilsUInt64(17039803216263454053),TPasDblStrUtilsUInt64(1401298464324817070)),
        (TPasDblStrUtilsUInt64(12076381983474541759),TPasDblStrUtilsUInt64(1751623080406021338)),(TPasDblStrUtilsUInt64(5872105442488401391),TPasDblStrUtilsUInt64(2189528850507526673)),
        (TPasDblStrUtilsUInt64(15199280947623720629),TPasDblStrUtilsUInt64(1368455531567204170)),(TPasDblStrUtilsUInt64(9775729147674874978),TPasDblStrUtilsUInt64(1710569414459005213)),
        (TPasDblStrUtilsUInt64(16831347453020981627),TPasDblStrUtilsUInt64(2138211768073756516)),(TPasDblStrUtilsUInt64(1296220121283337709),TPasDblStrUtilsUInt64(1336382355046097823)),
        (TPasDblStrUtilsUInt64(15455333206886335848),TPasDblStrUtilsUInt64(1670477943807622278)),(TPasDblStrUtilsUInt64(10095794471753144002),TPasDblStrUtilsUInt64(2088097429759527848)),
        (TPasDblStrUtilsUInt64(6309871544845715001),TPasDblStrUtilsUInt64(1305060893599704905)),(TPasDblStrUtilsUInt64(12499025449484531656),TPasDblStrUtilsUInt64(1631326116999631131)),
        (TPasDblStrUtilsUInt64(11012095793428276666),TPasDblStrUtilsUInt64(2039157646249538914)),(TPasDblStrUtilsUInt64(11494245889320060820),TPasDblStrUtilsUInt64(1274473528905961821)),
        (TPasDblStrUtilsUInt64(532749306367912313),TPasDblStrUtilsUInt64(1593091911132452277)),(TPasDblStrUtilsUInt64(5277622651387278295),TPasDblStrUtilsUInt64(1991364888915565346)),
        (TPasDblStrUtilsUInt64(7910200175544436838),TPasDblStrUtilsUInt64(1244603055572228341)),(TPasDblStrUtilsUInt64(14499436237857933952),TPasDblStrUtilsUInt64(1555753819465285426)),
        (TPasDblStrUtilsUInt64(8900923260467641632),TPasDblStrUtilsUInt64(1944692274331606783)),(TPasDblStrUtilsUInt64(12480606065433357876),TPasDblStrUtilsUInt64(1215432671457254239)),
        (TPasDblStrUtilsUInt64(10989071563364309441),TPasDblStrUtilsUInt64(1519290839321567799)),(TPasDblStrUtilsUInt64(9124653435777998898),TPasDblStrUtilsUInt64(1899113549151959749)),
        (TPasDblStrUtilsUInt64(8008751406574943263),TPasDblStrUtilsUInt64(1186945968219974843)),(TPasDblStrUtilsUInt64(5399253239791291175),TPasDblStrUtilsUInt64(1483682460274968554)),
        (TPasDblStrUtilsUInt64(15972438586593889776),TPasDblStrUtilsUInt64(1854603075343710692)),(TPasDblStrUtilsUInt64(759402079766405302),TPasDblStrUtilsUInt64(1159126922089819183)),
        (TPasDblStrUtilsUInt64(14784310654990170340),TPasDblStrUtilsUInt64(1448908652612273978)),(TPasDblStrUtilsUInt64(9257016281882937117),TPasDblStrUtilsUInt64(1811135815765342473)),
        (TPasDblStrUtilsUInt64(16182956370781059300),TPasDblStrUtilsUInt64(2263919769706678091)),(TPasDblStrUtilsUInt64(7808504722524468110),TPasDblStrUtilsUInt64(1414949856066673807)),
        (TPasDblStrUtilsUInt64(5148944884728197234),TPasDblStrUtilsUInt64(1768687320083342259)),(TPasDblStrUtilsUInt64(1824495087482858639),TPasDblStrUtilsUInt64(2210859150104177824)),
        (TPasDblStrUtilsUInt64(1140309429676786649),TPasDblStrUtilsUInt64(1381786968815111140)),(TPasDblStrUtilsUInt64(1425386787095983311),TPasDblStrUtilsUInt64(1727233711018888925)),
        (TPasDblStrUtilsUInt64(6393419502297367043),TPasDblStrUtilsUInt64(2159042138773611156)),(TPasDblStrUtilsUInt64(13219259225790630210),TPasDblStrUtilsUInt64(1349401336733506972)),
        (TPasDblStrUtilsUInt64(16524074032238287762),TPasDblStrUtilsUInt64(1686751670916883715)),(TPasDblStrUtilsUInt64(16043406521870471799),TPasDblStrUtilsUInt64(2108439588646104644)),
        (TPasDblStrUtilsUInt64(803757039314269066),TPasDblStrUtilsUInt64(1317774742903815403)),(TPasDblStrUtilsUInt64(14839754354425000045),TPasDblStrUtilsUInt64(1647218428629769253)),
        (TPasDblStrUtilsUInt64(4714634887749086344),TPasDblStrUtilsUInt64(2059023035787211567)),(TPasDblStrUtilsUInt64(9864175832484260821),TPasDblStrUtilsUInt64(1286889397367007229)),
        (TPasDblStrUtilsUInt64(16941905809032713930),TPasDblStrUtilsUInt64(1608611746708759036)),(TPasDblStrUtilsUInt64(2730638187581340797),TPasDblStrUtilsUInt64(2010764683385948796)),
        (TPasDblStrUtilsUInt64(10930020904093113806),TPasDblStrUtilsUInt64(1256727927116217997)),(TPasDblStrUtilsUInt64(18274212148543780162),TPasDblStrUtilsUInt64(1570909908895272496)),
        (TPasDblStrUtilsUInt64(4396021111970173586),TPasDblStrUtilsUInt64(1963637386119090621)),(TPasDblStrUtilsUInt64(5053356204195052443),TPasDblStrUtilsUInt64(1227273366324431638)),
        (TPasDblStrUtilsUInt64(15540067292098591362),TPasDblStrUtilsUInt64(1534091707905539547)),(TPasDblStrUtilsUInt64(14813398096695851299),TPasDblStrUtilsUInt64(1917614634881924434)),
        (TPasDblStrUtilsUInt64(13870059828862294966),TPasDblStrUtilsUInt64(1198509146801202771)),(TPasDblStrUtilsUInt64(12725888767650480803),TPasDblStrUtilsUInt64(1498136433501503464)),
        (TPasDblStrUtilsUInt64(15907360959563101004),TPasDblStrUtilsUInt64(1872670541876879330)),(TPasDblStrUtilsUInt64(14553786618154326031),TPasDblStrUtilsUInt64(1170419088673049581)),
        (TPasDblStrUtilsUInt64(4357175217410743827),TPasDblStrUtilsUInt64(1463023860841311977)),(TPasDblStrUtilsUInt64(10058155040190817688),TPasDblStrUtilsUInt64(1828779826051639971)),
        (TPasDblStrUtilsUInt64(7961007781811134206),TPasDblStrUtilsUInt64(2285974782564549964)),(TPasDblStrUtilsUInt64(14199001900486734687),TPasDblStrUtilsUInt64(1428734239102843727)),
        (TPasDblStrUtilsUInt64(13137066357181030455),TPasDblStrUtilsUInt64(1785917798878554659)),(TPasDblStrUtilsUInt64(11809646928048900164),TPasDblStrUtilsUInt64(2232397248598193324)),
        (TPasDblStrUtilsUInt64(16604401366885338411),TPasDblStrUtilsUInt64(1395248280373870827)),(TPasDblStrUtilsUInt64(16143815690179285109),TPasDblStrUtilsUInt64(1744060350467338534)),
        (TPasDblStrUtilsUInt64(10956397575869330579),TPasDblStrUtilsUInt64(2180075438084173168)),(TPasDblStrUtilsUInt64(6847748484918331612),TPasDblStrUtilsUInt64(1362547148802608230)),
        (TPasDblStrUtilsUInt64(17783057643002690323),TPasDblStrUtilsUInt64(1703183936003260287)),(TPasDblStrUtilsUInt64(17617136035325974999),TPasDblStrUtilsUInt64(2128979920004075359)),
        (TPasDblStrUtilsUInt64(17928239049719816230),TPasDblStrUtilsUInt64(1330612450002547099)),(TPasDblStrUtilsUInt64(17798612793722382384),TPasDblStrUtilsUInt64(1663265562503183874)),
        (TPasDblStrUtilsUInt64(13024893955298202172),TPasDblStrUtilsUInt64(2079081953128979843)),(TPasDblStrUtilsUInt64(5834715712847682405),TPasDblStrUtilsUInt64(1299426220705612402)),
        (TPasDblStrUtilsUInt64(16516766677914378815),TPasDblStrUtilsUInt64(1624282775882015502)),(TPasDblStrUtilsUInt64(11422586310538197711),TPasDblStrUtilsUInt64(2030353469852519378)),
        (TPasDblStrUtilsUInt64(11750802462513761473),TPasDblStrUtilsUInt64(1268970918657824611)),(TPasDblStrUtilsUInt64(10076817059714813937),TPasDblStrUtilsUInt64(1586213648322280764)),
        (TPasDblStrUtilsUInt64(12596021324643517422),TPasDblStrUtilsUInt64(1982767060402850955)),(TPasDblStrUtilsUInt64(5566670318688504437),TPasDblStrUtilsUInt64(1239229412751781847)),
        (TPasDblStrUtilsUInt64(2346651879933242642),TPasDblStrUtilsUInt64(1549036765939727309)),(TPasDblStrUtilsUInt64(7545000868343941206),TPasDblStrUtilsUInt64(1936295957424659136)),
        (TPasDblStrUtilsUInt64(4715625542714963254),TPasDblStrUtilsUInt64(1210184973390411960)),(TPasDblStrUtilsUInt64(5894531928393704067),TPasDblStrUtilsUInt64(1512731216738014950)),
        (TPasDblStrUtilsUInt64(16591536947346905892),TPasDblStrUtilsUInt64(1890914020922518687)),(TPasDblStrUtilsUInt64(17287239619732898039),TPasDblStrUtilsUInt64(1181821263076574179)),
        (TPasDblStrUtilsUInt64(16997363506238734644),TPasDblStrUtilsUInt64(1477276578845717724)),(TPasDblStrUtilsUInt64(2799960309088866689),TPasDblStrUtilsUInt64(1846595723557147156)),
        (TPasDblStrUtilsUInt64(10973347230035317489),TPasDblStrUtilsUInt64(1154122327223216972)),(TPasDblStrUtilsUInt64(13716684037544146861),TPasDblStrUtilsUInt64(1442652909029021215)),
        (TPasDblStrUtilsUInt64(12534169028502795672),TPasDblStrUtilsUInt64(1803316136286276519)),(TPasDblStrUtilsUInt64(11056025267201106687),TPasDblStrUtilsUInt64(2254145170357845649)),
        (TPasDblStrUtilsUInt64(18439230838069161439),TPasDblStrUtilsUInt64(1408840731473653530)),(TPasDblStrUtilsUInt64(13825666510731675991),TPasDblStrUtilsUInt64(1761050914342066913)),
        (TPasDblStrUtilsUInt64(3447025083132431277),TPasDblStrUtilsUInt64(2201313642927583642)),(TPasDblStrUtilsUInt64(6766076695385157452),TPasDblStrUtilsUInt64(1375821026829739776)),
        (TPasDblStrUtilsUInt64(8457595869231446815),TPasDblStrUtilsUInt64(1719776283537174720)),(TPasDblStrUtilsUInt64(10571994836539308519),TPasDblStrUtilsUInt64(2149720354421468400)),
        (TPasDblStrUtilsUInt64(6607496772837067824),TPasDblStrUtilsUInt64(1343575221513417750)),(TPasDblStrUtilsUInt64(17482743002901110588),TPasDblStrUtilsUInt64(1679469026891772187)),
        (TPasDblStrUtilsUInt64(17241742735199000331),TPasDblStrUtilsUInt64(2099336283614715234)),(TPasDblStrUtilsUInt64(15387775227926763111),TPasDblStrUtilsUInt64(1312085177259197021)),
        (TPasDblStrUtilsUInt64(5399660979626290177),TPasDblStrUtilsUInt64(1640106471573996277)),(TPasDblStrUtilsUInt64(11361262242960250625),TPasDblStrUtilsUInt64(2050133089467495346)),
        (TPasDblStrUtilsUInt64(11712474920277544544),TPasDblStrUtilsUInt64(1281333180917184591)),(TPasDblStrUtilsUInt64(10028907631919542777),TPasDblStrUtilsUInt64(1601666476146480739)),
        (TPasDblStrUtilsUInt64(7924448521472040567),TPasDblStrUtilsUInt64(2002083095183100924)),(TPasDblStrUtilsUInt64(14176152362774801162),TPasDblStrUtilsUInt64(1251301934489438077)),
        (TPasDblStrUtilsUInt64(3885132398186337741),TPasDblStrUtilsUInt64(1564127418111797597)),(TPasDblStrUtilsUInt64(9468101516160310080),TPasDblStrUtilsUInt64(1955159272639746996)),
        (TPasDblStrUtilsUInt64(15140935484454969608),TPasDblStrUtilsUInt64(1221974545399841872)),(TPasDblStrUtilsUInt64(479425281859160394),TPasDblStrUtilsUInt64(1527468181749802341)),
        (TPasDblStrUtilsUInt64(5210967620751338397),TPasDblStrUtilsUInt64(1909335227187252926)),(TPasDblStrUtilsUInt64(17091912818251750210),TPasDblStrUtilsUInt64(1193334516992033078)),
        (TPasDblStrUtilsUInt64(12141518985959911954),TPasDblStrUtilsUInt64(1491668146240041348)),(TPasDblStrUtilsUInt64(15176898732449889943),TPasDblStrUtilsUInt64(1864585182800051685)),
        (TPasDblStrUtilsUInt64(11791404716994875166),TPasDblStrUtilsUInt64(1165365739250032303)),(TPasDblStrUtilsUInt64(10127569877816206054),TPasDblStrUtilsUInt64(1456707174062540379)),
        (TPasDblStrUtilsUInt64(8047776328842869663),TPasDblStrUtilsUInt64(1820883967578175474)),(TPasDblStrUtilsUInt64(836348374198811271),TPasDblStrUtilsUInt64(2276104959472719343)),
        (TPasDblStrUtilsUInt64(7440246761515338900),TPasDblStrUtilsUInt64(1422565599670449589)),(TPasDblStrUtilsUInt64(13911994470321561530),TPasDblStrUtilsUInt64(1778206999588061986)),
        (TPasDblStrUtilsUInt64(8166621051047176104),TPasDblStrUtilsUInt64(2222758749485077483)),(TPasDblStrUtilsUInt64(2798295147690791113),TPasDblStrUtilsUInt64(1389224218428173427)),
        (TPasDblStrUtilsUInt64(17332926989895652603),TPasDblStrUtilsUInt64(1736530273035216783)),(TPasDblStrUtilsUInt64(17054472718942177850),TPasDblStrUtilsUInt64(2170662841294020979)),
        (TPasDblStrUtilsUInt64(8353202440125167204),TPasDblStrUtilsUInt64(1356664275808763112)),(TPasDblStrUtilsUInt64(10441503050156459005),TPasDblStrUtilsUInt64(1695830344760953890)),
        (TPasDblStrUtilsUInt64(3828506775840797949),TPasDblStrUtilsUInt64(2119787930951192363)),(TPasDblStrUtilsUInt64(86973725686804766),TPasDblStrUtilsUInt64(1324867456844495227)),
        (TPasDblStrUtilsUInt64(13943775212390669669),TPasDblStrUtilsUInt64(1656084321055619033)),(TPasDblStrUtilsUInt64(3594660960206173375),TPasDblStrUtilsUInt64(2070105401319523792)),
        (TPasDblStrUtilsUInt64(2246663100128858359),TPasDblStrUtilsUInt64(1293815875824702370)),(TPasDblStrUtilsUInt64(12031700912015848757),TPasDblStrUtilsUInt64(1617269844780877962)),
        (TPasDblStrUtilsUInt64(5816254103165035138),TPasDblStrUtilsUInt64(2021587305976097453)),(TPasDblStrUtilsUInt64(5941001823691840913),TPasDblStrUtilsUInt64(1263492066235060908)),
        (TPasDblStrUtilsUInt64(7426252279614801142),TPasDblStrUtilsUInt64(1579365082793826135)),(TPasDblStrUtilsUInt64(4671129331091113523),TPasDblStrUtilsUInt64(1974206353492282669)),
        (TPasDblStrUtilsUInt64(5225298841145639904),TPasDblStrUtilsUInt64(1233878970932676668)),(TPasDblStrUtilsUInt64(6531623551432049880),TPasDblStrUtilsUInt64(1542348713665845835)),
        (TPasDblStrUtilsUInt64(3552843420862674446),TPasDblStrUtilsUInt64(1927935892082307294)),(TPasDblStrUtilsUInt64(16055585193321335241),TPasDblStrUtilsUInt64(1204959932551442058)),
        (TPasDblStrUtilsUInt64(10846109454796893243),TPasDblStrUtilsUInt64(1506199915689302573)),(TPasDblStrUtilsUInt64(18169322836923504458),TPasDblStrUtilsUInt64(1882749894611628216)),
        (TPasDblStrUtilsUInt64(11355826773077190286),TPasDblStrUtilsUInt64(1176718684132267635)),(TPasDblStrUtilsUInt64(9583097447919099954),TPasDblStrUtilsUInt64(1470898355165334544)),
        (TPasDblStrUtilsUInt64(11978871809898874942),TPasDblStrUtilsUInt64(1838622943956668180)),(TPasDblStrUtilsUInt64(14973589762373593678),TPasDblStrUtilsUInt64(2298278679945835225)),
        (TPasDblStrUtilsUInt64(2440964573842414192),TPasDblStrUtilsUInt64(1436424174966147016)),(TPasDblStrUtilsUInt64(3051205717303017741),TPasDblStrUtilsUInt64(1795530218707683770)),
        (TPasDblStrUtilsUInt64(13037379183483547984),TPasDblStrUtilsUInt64(2244412773384604712)),(TPasDblStrUtilsUInt64(8148361989677217490),TPasDblStrUtilsUInt64(1402757983365377945)),
        (TPasDblStrUtilsUInt64(14797138505523909766),TPasDblStrUtilsUInt64(1753447479206722431)),(TPasDblStrUtilsUInt64(13884737113477499304),TPasDblStrUtilsUInt64(2191809349008403039)),
        (TPasDblStrUtilsUInt64(15595489723564518921),TPasDblStrUtilsUInt64(1369880843130251899)),(TPasDblStrUtilsUInt64(14882676136028260747),TPasDblStrUtilsUInt64(1712351053912814874)),
        (TPasDblStrUtilsUInt64(9379973133180550126),TPasDblStrUtilsUInt64(2140438817391018593)),(TPasDblStrUtilsUInt64(17391698254306313589),TPasDblStrUtilsUInt64(1337774260869386620)),
        (TPasDblStrUtilsUInt64(3292878744173340370),TPasDblStrUtilsUInt64(1672217826086733276)),(TPasDblStrUtilsUInt64(4116098430216675462),TPasDblStrUtilsUInt64(2090272282608416595)),
        (TPasDblStrUtilsUInt64(266718509671728212),TPasDblStrUtilsUInt64(1306420176630260372)),(TPasDblStrUtilsUInt64(333398137089660265),TPasDblStrUtilsUInt64(1633025220787825465)),
        (TPasDblStrUtilsUInt64(5028433689789463235),TPasDblStrUtilsUInt64(2041281525984781831)),(TPasDblStrUtilsUInt64(10060300083759496378),TPasDblStrUtilsUInt64(1275800953740488644)),
        (TPasDblStrUtilsUInt64(12575375104699370472),TPasDblStrUtilsUInt64(1594751192175610805)),(TPasDblStrUtilsUInt64(1884160825592049379),TPasDblStrUtilsUInt64(1993438990219513507)),
        (TPasDblStrUtilsUInt64(17318501580490888525),TPasDblStrUtilsUInt64(1245899368887195941)),(TPasDblStrUtilsUInt64(7813068920331446945),TPasDblStrUtilsUInt64(1557374211108994927)),
        (TPasDblStrUtilsUInt64(5154650131986920777),TPasDblStrUtilsUInt64(1946717763886243659)),(TPasDblStrUtilsUInt64(915813323278131534),TPasDblStrUtilsUInt64(1216698602428902287)),
        (TPasDblStrUtilsUInt64(14979824709379828129),TPasDblStrUtilsUInt64(1520873253036127858)),(TPasDblStrUtilsUInt64(9501408849870009354),TPasDblStrUtilsUInt64(1901091566295159823)),
        (TPasDblStrUtilsUInt64(12855909558809837702),TPasDblStrUtilsUInt64(1188182228934474889)),(TPasDblStrUtilsUInt64(2234828893230133415),TPasDblStrUtilsUInt64(1485227786168093612)),
        (TPasDblStrUtilsUInt64(2793536116537666769),TPasDblStrUtilsUInt64(1856534732710117015)),(TPasDblStrUtilsUInt64(8663489100477123587),TPasDblStrUtilsUInt64(1160334207943823134)),
        (TPasDblStrUtilsUInt64(1605989338741628675),TPasDblStrUtilsUInt64(1450417759929778918)),(TPasDblStrUtilsUInt64(11230858710281811652),TPasDblStrUtilsUInt64(1813022199912223647)),
        (TPasDblStrUtilsUInt64(9426887369424876662),TPasDblStrUtilsUInt64(2266277749890279559)),(TPasDblStrUtilsUInt64(12809333633531629769),TPasDblStrUtilsUInt64(1416423593681424724)),
        (TPasDblStrUtilsUInt64(16011667041914537212),TPasDblStrUtilsUInt64(1770529492101780905)),(TPasDblStrUtilsUInt64(6179525747111007803),TPasDblStrUtilsUInt64(2213161865127226132)),
        (TPasDblStrUtilsUInt64(13085575628799155685),TPasDblStrUtilsUInt64(1383226165704516332)),(TPasDblStrUtilsUInt64(16356969535998944606),TPasDblStrUtilsUInt64(1729032707130645415)),
        (TPasDblStrUtilsUInt64(15834525901571292854),TPasDblStrUtilsUInt64(2161290883913306769)),(TPasDblStrUtilsUInt64(2979049660840976177),TPasDblStrUtilsUInt64(1350806802445816731)),
        (TPasDblStrUtilsUInt64(17558870131333383934),TPasDblStrUtilsUInt64(1688508503057270913)),(TPasDblStrUtilsUInt64(8113529608884566205),TPasDblStrUtilsUInt64(2110635628821588642)),
        (TPasDblStrUtilsUInt64(9682642023980241782),TPasDblStrUtilsUInt64(1319147268013492901)),(TPasDblStrUtilsUInt64(16714988548402690132),TPasDblStrUtilsUInt64(1648934085016866126)),
        (TPasDblStrUtilsUInt64(11670363648648586857),TPasDblStrUtilsUInt64(2061167606271082658)),(TPasDblStrUtilsUInt64(11905663298832754689),TPasDblStrUtilsUInt64(1288229753919426661)),
        (TPasDblStrUtilsUInt64(1047021068258779650),TPasDblStrUtilsUInt64(1610287192399283327)),(TPasDblStrUtilsUInt64(15143834390605638274),TPasDblStrUtilsUInt64(2012858990499104158)),
        (TPasDblStrUtilsUInt64(4853210475701136017),TPasDblStrUtilsUInt64(1258036869061940099)),(TPasDblStrUtilsUInt64(1454827076199032118),TPasDblStrUtilsUInt64(1572546086327425124)),
        (TPasDblStrUtilsUInt64(1818533845248790147),TPasDblStrUtilsUInt64(1965682607909281405)),(TPasDblStrUtilsUInt64(3442426662494187794),TPasDblStrUtilsUInt64(1228551629943300878)),
        (TPasDblStrUtilsUInt64(13526405364972510550),TPasDblStrUtilsUInt64(1535689537429126097)),(TPasDblStrUtilsUInt64(3072948650933474476),TPasDblStrUtilsUInt64(1919611921786407622)),
        (TPasDblStrUtilsUInt64(15755650962115585259),TPasDblStrUtilsUInt64(1199757451116504763)),(TPasDblStrUtilsUInt64(15082877684217093670),TPasDblStrUtilsUInt64(1499696813895630954)),
        (TPasDblStrUtilsUInt64(9630225068416591280),TPasDblStrUtilsUInt64(1874621017369538693)),(TPasDblStrUtilsUInt64(8324733676974063502),TPasDblStrUtilsUInt64(1171638135855961683)),
        (TPasDblStrUtilsUInt64(5794231077790191473),TPasDblStrUtilsUInt64(1464547669819952104)),(TPasDblStrUtilsUInt64(7242788847237739342),TPasDblStrUtilsUInt64(1830684587274940130)),
        (TPasDblStrUtilsUInt64(18276858095901949986),TPasDblStrUtilsUInt64(2288355734093675162)),(TPasDblStrUtilsUInt64(16034722328366106645),TPasDblStrUtilsUInt64(1430222333808546976)),
        (TPasDblStrUtilsUInt64(1596658836748081690),TPasDblStrUtilsUInt64(1787777917260683721)),(TPasDblStrUtilsUInt64(6607509564362490017),TPasDblStrUtilsUInt64(2234722396575854651)),
        (TPasDblStrUtilsUInt64(1823850468512862308),TPasDblStrUtilsUInt64(1396701497859909157)),(TPasDblStrUtilsUInt64(6891499104068465790),TPasDblStrUtilsUInt64(1745876872324886446)),
        (TPasDblStrUtilsUInt64(17837745916940358045),TPasDblStrUtilsUInt64(2182346090406108057)),(TPasDblStrUtilsUInt64(4231062170446641922),TPasDblStrUtilsUInt64(1363966306503817536)),
        (TPasDblStrUtilsUInt64(5288827713058302403),TPasDblStrUtilsUInt64(1704957883129771920)),(TPasDblStrUtilsUInt64(6611034641322878003),TPasDblStrUtilsUInt64(2131197353912214900)),
        (TPasDblStrUtilsUInt64(13355268687681574560),TPasDblStrUtilsUInt64(1331998346195134312)),(TPasDblStrUtilsUInt64(16694085859601968200),TPasDblStrUtilsUInt64(1664997932743917890)),
        (TPasDblStrUtilsUInt64(11644235287647684442),TPasDblStrUtilsUInt64(2081247415929897363)),(TPasDblStrUtilsUInt64(4971804045566108824),TPasDblStrUtilsUInt64(1300779634956185852)),
        (TPasDblStrUtilsUInt64(6214755056957636030),TPasDblStrUtilsUInt64(1625974543695232315)),(TPasDblStrUtilsUInt64(3156757802769657134),TPasDblStrUtilsUInt64(2032468179619040394)),
        (TPasDblStrUtilsUInt64(6584659645158423613),TPasDblStrUtilsUInt64(1270292612261900246)),(TPasDblStrUtilsUInt64(17454196593302805324),TPasDblStrUtilsUInt64(1587865765327375307)),
        (TPasDblStrUtilsUInt64(17206059723201118751),TPasDblStrUtilsUInt64(1984832206659219134)),(TPasDblStrUtilsUInt64(6142101308573311315),TPasDblStrUtilsUInt64(1240520129162011959)),
        (TPasDblStrUtilsUInt64(3065940617289251240),TPasDblStrUtilsUInt64(1550650161452514949)),(TPasDblStrUtilsUInt64(8444111790038951954),TPasDblStrUtilsUInt64(1938312701815643686)),
        (TPasDblStrUtilsUInt64(665883850346957067),TPasDblStrUtilsUInt64(1211445438634777304)),(TPasDblStrUtilsUInt64(832354812933696334),TPasDblStrUtilsUInt64(1514306798293471630)),
        (TPasDblStrUtilsUInt64(10263815553021896226),TPasDblStrUtilsUInt64(1892883497866839537)),(TPasDblStrUtilsUInt64(17944099766707154901),TPasDblStrUtilsUInt64(1183052186166774710)),
        (TPasDblStrUtilsUInt64(13206752671529167818),TPasDblStrUtilsUInt64(1478815232708468388)),(TPasDblStrUtilsUInt64(16508440839411459773),TPasDblStrUtilsUInt64(1848519040885585485)),
        (TPasDblStrUtilsUInt64(12623618533845856310),TPasDblStrUtilsUInt64(1155324400553490928)),(TPasDblStrUtilsUInt64(15779523167307320387),TPasDblStrUtilsUInt64(1444155500691863660)),
        (TPasDblStrUtilsUInt64(1277659885424598868),TPasDblStrUtilsUInt64(1805194375864829576)),(TPasDblStrUtilsUInt64(1597074856780748586),TPasDblStrUtilsUInt64(2256492969831036970)),
        (TPasDblStrUtilsUInt64(5609857803915355770),TPasDblStrUtilsUInt64(1410308106144398106)),(TPasDblStrUtilsUInt64(16235694291748970521),TPasDblStrUtilsUInt64(1762885132680497632)),
        (TPasDblStrUtilsUInt64(1847873790976661535),TPasDblStrUtilsUInt64(2203606415850622041)),(TPasDblStrUtilsUInt64(12684136165428883219),TPasDblStrUtilsUInt64(1377254009906638775)),
        (TPasDblStrUtilsUInt64(11243484188358716120),TPasDblStrUtilsUInt64(1721567512383298469)),(TPasDblStrUtilsUInt64(219297180166231438),TPasDblStrUtilsUInt64(2151959390479123087)),
        (TPasDblStrUtilsUInt64(7054589765244976505),TPasDblStrUtilsUInt64(1344974619049451929)),(TPasDblStrUtilsUInt64(13429923224983608535),TPasDblStrUtilsUInt64(1681218273811814911)),
        (TPasDblStrUtilsUInt64(12175718012802122765),TPasDblStrUtilsUInt64(2101522842264768639)),(TPasDblStrUtilsUInt64(14527352785642408584),TPasDblStrUtilsUInt64(1313451776415480399)),
        (TPasDblStrUtilsUInt64(13547504963625622826),TPasDblStrUtilsUInt64(1641814720519350499)),(TPasDblStrUtilsUInt64(12322695186104640628),TPasDblStrUtilsUInt64(2052268400649188124)),
        (TPasDblStrUtilsUInt64(16925056528170176201),TPasDblStrUtilsUInt64(1282667750405742577)),(TPasDblStrUtilsUInt64(7321262604930556539),TPasDblStrUtilsUInt64(1603334688007178222)),
        (TPasDblStrUtilsUInt64(18374950293017971482),TPasDblStrUtilsUInt64(2004168360008972777)),(TPasDblStrUtilsUInt64(4566814905495150320),TPasDblStrUtilsUInt64(1252605225005607986)),
        (TPasDblStrUtilsUInt64(14931890668723713708),TPasDblStrUtilsUInt64(1565756531257009982)),(TPasDblStrUtilsUInt64(9441491299049866327),TPasDblStrUtilsUInt64(1957195664071262478)),
        (TPasDblStrUtilsUInt64(1289246043478778550),TPasDblStrUtilsUInt64(1223247290044539049)),(TPasDblStrUtilsUInt64(6223243572775861092),TPasDblStrUtilsUInt64(1529059112555673811)),
        (TPasDblStrUtilsUInt64(3167368447542438461),TPasDblStrUtilsUInt64(1911323890694592264)),(TPasDblStrUtilsUInt64(1979605279714024038),TPasDblStrUtilsUInt64(1194577431684120165)),
        (TPasDblStrUtilsUInt64(7086192618069917952),TPasDblStrUtilsUInt64(1493221789605150206)),(TPasDblStrUtilsUInt64(18081112809442173248),TPasDblStrUtilsUInt64(1866527237006437757)),
        (TPasDblStrUtilsUInt64(13606538515115052232),TPasDblStrUtilsUInt64(1166579523129023598)),(TPasDblStrUtilsUInt64(7784801107039039482),TPasDblStrUtilsUInt64(1458224403911279498)),
        (TPasDblStrUtilsUInt64(507629346944023544),TPasDblStrUtilsUInt64(1822780504889099373)),(TPasDblStrUtilsUInt64(5246222702107417334),TPasDblStrUtilsUInt64(2278475631111374216)),
        (TPasDblStrUtilsUInt64(3278889188817135834),TPasDblStrUtilsUInt64(1424047269444608885)),(TPasDblStrUtilsUInt64(8710297504448807696),TPasDblStrUtilsUInt64(1780059086805761106))
       );

function UInt64Bits2Double(const Bits:TPasDblStrUtilsUInt64):TPasDblStrUtilsDouble;
begin
 result:=TPasDblStrUtilsDouble(Pointer(@Bits)^);
end;

function RyuStringToDouble(const aStringValue:TPasDblStrUtilsString;const aOK:PPasDblStrUtilsBoolean=nil;const aStrict:boolean=false):TPasDblStrUtilsDouble;
const DOUBLE_MANTISSA_BITS=52;
      DOUBLE_EXPONENT_BITS=11;
      DOUBLE_EXPONENT_BIAS=1023;
var InputStringLength,
    CountBase10MantissaDigits,ExtraCountBase10MantissaDigits,CountBase10ExponentDigits,
    DotPosition,ExponentPosition,
    Base10MantissaBits,
    Base10Exponent,Position,Base2Exponent,Shift,Temporary,Exponent:TPasDblStrUtilsInt32;
    Base10Mantissa,Base2Mantissa,IEEEMantissa:TPasDblStrUtilsUInt64;
    IEEEExponent,LastRemovedBit:TPasDblStrUtilsUInt32;
    SignedMantissa,SignedExponent,TrailingZeros,RoundUp,ExtraRoundUp:boolean;
    c:AnsiChar;
begin
 if assigned(aOK) then begin
  aOK^:=false;
 end;
 InputStringLength:=length(aStringValue);
 if InputStringLength=0 then begin
  result:=0.0;
  exit;
 end;
 CountBase10MantissaDigits:=0;
 ExtraCountBase10MantissaDigits:=0;
 CountBase10ExponentDigits:=0;
 DotPosition:=InputStringLength+1;
 ExponentPosition:=InputStringLength+1;
 Base10Mantissa:=0;
 Base10Exponent:=0;
 SignedMantissa:=false;
 SignedExponent:=false;
 ExtraRoundUp:=false;
 Position:=1;
 while (Position<=InputStringLength) and (aStringValue[Position] in [#0..#32]) do begin
  inc(Position);
 end;
 while (Position<=InputStringLength) and (aStringValue[Position]='-') do begin
  SignedMantissa:=not SignedMantissa;
  inc(Position);
 end;
 if (Position+2)<=InputStringLength then begin
  if (aStringValue[Position] in ['n','N']) and
     (aStringValue[Position+1] in ['a','A']) and
     (aStringValue[Position+2] in ['n','N']) then begin
   if SignedMantissa then begin
    result:=UInt64Bits2Double(TPasDblStrUtilsUInt64($fff8000000000000)); // -NaN
   end else begin
    result:=UInt64Bits2Double(TPasDblStrUtilsUInt64($7ff8000000000000)); // +NaN
   end;
   if assigned(aOK) then begin
    aOK^:=true;
   end;
   exit;
  end else if (aStringValue[Position] in ['i','I']) and
              (aStringValue[Position+1] in ['n','N']) and
              (aStringValue[Position+2] in ['f','F']) then begin
   if SignedMantissa then begin
    result:=UInt64Bits2Double(TPasDblStrUtilsUInt64($fff0000000000000)); // -Inf
   end else begin
    result:=UInt64Bits2Double(TPasDblStrUtilsUInt64($7ff0000000000000)); // +Inf
   end;
   if assigned(aOK) then begin
    aOK^:=true;
   end;
   exit;
  end;
 end;
 while Position<=InputStringLength do begin
  c:=aStringValue[Position];
  case c of
   '.':begin
    if DotPosition<>(InputStringLength+1) then begin
     result:=0.0;
     exit;
    end;
    DotPosition:=Position;
   end;
   '0'..'9':begin
    if CountBase10MantissaDigits<19 then begin
     Base10Mantissa:=(Base10Mantissa*10)+TPasDblStrUtilsUInt64(TPasDblStrUtilsUInt8(AnsiChar(c))-TPasDblStrUtilsUInt8(AnsiChar('0')));
     if Base10Mantissa<>0 then begin
      inc(CountBase10MantissaDigits);
     end;
    end else begin
     inc(ExtraCountBase10MantissaDigits);
    end;
   end;
   else begin
    break;
   end;
  end;
  inc(Position);
 end;
 if (Position<=InputStringLength) and (aStringValue[Position] in ['e','E']) then begin
  ExponentPosition:=Position;
  inc(Position);
  if (Position<=InputStringLength) and (aStringValue[Position] in ['-','+']) then begin
   SignedExponent:=aStringValue[Position]='-';
   inc(Position);
  end;
  while Position<=InputStringLength do begin
   c:=aStringValue[Position];
   case c of
    '0'..'9':begin
     if CountBase10ExponentDigits>3 then begin
      if SignedExponent or (Base10Mantissa=0) then begin
       if SignedMantissa then begin
        result:=UInt64Bits2Double(TPasDblStrUtilsUInt64($8000000000000000)); // -0
       end else begin
        result:=UInt64Bits2Double(TPasDblStrUtilsUInt64($0000000000000000)); // +0
       end;
      end else begin
       if SignedMantissa then begin
        result:=UInt64Bits2Double(TPasDblStrUtilsUInt64($fff0000000000000)); // -Inf
       end else begin
        result:=UInt64Bits2Double(TPasDblStrUtilsUInt64($7ff0000000000000)); // +Inf
       end;
      end;
      if assigned(aOK) then begin
       aOK^:=true;
      end;
      exit;
     end;
     Base10Exponent:=(Base10Exponent*10)+(TPasDblStrUtilsUInt8(AnsiChar(c))-TPasDblStrUtilsUInt8(AnsiChar('0')));
     if Base10Exponent<>0 then begin
      inc(CountBase10ExponentDigits);
     end;
    end;
    else begin
     result:=0.0;
     exit;
    end;
   end;
   inc(Position);
  end;
 end;
 if Position<=InputStringLength then begin
  result:=0.0;
  exit;
 end;
 if SignedExponent then begin
  Base10Exponent:=-Base10Exponent;
 end;
 inc(Base10Exponent,ExtraCountBase10MantissaDigits);
 if DotPosition<ExponentPosition then begin
  dec(Base10Exponent,(ExponentPosition-DotPosition)-1);
 end;
 if Base10Mantissa=0 then begin
  if SignedMantissa then begin
   result:=UInt64Bits2Double(TPasDblStrUtilsUInt64($8000000000000000)); // -0
  end else begin
   result:=UInt64Bits2Double(TPasDblStrUtilsUInt64($0000000000000000)); // +0
  end;
  if assigned(aOK) then begin
   aOK^:=true;
  end;
  exit;
 end;
 if ((CountBase10MantissaDigits+Base10Exponent)<=-324) or (Base10Mantissa=0) then begin
  result:=UInt64Bits2Double(TPasDblStrUtilsUInt64((ord(SignedMantissa) and 1)) shl (DOUBLE_EXPONENT_BITS+DOUBLE_MANTISSA_BITS));
  if assigned(aOK) then begin
   aOK^:=true;
  end;
  exit;
 end;
 if (CountBase10MantissaDigits+Base10Exponent)>=310 then begin
  result:=UInt64Bits2Double((TPasDblStrUtilsUInt64((ord(SignedMantissa) and 1)) shl (DOUBLE_EXPONENT_BITS+DOUBLE_MANTISSA_BITS)) or (TPasDblStrUtilsUInt64($7ff) shl DOUBLE_MANTISSA_BITS));
  if assigned(aOK) then begin
   aOK^:=false;
  end;
  exit;
 end;
 if Base10Exponent>=0 then begin
  while Base10Exponent>=DOUBLE_POW5_TABLE_SIZE do begin
   Base10Mantissa:=RoundDiv10(Base10Mantissa);
   dec(Base10Exponent);
  end;
  Base10MantissaBits:=FloorLog2(Base10Mantissa);
  Base2Exponent:=((TPasDblStrUtilsInt32(Base10MantissaBits)+Base10Exponent)+TPasDblStrUtilsInt32(Log2Pow5(Base10Exponent)))-(DOUBLE_MANTISSA_BITS+1);
  Temporary:=((Base2Exponent-Base10Exponent)-CeilLog2Pow5(Base10Exponent))+DOUBLE_POW5_BITCOUNT;
  Assert(Temporary>=0);
  Base2Mantissa:=MulShift64(Base10Mantissa,@DOUBLE_POW5_SPLIT[Base10Exponent],Temporary);
  TrailingZeros:=(Base2Exponent<Base10Exponent) or
                 (((Base2Exponent-Base10Exponent)<64) and
                  MultipleOfPowerOf2(Base10Mantissa,Base2Exponent-Base10Exponent));
 end else begin
  while (-Base10Exponent)>=DOUBLE_POW5_INV_TABLE_SIZE do begin
   Base10Mantissa:=RoundDiv10(Base10Mantissa);
   inc(Base10Exponent);
  end;
  Base10MantissaBits:=FloorLog2(Base10Mantissa);
  Base2Exponent:=((TPasDblStrUtilsInt32(Base10MantissaBits)+Base10Exponent)-TPasDblStrUtilsInt32(CeilLog2Pow5(-Base10Exponent)))-(DOUBLE_MANTISSA_BITS+1);
  Temporary:=(((Base2Exponent-Base10Exponent)+CeilLog2Pow5(-Base10Exponent))-1)+DOUBLE_POW5_INV_BITCOUNT;
  assert((-Base10Exponent)<DOUBLE_POW5_INV_TABLE_SIZE);
  Base2Mantissa:=MulShift64(Base10Mantissa,@DOUBLE_POW5_INV_SPLIT[-Base10Exponent],Temporary);
  TrailingZeros:=MultipleOfPowerOf5(Base10Mantissa,-Base10Exponent);
 end;
 Exponent:=Base2Exponent+DOUBLE_EXPONENT_BIAS+TPasDblStrUtilsInt32(FloorLog2(Base2Mantissa));
 if Exponent<0 then begin
  IEEEExponent:=0;
 end else begin
  IEEEExponent:=Exponent;
 end;
 if IEEEExponent>$7fe then begin
  result:=UInt64Bits2Double((TPasDblStrUtilsUInt64((ord(SignedMantissa) and 1)) shl (DOUBLE_EXPONENT_BITS+DOUBLE_MANTISSA_BITS)) or (TPasDblStrUtilsUInt64($7ff) shl DOUBLE_MANTISSA_BITS));
  if assigned(aOK) then begin
   aOK^:=true;
  end;
  exit;
 end;
 if IEEEExponent=0 then begin
  Shift:=1;
 end else begin
  Shift:=IEEEExponent;
 end;
 Shift:=(Shift-Base2Exponent)-(DOUBLE_EXPONENT_BIAS+DOUBLE_MANTISSA_BITS);
 Assert(Shift>=0);
 TrailingZeros:=TrailingZeros and ((Base2Mantissa and ((TPasDblStrUtilsUInt64(1) shl (Shift-1))-1))=0);
 LastRemovedBit:=(Base2Mantissa shr (Shift-1)) and 1;
 RoundUp:=ExtraRoundUp or ((LastRemovedBit<>0) and ((not TrailingZeros) or (((Base2Mantissa shr Shift) and 1)<>0)));
 IEEEMantissa:=(Base2Mantissa shr Shift)+TPasDblStrUtilsUInt64(ord(RoundUp) and 1);
 Assert(IEEEMantissa<=(TPasDblStrUtilsUInt64(1) shl (DOUBLE_MANTISSA_BITS+1)));
 IEEEMantissa:=IEEEMantissa and ((TPasDblStrUtilsUInt64(1) shl DOUBLE_MANTISSA_BITS)-1);
 if (IEEEMantissa=0) and RoundUp then begin
  inc(IEEEExponent);
 end;
 result:=UInt64Bits2Double((TPasDblStrUtilsUInt64(ord(SignedMantissa) and 1) shl (DOUBLE_EXPONENT_BITS+DOUBLE_MANTISSA_BITS)) or (TPasDblStrUtilsUInt64(IEEEExponent) shl DOUBLE_MANTISSA_BITS) or IEEEMantissa);
 if assigned(aOK) then begin
  aOK^:=(not aStrict) or (ExtraCountBase10MantissaDigits=0);
 end;
end;

function RyuDoubleToString(const aValue:TPasDblStrUtilsDouble;const aExponential:boolean=true):TPasDblStrUtilsString;
const DOUBLE_MANTISSA_BITS=52;
      DOUBLE_EXPONENT_BITS=11;
      DOUBLE_BIAS=1023;
type TFloatingDecimal64=record
      Mantissa:TPasDblStrUtilsUInt64;
      Exponent:TPasDblStrUtilsInt32;
     end;
 function DecimalLength17(const aValue:TPasDblStrUtilsUInt64):TPasDblStrUtilsUInt32;
 begin
{$ifdef fpc}
  case aValue of
   TPasDblStrUtilsUInt64(0)..TPasDblStrUtilsUInt64(9):begin
    result:=1;
   end;
   TPasDblStrUtilsUInt64(10)..TPasDblStrUtilsUInt64(99):begin
    result:=2;
   end;
   TPasDblStrUtilsUInt64(100)..TPasDblStrUtilsUInt64(999):begin
    result:=3;
   end;
   TPasDblStrUtilsUInt64(1000)..TPasDblStrUtilsUInt64(9999):begin
    result:=4;
   end;
   TPasDblStrUtilsUInt64(10000)..TPasDblStrUtilsUInt64(99999):begin
    result:=5;
   end;
   TPasDblStrUtilsUInt64(100000)..TPasDblStrUtilsUInt64(999999):begin
    result:=6;
   end;
   TPasDblStrUtilsUInt64(1000000)..TPasDblStrUtilsUInt64(9999999):begin
    result:=7;
   end;
   TPasDblStrUtilsUInt64(10000000)..TPasDblStrUtilsUInt64(99999999):begin
    result:=8;
   end;
   TPasDblStrUtilsUInt64(100000000)..TPasDblStrUtilsUInt64(999999999):begin
    result:=9;
   end;
   TPasDblStrUtilsUInt64(1000000000)..TPasDblStrUtilsUInt64(9999999999):begin
    result:=10;
   end;
   TPasDblStrUtilsUInt64(10000000000)..TPasDblStrUtilsUInt64(99999999999):begin
    result:=11;
   end;
   TPasDblStrUtilsUInt64(100000000000)..TPasDblStrUtilsUInt64(999999999999):begin
    result:=12;
   end;
   TPasDblStrUtilsUInt64(1000000000000)..TPasDblStrUtilsUInt64(9999999999999):begin
    result:=13;
   end;
   TPasDblStrUtilsUInt64(10000000000000)..TPasDblStrUtilsUInt64(99999999999999):begin
    result:=14;
   end;
   TPasDblStrUtilsUInt64(100000000000000)..TPasDblStrUtilsUInt64(999999999999999):begin
    result:=15;
   end;
   TPasDblStrUtilsUInt64(1000000000000000)..TPasDblStrUtilsUInt64(9999999999999999):begin
    result:=16;
   end;
   else begin
    Assert(aValue<TPasDblStrUtilsUInt64(100000000000000000));
    result:=17;
   end;
  end;
{$else}
  if aValue<TPasDblStrUtilsUInt64(10) then begin
   result:=1;
  end else if aValue<TPasDblStrUtilsUInt64(100) then begin
   result:=2;
  end else if aValue<TPasDblStrUtilsUInt64(1000) then begin
   result:=3;
  end else if aValue<TPasDblStrUtilsUInt64(10000) then begin
   result:=4;
  end else if aValue<TPasDblStrUtilsUInt64(100000) then begin
   result:=5;
  end else if aValue<TPasDblStrUtilsUInt64(1000000) then begin
   result:=6;
  end else if aValue<TPasDblStrUtilsUInt64(10000000) then begin
   result:=7;
  end else if aValue<TPasDblStrUtilsUInt64(100000000) then begin
   result:=8;
  end else if aValue<TPasDblStrUtilsUInt64(1000000000) then begin
   result:=9;
  end else if aValue<TPasDblStrUtilsUInt64(10000000000) then begin
   result:=10;
  end else if aValue<TPasDblStrUtilsUInt64(100000000000) then begin
   result:=11;
  end else if aValue<TPasDblStrUtilsUInt64(1000000000000) then begin
   result:=12;
  end else if aValue<TPasDblStrUtilsUInt64(10000000000000) then begin
   result:=13;
  end else if aValue<TPasDblStrUtilsUInt64(100000000000000) then begin
   result:=14;
  end else if aValue<TPasDblStrUtilsUInt64(1000000000000000) then begin
   result:=15;
  end else if aValue<TPasDblStrUtilsUInt64(10000000000000000) then begin
   result:=16;
  end else begin
   Assert(aValue<TPasDblStrUtilsUInt64(100000000000000000));
   result:=17;
  end;
{$endif}
 end;
 function DoubleToDecimal(const aIEEEMantissa:TPasDblStrUtilsUInt64;const aIEEEExponent:TPasDblStrUtilsUInt32):TFloatingDecimal64;
 var e2:TPasDblStrUtilsInt32;
     m2,mv,vr,vp,vm,Output,vpDiv10,vmDiv10,vrDiv10,vpDiv100,vmDiv100,vrDiv100:TPasDblStrUtilsUInt64;
     mmShift,q,mvMod5,vpMod10,vmMod10,vrMod10,vrMod100:TPasDblStrUtilsUInt32;
     e10,k,i,j,Removed:TPasDblStrUtilsInt32;
     LastRemovedDigit:TPasDblStrUtilsUInt8;
     Even,AcceptBounds,vmIsTrailingZeros,vrIsTrailingZeros,RoundUp:boolean;
 begin
  if aIEEEExponent=0 then begin
   e2:=1-(DOUBLE_BIAS+DOUBLE_MANTISSA_BITS+2);
   m2:=aIEEEMantissa;
  end else begin
   e2:=aIEEEExponent-(DOUBLE_BIAS+DOUBLE_MANTISSA_BITS+2);
   m2:=aIEEEMantissa or (TPasDblStrUtilsUInt64(1) shl DOUBLE_MANTISSA_BITS);
  end;
  Even:=(m2 and 1)=0;
  AcceptBounds:=Even;
  mv:=m2 shl 2;
  mmShift:=ord((aIEEEMantissa<>0) or (aIEEEExponent<=1)) and 1;
  vmIsTrailingZeros:=false;
  vrIsTrailingZeros:=false;
  if e2>=0 then begin
   q:=Log10Pow2(e2)-(ord(e2>3) and 1);
   e10:=q;
   k:=Pow5Bits(q)+(DOUBLE_POW5_INV_BITCOUNT-1);
   i:=(TPasDblStrUtilsInt32(q)+TPasDblStrUtilsInt32(k))-TPasDblStrUtilsInt32(e2);
   vr:=MulShiftAll64(m2,@DOUBLE_POW5_INV_SPLIT[q],i,vp,vm,mmShift);
   if q<=21 then begin
    mvMod5:=TPasDblStrUtilsUInt32(mv and $ffffffff)-TPasDblStrUtilsUInt32(5*(Div5(mv) and $ffffffff));
    if mvMod5=0 then begin
     vrIsTrailingZeros:=MultipleOfPowerOf5(mv,q);
    end else if AcceptBounds then begin
     vmIsTrailingZeros:=MultipleOfPowerOf5(mv-(1+mmShift),q);
    end else begin
     dec(vp,ord(MultipleOfPowerOf5(mv+2,q)) and 1);
    end;
   end;
  end else begin
   q:=Log10Pow5(-e2)-(ord((-e2)>1) and 1);
   e10:=q+e2;
   i:=(-e2)-TPasDblStrUtilsInt32(q);
   k:=Pow5bits(i)-DOUBLE_POW5_BITCOUNT;
   j:=TPasDblStrUtilsInt32(q)-k;
   vr:=MulShiftAll64(m2,@DOUBLE_POW5_SPLIT[i],j,vp,vm,mmShift);
   if q<=1 then begin
    vrIsTrailingZeros:=true;
    if AcceptBounds then begin
     vmIsTrailingZeros:=mmShift=1;
    end else begin
     dec(vp);
    end;
   end else if q<63 then begin
    vrIsTrailingZeros:=MultipleOfPowerOf2(mv,q);
   end;
  end;
  Removed:=0;
  LastRemovedDigit:=0;
  if vmIsTrailingZeros or vrIsTrailingZeros then begin
   repeat
    vpDiv10:=Div10(vp);
    vmDiv10:=Div10(vm);
    if vpDiv10<=vmDiv10 then begin
     break;
    end;
    vmMod10:=TPasDblStrUtilsUInt32(vm and $ffffffff)-(10*TPasDblStrUtilsUInt32(vmDiv10 and $ffffffff));
    vrDiv10:=Div10(vr);
    vrMod10:=TPasDblStrUtilsUInt32(vr and $ffffffff)-(10*TPasDblStrUtilsUInt32(vrDiv10 and $ffffffff));
    vmIsTrailingZeros:=vmIsTrailingZeros and (vmMod10=0);
    vrIsTrailingZeros:=vrIsTrailingZeros and (LastRemovedDigit=0);
    LastRemovedDigit:=TPasDblStrUtilsUInt8(vrMod10 and $ff);
    vr:=vrDiv10;
    vp:=vpDiv10;
    vm:=vmDiv10;
    inc(Removed);
   until false;
   if vmIsTrailingZeros then begin
    repeat
     vmDiv10:=Div10(vm);
     vmMod10:=TPasDblStrUtilsUInt32(vm and $ffffffff)-(10*TPasDblStrUtilsUInt32(vmDiv10 and $ffffffff));
     if vmMod10<>0 then begin
      break;
     end;
     vpDiv10:=Div10(vp);
     vrDiv10:=Div10(vr);
     vrMod10:=TPasDblStrUtilsUInt32(vr and $ffffffff)-(10*TPasDblStrUtilsUInt32(vrDiv10 and $ffffffff));
     vrIsTrailingZeros:=vrIsTrailingZeros and (LastRemovedDigit=0);
     LastRemovedDigit:=TPasDblStrUtilsUInt8(vrMod10 and $ff);
     vr:=vrDiv10;
     vp:=vpDiv10;
     vm:=vmDiv10;
     inc(Removed);
    until false;
   end;
   if vrIsTrailingZeros and (LastRemovedDigit=5) and ((vr and 1)=0) then begin
    LastRemovedDigit:=4;
   end;
   Output:=vr+TPasDblStrUtilsUInt64(ord(((vr=vm) and ((not acceptBounds) or not vmIsTrailingZeros)) or (LastRemovedDigit>=5)) and 1);
  end else begin
   RoundUp:=false;
   vpDiv100:=Div100(vp);
   vmDiv100:=Div100(vm);
   if vpDiv100>vmDiv100 then begin
    vrDiv100:=div100(vr);
    vrMod100:=TPasDblStrUtilsUInt32(vr and $ffffffff)-(100*TPasDblStrUtilsUInt32(vrDiv100 and $ffffffff));
    RoundUp:=vrMod100>=50;
    vr:=vrDiv100;
    vp:=vpDiv100;
    vm:=vmDiv100;
    inc(Removed,2);
   end;
   repeat
    vpDiv10:=Div10(vp);
    vmDiv10:=Div10(vm);
    if vpDiv10<=vmDiv10 then begin
     break;
    end;
    vrDiv10:=Div10(vr);
    vrMod10:=TPasDblStrUtilsUInt32(vr and $ffffffff)-(10*TPasDblStrUtilsUInt32(vrDiv10 and $ffffffff));
    RoundUp:=vrMod10>=5;
    vr:=vrDiv10;
    vp:=vpDiv10;
    vm:=vmDiv10;
    inc(Removed);
   until false;
   Output:=vr+TPasDblStrUtilsUInt64(ord((vr=vm) or RoundUp) and 1);
  end;
  result.Exponent:=e10+Removed;
  result.Mantissa:=Output;
 end;
 function DoubleToDecimalSmallInt(const aIEEEMantissa:TPasDblStrUtilsUInt64;const aIEEEExponent:TPasDblStrUtilsUInt32;out aResult:TFloatingDecimal64):boolean;
 var m2:TPasDblStrUtilsUInt64;
     e2:TPasDblStrUtilsInt32;
 begin
  m2:=(TPasDblStrUtilsUInt64(1) shl DOUBLE_MANTISSA_BITS) or aIEEEMantissa;
  e2:=aIEEEExponent-(DOUBLE_BIAS+DOUBLE_MANTISSA_BITS);
  if (e2>0) or (e2<-52) or ((m2 and ((TPasDblStrUtilsUInt64(1) shl (-e2))-1))<>0) then begin
   result:=false;
  end else begin
   aResult.Mantissa:=m2 shr (-e2);
   aResult.Exponent:=0;
   result:=true;
  end;
 end;
var FloatingDecimal64:TFloatingDecimal64;
    Bits,IEEEMantissa,q,Output:TPasDblStrUtilsUInt64;
    IEEEExponent,r:TPasDblStrUtilsUInt32;
    IEEESign:boolean;
    Len,OutputLen,Index,Anchor,Exponent,Position:TPasDblStrUtilsInt32;
    Digits:array[0..31] of AnsiChar;
begin
 Bits:=TPasDblStrUtilsUInt64(pointer(@aValue)^);
 IEEESign:=((Bits shr (DOUBLE_MANTISSA_BITS+DOUBLE_EXPONENT_BITS)) and 1)<>0;
 IEEEMantissa:=Bits and ((TPasDblStrUtilsUInt64(1) shl DOUBLE_MANTISSA_BITS)-1);
 IEEEExponent:=TPasDblStrUtilsUInt32((Bits shr DOUBLE_MANTISSA_BITS) and ((TPasDblStrUtilsUInt64(1) shl DOUBLE_EXPONENT_BITS)-1));
 if (IEEEExponent=((TPasDblStrUtilsUInt64(1) shl DOUBLE_EXPONENT_BITS)-1)) or ((IEEEExponent=0) and (IEEEMantissa=0)) then begin
  if IEEEMantissa<>0 then begin
   result:='NaN';
  end else if IEEEExponent<>0 then begin
   if IEEESign then begin
    result:='-Infinity';
   end else begin
    result:='Infinity';
   end;
  end else begin
   if aExponential then begin
    if IEEESign then begin
     result:='-0e0';
    end else begin
     result:='0e0';
    end;
   end else begin
    if IEEESign then begin
     result:='-0';
    end else begin
     result:='0';
    end;
   end;
  end;
 end else begin
  if DoubleToDecimalSmallInt(IEEEMantissa,IEEEExponent,FloatingDecimal64) then begin
   repeat
    q:=Div10(FloatingDecimal64.Mantissa);
    r:=TPasDblStrUtilsUInt32(FloatingDecimal64.Mantissa and $ffffffff)-(10*TPasDblStrUtilsUInt32(q and $ffffffff));
    if r<>0 then begin
     break;
    end;
    FloatingDecimal64.Mantissa:=q;
    inc(FloatingDecimal64.Exponent);
   until false;
  end else begin
   FloatingDecimal64:=DoubleToDecimal(IEEEMantissa,IEEEExponent);
  end;
  result:='';
  Len:=0;
  try
   SetLength(result,128);
   if IEEESign then begin
    inc(Len);
    result[Len]:='-';
   end;
   Output:=FloatingDecimal64.Mantissa;
   OutputLen:=DecimalLength17(Output);
   Exponent:=(FloatingDecimal64.Exponent+TPasDblStrUtilsInt32(OutputLen))-1;
   if aExponential or (abs(Exponent)>8) then begin
    if OutputLen>1 then begin
     Anchor:=Len+1;
     inc(Len,OutputLen+1);
     for Index:=0 to OutputLen-2 do begin
      result[(Anchor+OutputLen)-Index]:=AnsiChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(AnsiChar('0'))+(Output mod 10)));
      Output:=Output div 10;
     end;
     result[Anchor]:=AnsiChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(AnsiChar('0'))+(Output mod 10)));
     result[Anchor+1]:='.';
    end else begin
     inc(Len);
     result[Len]:=AnsiChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(AnsiChar('0'))+(Output mod 10)));
    end;
    inc(Len);
    result[Len]:='E';
    if Exponent<0 then begin
     inc(Len);
     result[Len]:='-';
     Exponent:=-Exponent;
    end;
    if Exponent=0 then begin
     inc(Len);
     result[Len]:='0';
    end else begin
     inc(Len,DecimalLength17(Exponent));
     Index:=Len;
     while Exponent>0 do begin
      result[Index]:=AnsiChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(AnsiChar('0'))+(Exponent mod 10)));
      dec(Index);
      Exponent:=Exponent div 10;
     end;
    end;
   end else begin
    if Exponent<0 then begin
     inc(Len);
     result[Len]:='0';
     inc(Len);
     result[Len]:='.';
     inc(Exponent);
     while Exponent<0 do begin
      inc(Len);
      result[Len]:='0';
      inc(Exponent);
     end;
     inc(Len,OutputLen);
     for Index:=0 to OutputLen-2 do begin
      result[Len-Index]:=AnsiChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(AnsiChar('0'))+(Output mod 10)));
      Output:=Output div 10;
     end;
     result[(Len-OutputLen)+1]:=AnsiChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(AnsiChar('0'))+(Output mod 10)));
    end else begin
     Anchor:=Len+1;
     Position:=OutputLen-1;
     for Index:=0 to OutputLen-1 do begin
      Digits[Position]:=AnsiChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(AnsiChar('0'))+(Output mod 10)));
      dec(Position);
      Output:=Output div 10;
     end;
     for Index:=0 to OutputLen-1 do begin
      inc(Len);
      result[Len]:=Digits[Index];
      if Exponent=Index then begin
       inc(Len);
       result[Len]:='.';
      end;
     end;
    end;
   end;
  finally
   SetLength(result,Len);
  end;
 end;
end;

const DoubleToStringPowerOfTenTable:array[0..86,0..2] of TPasDblStrUtilsInt64=((TPasDblStrUtilsInt64($fa8fd5a0081c0288),-1220,-348),
                                                                               (TPasDblStrUtilsInt64($baaee17fa23ebf76),-1193,-340),
                                                                               (TPasDblStrUtilsInt64($8b16fb203055ac76),-1166,-332),
                                                                               (TPasDblStrUtilsInt64($cf42894a5dce35ea),-1140,-324),
                                                                               (TPasDblStrUtilsInt64($9a6bb0aa55653b2d),-1113,-316),
                                                                               (TPasDblStrUtilsInt64($e61acf033d1a45df),-1087,-308),
                                                                               (TPasDblStrUtilsInt64($ab70fe17c79ac6ca),-1060,-300),
                                                                               (TPasDblStrUtilsInt64($ff77b1fcbebcdc4f),-1034,-292),
                                                                               (TPasDblStrUtilsInt64($be5691ef416bd60c),-1007,-284),
                                                                               (TPasDblStrUtilsInt64($8dd01fad907ffc3c),-980,-276),
                                                                               (TPasDblStrUtilsInt64($d3515c2831559a83),-954,-268),
                                                                               (TPasDblStrUtilsInt64($9d71ac8fada6c9b5),-927,-260),
                                                                               (TPasDblStrUtilsInt64($ea9c227723ee8bcb),-901,-252),
                                                                               (TPasDblStrUtilsInt64($aecc49914078536d),-874,-244),
                                                                               (TPasDblStrUtilsInt64($823c12795db6ce57),-847,-236),
                                                                               (TPasDblStrUtilsInt64($c21094364dfb5637),-821,-228),
                                                                               (TPasDblStrUtilsInt64($9096ea6f3848984f),-794,-220),
                                                                               (TPasDblStrUtilsInt64($d77485cb25823ac7),-768,-212),
                                                                               (TPasDblStrUtilsInt64($a086cfcd97bf97f4),-741,-204),
                                                                               (TPasDblStrUtilsInt64($ef340a98172aace5),-715,-196),
                                                                               (TPasDblStrUtilsInt64($b23867fb2a35b28e),-688,-188),
                                                                               (TPasDblStrUtilsInt64($84c8d4dfd2c63f3b),-661,-180),
                                                                               (TPasDblStrUtilsInt64($c5dd44271ad3cdba),-635,-172),
                                                                               (TPasDblStrUtilsInt64($936b9fcebb25c996),-608,-164),
                                                                               (TPasDblStrUtilsInt64($dbac6c247d62a584),-582,-156),
                                                                               (TPasDblStrUtilsInt64($a3ab66580d5fdaf6),-555,-148),
                                                                               (TPasDblStrUtilsInt64($f3e2f893dec3f126),-529,-140),
                                                                               (TPasDblStrUtilsInt64($b5b5ada8aaff80b8),-502,-132),
                                                                               (TPasDblStrUtilsInt64($87625f056c7c4a8b),-475,-124),
                                                                               (TPasDblStrUtilsInt64($c9bcff6034c13053),-449,-116),
                                                                               (TPasDblStrUtilsInt64($964e858c91ba2655),-422,-108),
                                                                               (TPasDblStrUtilsInt64($dff9772470297ebd),-396,-100),
                                                                               (TPasDblStrUtilsInt64($a6dfbd9fb8e5b88f),-369,-92),
                                                                               (TPasDblStrUtilsInt64($f8a95fcf88747d94),-343,-84),
                                                                               (TPasDblStrUtilsInt64($b94470938fa89bcf),-316,-76),
                                                                               (TPasDblStrUtilsInt64($8a08f0f8bf0f156b),-289,-68),
                                                                               (TPasDblStrUtilsInt64($cdb02555653131b6),-263,-60),
                                                                               (TPasDblStrUtilsInt64($993fe2c6d07b7fac),-236,-52),
                                                                               (TPasDblStrUtilsInt64($e45c10c42a2b3b06),-210,-44),
                                                                               (TPasDblStrUtilsInt64($aa242499697392d3),-183,-36),
                                                                               (TPasDblStrUtilsInt64($fd87b5f28300ca0e),-157,-28),
                                                                               (TPasDblStrUtilsInt64($bce5086492111aeb),-130,-20),
                                                                               (TPasDblStrUtilsInt64($8cbccc096f5088cc),-103,-12),
                                                                               (TPasDblStrUtilsInt64($d1b71758e219652c),-77,-4),
                                                                               (TPasDblStrUtilsInt64($9c40000000000000),-50,4),
                                                                               (TPasDblStrUtilsInt64($e8d4a51000000000),-24,12),
                                                                               (TPasDblStrUtilsInt64($ad78ebc5ac620000),3,20),
                                                                               (TPasDblStrUtilsInt64($813f3978f8940984),30,28),
                                                                               (TPasDblStrUtilsInt64($c097ce7bc90715b3),56,36),
                                                                               (TPasDblStrUtilsInt64($8f7e32ce7bea5c70),83,44),
                                                                               (TPasDblStrUtilsInt64($d5d238a4abe98068),109,52),
                                                                               (TPasDblStrUtilsInt64($9f4f2726179a2245),136,60),
                                                                               (TPasDblStrUtilsInt64($ed63a231d4c4fb27),162,68),
                                                                               (TPasDblStrUtilsInt64($b0de65388cc8ada8),189,76),
                                                                               (TPasDblStrUtilsInt64($83c7088e1aab65db),216,84),
                                                                               (TPasDblStrUtilsInt64($c45d1df942711d9a),242,92),
                                                                               (TPasDblStrUtilsInt64($924d692ca61be758),269,100),
                                                                               (TPasDblStrUtilsInt64($da01ee641a708dea),295,108),
                                                                               (TPasDblStrUtilsInt64($a26da3999aef774a),322,116),
                                                                               (TPasDblStrUtilsInt64($f209787bb47d6b85),348,124),
                                                                               (TPasDblStrUtilsInt64($b454e4a179dd1877),375,132),
                                                                               (TPasDblStrUtilsInt64($865b86925b9bc5c2),402,140),
                                                                               (TPasDblStrUtilsInt64($c83553c5c8965d3d),428,148),
                                                                               (TPasDblStrUtilsInt64($952ab45cfa97a0b3),455,156),
                                                                               (TPasDblStrUtilsInt64($de469fbd99a05fe3),481,164),
                                                                               (TPasDblStrUtilsInt64($a59bc234db398c25),508,172),
                                                                               (TPasDblStrUtilsInt64($f6c69a72a3989f5c),534,180),
                                                                               (TPasDblStrUtilsInt64($b7dcbf5354e9bece),561,188),
                                                                               (TPasDblStrUtilsInt64($88fcf317f22241e2),588,196),
                                                                               (TPasDblStrUtilsInt64($cc20ce9bd35c78a5),614,204),
                                                                               (TPasDblStrUtilsInt64($98165af37b2153df),641,212),
                                                                               (TPasDblStrUtilsInt64($e2a0b5dc971f303a),667,220),
                                                                               (TPasDblStrUtilsInt64($a8d9d1535ce3b396),694,228),
                                                                               (TPasDblStrUtilsInt64($fb9b7cd9a4a7443c),720,236),
                                                                               (TPasDblStrUtilsInt64($bb764c4ca7a44410),747,244),
                                                                               (TPasDblStrUtilsInt64($8bab8eefb6409c1a),774,252),
                                                                               (TPasDblStrUtilsInt64($d01fef10a657842c),800,260),
                                                                               (TPasDblStrUtilsInt64($9b10a4e5e9913129),827,268),
                                                                               (TPasDblStrUtilsInt64($e7109bfba19c0c9d),853,276),
                                                                               (TPasDblStrUtilsInt64($ac2820d9623bf429),880,284),
                                                                               (TPasDblStrUtilsInt64($80444b5e7aa7cf85),907,292),
                                                                               (TPasDblStrUtilsInt64($bf21e44003acdd2d),933,300),
                                                                               (TPasDblStrUtilsInt64($8e679c2f5e44ff8f),960,308),
                                                                               (TPasDblStrUtilsInt64($d433179d9c8cb841),986,316),
                                                                               (TPasDblStrUtilsInt64($9e19db92b4e31ba9),1013,324),
                                                                               (TPasDblStrUtilsInt64($eb96bf6ebadf77d9),1039,332),
                                                                               (TPasDblStrUtilsInt64($af87023b9bf0ee6b),1066,340));

      DoubleToStringPowerOfTenBinaryExponentTable:array[-1220..(1066+27)-1] of TPasDblStrUtilsUInt8=(0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                                                                     1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,
                                                                                                     2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                                                                                     2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,
                                                                                                     3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                                                                                     3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
                                                                                                     4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,
                                                                                                     5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
                                                                                                     5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,
                                                                                                     6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
                                                                                                     6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
                                                                                                     7,7,7,7,7,7,7,7,7,7,7,8,8,8,8,8,
                                                                                                     8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
                                                                                                     8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,9,
                                                                                                     9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
                                                                                                     9,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
                                                                                                     10,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,
                                                                                                     11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,
                                                                                                     11,11,11,11,11,11,12,12,12,12,12,12,12,12,12,12,
                                                                                                     12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,
                                                                                                     13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,
                                                                                                     13,13,13,13,13,13,13,13,13,13,13,14,14,14,14,14,
                                                                                                     14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,
                                                                                                     14,14,14,14,14,14,15,15,15,15,15,15,15,15,15,15,
                                                                                                     15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,
                                                                                                     16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,
                                                                                                     16,16,16,16,16,16,16,16,16,16,16,17,17,17,17,17,
                                                                                                     17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,
                                                                                                     17,17,17,17,17,18,18,18,18,18,18,18,18,18,18,18,
                                                                                                     18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,
                                                                                                     19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,
                                                                                                     19,19,19,19,19,19,19,19,19,19,20,20,20,20,20,20,
                                                                                                     20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,
                                                                                                     20,20,20,20,20,21,21,21,21,21,21,21,21,21,21,21,
                                                                                                     21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                                                                                                     22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,
                                                                                                     22,22,22,22,22,22,22,22,22,22,23,23,23,23,23,23,
                                                                                                     23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,
                                                                                                     23,23,23,23,23,24,24,24,24,24,24,24,24,24,24,24,
                                                                                                     24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,25,
                                                                                                     25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,
                                                                                                     25,25,25,25,25,25,25,25,25,25,26,26,26,26,26,26,
                                                                                                     26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,
                                                                                                     26,26,26,26,27,27,27,27,27,27,27,27,27,27,27,27,
                                                                                                     27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,28,
                                                                                                     28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,
                                                                                                     28,28,28,28,28,28,28,28,28,28,29,29,29,29,29,29,
                                                                                                     29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,
                                                                                                     29,29,29,29,30,30,30,30,30,30,30,30,30,30,30,30,
                                                                                                     30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,31,
                                                                                                     31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,
                                                                                                     31,31,31,31,31,31,31,31,31,32,32,32,32,32,32,32,
                                                                                                     32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,
                                                                                                     32,32,32,32,33,33,33,33,33,33,33,33,33,33,33,33,
                                                                                                     33,33,33,33,33,33,33,33,33,33,33,33,33,33,34,34,
                                                                                                     34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,
                                                                                                     34,34,34,34,34,34,34,34,34,35,35,35,35,35,35,35,
                                                                                                     35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,
                                                                                                     35,35,35,35,36,36,36,36,36,36,36,36,36,36,36,36,
                                                                                                     36,36,36,36,36,36,36,36,36,36,36,36,36,36,37,37,
                                                                                                     37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,
                                                                                                     37,37,37,37,37,37,37,37,37,38,38,38,38,38,38,38,
                                                                                                     38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,
                                                                                                     38,38,38,39,39,39,39,39,39,39,39,39,39,39,39,39,
                                                                                                     39,39,39,39,39,39,39,39,39,39,39,39,39,39,40,40,
                                                                                                     40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,
                                                                                                     40,40,40,40,40,40,40,40,41,41,41,41,41,41,41,41,
                                                                                                     41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,
                                                                                                     41,41,41,42,42,42,42,42,42,42,42,42,42,42,42,42,
                                                                                                     42,42,42,42,42,42,42,42,42,42,42,42,42,42,43,43,
                                                                                                     43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,
                                                                                                     43,43,43,43,43,43,43,43,44,44,44,44,44,44,44,44,
                                                                                                     44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,
                                                                                                     44,44,44,45,45,45,45,45,45,45,45,45,45,45,45,45,
                                                                                                     45,45,45,45,45,45,45,45,45,45,45,45,45,46,46,46,
                                                                                                     46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,
                                                                                                     46,46,46,46,46,46,46,46,47,47,47,47,47,47,47,47,
                                                                                                     47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,
                                                                                                     47,47,47,48,48,48,48,48,48,48,48,48,48,48,48,48,
                                                                                                     48,48,48,48,48,48,48,48,48,48,48,48,48,49,49,49,
                                                                                                     49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,
                                                                                                     49,49,49,49,49,49,49,49,50,50,50,50,50,50,50,50,
                                                                                                     50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,
                                                                                                     50,50,51,51,51,51,51,51,51,51,51,51,51,51,51,51,
                                                                                                     51,51,51,51,51,51,51,51,51,51,51,51,51,52,52,52,
                                                                                                     52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,
                                                                                                     52,52,52,52,52,52,52,53,53,53,53,53,53,53,53,53,
                                                                                                     53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,
                                                                                                     53,53,54,54,54,54,54,54,54,54,54,54,54,54,54,54,
                                                                                                     54,54,54,54,54,54,54,54,54,54,54,54,54,55,55,55,
                                                                                                     55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,
                                                                                                     55,55,55,55,55,55,55,56,56,56,56,56,56,56,56,56,
                                                                                                     56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,
                                                                                                     56,56,57,57,57,57,57,57,57,57,57,57,57,57,57,57,
                                                                                                     57,57,57,57,57,57,57,57,57,57,57,57,58,58,58,58,
                                                                                                     58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,
                                                                                                     58,58,58,58,58,58,58,59,59,59,59,59,59,59,59,59,
                                                                                                     59,59,59,59,59,59,59,59,59,59,59,59,59,59,59,59,
                                                                                                     59,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,
                                                                                                     60,60,60,60,60,60,60,60,60,60,60,60,61,61,61,61,
                                                                                                     61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,
                                                                                                     61,61,61,61,61,61,61,62,62,62,62,62,62,62,62,62,
                                                                                                     62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,
                                                                                                     62,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,
                                                                                                     63,63,63,63,63,63,63,63,63,63,63,63,64,64,64,64,
                                                                                                     64,64,64,64,64,64,64,64,64,64,64,64,64,64,64,64,
                                                                                                     64,64,64,64,64,64,65,65,65,65,65,65,65,65,65,65,
                                                                                                     65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,
                                                                                                     65,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,
                                                                                                     66,66,66,66,66,66,66,66,66,66,66,67,67,67,67,67,
                                                                                                     67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,
                                                                                                     67,67,67,67,67,67,68,68,68,68,68,68,68,68,68,68,
                                                                                                     68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,
                                                                                                     68,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,
                                                                                                     69,69,69,69,69,69,69,69,69,69,69,70,70,70,70,70,
                                                                                                     70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,
                                                                                                     70,70,70,70,70,70,71,71,71,71,71,71,71,71,71,71,
                                                                                                     71,71,71,71,71,71,71,71,71,71,71,71,71,71,71,71,
                                                                                                     72,72,72,72,72,72,72,72,72,72,72,72,72,72,72,72,
                                                                                                     72,72,72,72,72,72,72,72,72,72,72,73,73,73,73,73,
                                                                                                     73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,
                                                                                                     73,73,73,73,73,74,74,74,74,74,74,74,74,74,74,74,
                                                                                                     74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,
                                                                                                     75,75,75,75,75,75,75,75,75,75,75,75,75,75,75,75,
                                                                                                     75,75,75,75,75,75,75,75,75,75,75,76,76,76,76,76,
                                                                                                     76,76,76,76,76,76,76,76,76,76,76,76,76,76,76,76,
                                                                                                     76,76,76,76,76,77,77,77,77,77,77,77,77,77,77,77,
                                                                                                     77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,
                                                                                                     78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,
                                                                                                     78,78,78,78,78,78,78,78,78,78,79,79,79,79,79,79,
                                                                                                     79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,
                                                                                                     79,79,79,79,79,80,80,80,80,80,80,80,80,80,80,80,
                                                                                                     80,80,80,80,80,80,80,80,80,80,80,80,80,80,80,80,
                                                                                                     81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,
                                                                                                     81,81,81,81,81,81,81,81,81,81,82,82,82,82,82,82,
                                                                                                     82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,
                                                                                                     82,82,82,82,82,83,83,83,83,83,83,83,83,83,83,83,
                                                                                                     83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,84,
                                                                                                     84,84,84,84,84,84,84,84,84,84,84,84,84,84,84,84,
                                                                                                     84,84,84,84,84,84,84,84,84,84,85,85,85,85,85,85,
                                                                                                     85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,
                                                                                                     85,85,85,85,86,86,86,86,86,86,86,86,86,86,86,86,
                                                                                                     86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,
                                                                                                     86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,
                                                                                                     86,86,86,86,86,86,86,86,86);

      DoubleToStringPowerOfTenDecimalExponentTable:array[-348..(340+8)-1] of TPasDblStrUtilsUInt8=(0,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,
                                                                                                   2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,
                                                                                                   4,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,
                                                                                                   6,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,
                                                                                                   8,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,
                                                                                                   10,11,11,11,11,11,11,11,11,12,12,12,12,12,12,12,
                                                                                                   12,13,13,13,13,13,13,13,13,14,14,14,14,14,14,14,
                                                                                                   14,15,15,15,15,15,15,15,15,16,16,16,16,16,16,16,
                                                                                                   16,17,17,17,17,17,17,17,17,18,18,18,18,18,18,18,
                                                                                                   18,19,19,19,19,19,19,19,19,20,20,20,20,20,20,20,
                                                                                                   20,21,21,21,21,21,21,21,21,22,22,22,22,22,22,22,
                                                                                                   22,23,23,23,23,23,23,23,23,24,24,24,24,24,24,24,
                                                                                                   24,25,25,25,25,25,25,25,25,26,26,26,26,26,26,26,
                                                                                                   26,27,27,27,27,27,27,27,27,28,28,28,28,28,28,28,
                                                                                                   28,29,29,29,29,29,29,29,29,30,30,30,30,30,30,30,
                                                                                                   30,31,31,31,31,31,31,31,31,32,32,32,32,32,32,32,
                                                                                                   32,33,33,33,33,33,33,33,33,34,34,34,34,34,34,34,
                                                                                                   34,35,35,35,35,35,35,35,35,36,36,36,36,36,36,36,
                                                                                                   36,37,37,37,37,37,37,37,37,38,38,38,38,38,38,38,
                                                                                                   38,39,39,39,39,39,39,39,39,40,40,40,40,40,40,40,
                                                                                                   40,41,41,41,41,41,41,41,41,42,42,42,42,42,42,42,
                                                                                                   42,43,43,43,43,43,43,43,43,44,44,44,44,44,44,44,
                                                                                                   44,45,45,45,45,45,45,45,45,46,46,46,46,46,46,46,
                                                                                                   46,47,47,47,47,47,47,47,47,48,48,48,48,48,48,48,
                                                                                                   48,49,49,49,49,49,49,49,49,50,50,50,50,50,50,50,
                                                                                                   50,51,51,51,51,51,51,51,51,52,52,52,52,52,52,52,
                                                                                                   52,53,53,53,53,53,53,53,53,54,54,54,54,54,54,54,
                                                                                                   54,55,55,55,55,55,55,55,55,56,56,56,56,56,56,56,
                                                                                                   56,57,57,57,57,57,57,57,57,58,58,58,58,58,58,58,
                                                                                                   58,59,59,59,59,59,59,59,59,60,60,60,60,60,60,60,
                                                                                                   60,61,61,61,61,61,61,61,61,62,62,62,62,62,62,62,
                                                                                                   62,63,63,63,63,63,63,63,63,64,64,64,64,64,64,64,
                                                                                                   64,65,65,65,65,65,65,65,65,66,66,66,66,66,66,66,
                                                                                                   66,67,67,67,67,67,67,67,67,68,68,68,68,68,68,68,
                                                                                                   68,69,69,69,69,69,69,69,69,70,70,70,70,70,70,70,
                                                                                                   70,71,71,71,71,71,71,71,71,72,72,72,72,72,72,72,
                                                                                                   72,73,73,73,73,73,73,73,73,74,74,74,74,74,74,74,
                                                                                                   74,75,75,75,75,75,75,75,75,76,76,76,76,76,76,76,
                                                                                                   76,77,77,77,77,77,77,77,77,78,78,78,78,78,78,78,
                                                                                                   78,79,79,79,79,79,79,79,79,80,80,80,80,80,80,80,
                                                                                                   80,81,81,81,81,81,81,81,81,82,82,82,82,82,82,82,
                                                                                                   82,83,83,83,83,83,83,83,83,84,84,84,84,84,84,84,
                                                                                                   84,85,85,85,85,85,85,85,85,86,86,86,86,86,86,86,
                                                                                                   86,86,86,86,86,86,86,86);

      DoubleToStringEstimatePowerFactorTable:array[2..36] of TPasDblStrUtilsInt64=(4294967296, // round((ln(2)/ln(Radix))*4294967296.0);
                                                                                   2709822658,
                                                                                   2147483648,
                                                                                   1849741732,
                                                                                   1661520155,
                                                                                   1529898219,
                                                                                   1431655765,
                                                                                   1354911329,
                                                                                   1292913986,
                                                                                   1241523975,
                                                                                   1198050829,
                                                                                   1160664035,
                                                                                   1128071163,
                                                                                   1099331346,
                                                                                   1073741824,
                                                                                   1050766077,
                                                                                   1029986701,
                                                                                   1011073584,
                                                                                   993761859,
                                                                                   977836272,
                                                                                   963119891,
                                                                                   949465783,
                                                                                   936750801,
                                                                                   924870866,
                                                                                   913737342,
                                                                                   903274219,
                                                                                   893415894,
                                                                                   884105413,
                                                                                   875293062,
                                                                                   866935226,
                                                                                   858993459,
                                                                                   851433729,
                                                                                   844225782,
                                                                                   837342623,
                                                                                   830760078);

function ConvertStringToDouble(const StringValue:TPasDblStrUtilsString;const RoundingMode:TPasDblStrUtilsRoundingMode=rmNearest;const OK:PPasDblStrUtilsBoolean=nil;const Base:TPasDblStrUtilsInt32=-1):TPasDblStrUtilsDouble;
type PDoubleCasted=^TDoubleCasted;
     TDoubleCasted=packed record
      case TPasDblStrUtilsUInt8 of
       0:(Value:TPasDblStrUtilsDouble);
       1:({$ifdef BIG_ENDIAN}Hi,Lo{$else}Lo,Hi{$endif}:TPasDblStrUtilsUInt32);
       2:(Value64:TPasDblStrUtilsInt64);
     end;
const MantissaWords=12; //6; // 12
      MantissaDigits=52; //28; // 52
      WordTopBit=$8000;
      WordBits=16;
      WordBitShift=4;
      WordBitMask=WordBits-1;
      WordMask=$ffff;
      IEEEFormatBytes=8;
      IEEEFormatBits=IEEEFormatBytes shl 3;
      IEEEFormatExplicit=0;
      IEEEFormatExponent=11;
      IEEEFormatOneMask=WordTopBit shr ((IEEEFormatExponent+IEEEFormatExplicit) and WordBitMask);
      IEEEFormatOnePos=(IEEEFormatExponent+IEEEFormatExplicit) shr WordBitShift;
      IEEEFormatExpMax=1 shl (IEEEFormatExponent-1);
      Bit53=TPasDblStrUtilsInt64(TPasDblStrUtilsInt64(1) shl 53);
      InvBit53Mask=TPasDblStrUtilsInt64($ffe0000000000000);
      MaximumMultiplier=TPasDblStrUtilsUInt32(TPasDblStrUtilsUInt32($ffffffff) div 36);
      DtoAFPUExceptionMask:TFPUExceptionMask=[exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision];
      DtoAFPUPrecisionMode:TFPUPrecisionMode=pmDOUBLE;
      MaxFastPathDigits=16;
      TenPowers:array[0..18] of TPasDblStrUtilsInt64=
       (
        1,
        10,
        100,
        1000,
        10000,
        100000,
        1000000,
        10000000,
        100000000,
        1000000000,
        10000000000,
        100000000000,
        1000000000000,
        10000000000000,
        100000000000000,
        1000000000000000,
        10000000000000000,
        100000000000000000,
        1000000000000000000
       );
type PWords=^TWords;
     TWords=array[0..MantissaWords] of TPasDblStrUtilsUInt16;
     PTemp=^TTemp;
     TTemp=array[0..MantissaWords*2] of TPasDblStrUtilsUInt32;
     PDigits=^TDigits;
     TDigits=array[0..MantissaDigits] of TPasDblStrUtilsUInt8;
var MantissaPosition,Exponent,TenPower,TwoPower,ExtraTwos,Shift,i,DigitPos,StoredDigitPos,DigitPosBackwards,Digit,Overflow,OverflowBits,DroppedBits,DroppedBitsMask,MiddleValue,ExponentPower,ExponentValue:TPasDblStrUtilsInt32;
    Bit,Carry:TPasDblStrUtilsUInt16;
    Negative,ExponentNegative,HasDigits,Started,ZeroTail,Done,TemporaryOK:TPasDblStrUtilsBoolean;
    ResultCasted:PDoubleCasted;
    Temp:PTemp;
    Digits:PDigits;
    MantissaMultiplicator,Mantissa:PWords;
    Value:TPasDblStrUtilsInt64;
    c:TPasDblStrUtilsChar;
    Part,Multiplier,NextMultiplier:TPasDblStrUtilsUInt32;
    OldFPUExceptionMask:TFPUExceptionMask;
    OldFPURoundingMode,NewFPURoundingMode:TFPURoundingMode;
    OldFPUPrecisionMode:TFPUPrecisionMode;
 function MantissaMultiply(vTo,vFrom:PWords):TPasDblStrUtilsInt32;
 var i,j,k:TPasDblStrUtilsInt32;
     v:TPasDblStrUtilsUInt32;
     t:PTemp;
 begin
  t:=Temp;
  FillChar(t^,sizeof(TTemp),#0);
  for i:=0 to MantissaWords-1 do begin
   for j:=0 to MantissaWords-1 do begin
    v:=TPasDblStrUtilsUInt32(vTo^[i]+0)*TPasDblStrUtilsUInt32(vFrom^[j]+0);
    k:=i+j;
    inc(t^[k],v shr WordBits);
    inc(t^[k+1],v and WordMask);
   end;
  end;
  for i:=high(TTemp) downto 1 do begin
   inc(t^[i-1],t^[i] shr WordBits);
   t^[i]:=t^[i] and WordMask;
  end;
  if (t^[0] and WordTopBit)<>0 then begin
   for i:=0 to MantissaWords-1 do begin
    vTo^[i]:=t^[i] and WordMask;
   end;
   result:=0;
  end else begin
   for i:=0 to MantissaWords-1 do begin
    vTo^[i]:=(t^[i] shl 1)+TPasDblStrUtilsUInt16(ord((t^[i+1] and WordTopBit)<>0));
   end;
   result:=-1;
  end;
 end;
 procedure MantissaShiftRight(var Mantissa:TWords;Shift:TPasDblStrUtilsInt32);
 var Bits,Words,InvBits,Position:TPasDblStrUtilsInt32;
     Carry,Current:TPasDblStrUtilsUInt32;
 begin
  Bits:=Shift and WordBitMask;
  Words:=Shift shr WordBitShift;
  InvBits:=WordBits-Bits;
  Position:=high(TWords);
  if Bits=0 then begin
   if Words<>0 then begin
    while Position>=Words do begin
     Mantissa[Position]:=Mantissa[Position-Words];
     dec(Position);
    end;
   end;
  end else begin
   if (high(TWords)-Words)>=0 then begin
    Carry:=Mantissa[high(TWords)-Words] shr Bits;
   end else begin
    Carry:=0;
   end;
   while Position>Words do begin
    Current:=Mantissa[Position-(Words+1)];
    Mantissa[Position]:=(Current shl InvBits) or Carry;
    Carry:=Current shr Bits;
    dec(Position);
   end;
   Mantissa[Position]:=Carry;
   dec(Position);
  end;
  while Position>=0 do begin
   Mantissa[Position]:=0;
   dec(Position);
  end;
 end;
 procedure MantissaSetBit(var Mantissa:TWords;i:TPasDblStrUtilsInt32); {$ifdef CanInline}inline;{$endif}
 begin
  Mantissa[i shr WordBitShift]:=Mantissa[i shr WordBitShift] or (WordTopBit shr (i and WordBitMask));
 end;
 function MantissaTestBit(var Mantissa:TWords;i:TPasDblStrUtilsInt32):TPasDblStrUtilsBoolean; {$ifdef CanInline}inline;{$endif}
 begin
  result:=(Mantissa[i shr WordBitShift] shr ((not i) and WordBitMask))<>0;
 end;
 function MantissaIsZero(var Mantissa:TWords):TPasDblStrUtilsBoolean;
 var i:TPasDblStrUtilsInt32;
 begin
  result:=true;
  for i:=low(TWords) to High(TWords) do begin
   if Mantissa[i]<>0 then begin
    result:=false;
    break;
   end;
  end;
 end;
 function MantissaRound(Negative:TPasDblStrUtilsBoolean;var Mantissa:TWords;BitPos:TPasDblStrUtilsInt32):TPasDblStrUtilsBoolean;
 var i,p:TPasDblStrUtilsInt32;
     Bit:TPasDblStrUtilsUInt32;
  function RoundAbsDown:TPasDblStrUtilsBoolean;
  var j:TPasDblStrUtilsInt32;
  begin
   Mantissa[i]:=Mantissa[i] and not (Bit-1);
   for j:=i+1 to high(TWords) do begin
    Mantissa[j]:=0;
   end;
   result:=false;
  end;
  function RoundAbsUp:TPasDblStrUtilsBoolean;
  var j:TPasDblStrUtilsInt32;
  begin
   Mantissa[i]:=(Mantissa[i] and not (Bit-1))+Bit;
   for j:=i+1 to high(TWords) do begin
    Mantissa[j]:=0;
   end;
   while (i>0) and (Mantissa[i]=0) do begin
    dec(i);
    inc(Mantissa[i]);
   end;
   result:=Mantissa[0]=0;
  end;
  function RoundTowardsInfinity:TPasDblStrUtilsBoolean;
  var j:TPasDblStrUtilsInt32;
      m:TPasDblStrUtilsUInt32;
  begin
   m:=Mantissa[i] and ((Bit shl 1)-1);
   for j:=i+1 to high(TWords) do begin
    m:=m or Mantissa[j];
   end;
   if m<>0 then begin
    result:=RoundAbsUp;
   end else begin
    result:=RoundAbsDown;
   end;
  end;
  function RoundNear:TPasDblStrUtilsBoolean;
  var j:TPasDblStrUtilsInt32;
      m:TPasDblStrUtilsUInt32;
  begin
   if (Mantissa[i] and Bit)<>0 then begin
    Mantissa[i]:=Mantissa[i] and not Bit;
    m:=Mantissa[i] and ((Bit shl 1)-1);
    for j:=i+1 to high(TWords) do begin
     m:=m or Mantissa[j];
    end;
    Mantissa[i]:=Mantissa[i] or Bit;
    if m<>0 then begin
     result:=RoundAbsUp;
    end else begin
     if MantissaTestBit(Mantissa,BitPos-1) then begin
      result:=RoundAbsUp;
     end else begin
      result:=RoundAbsDown;
     end;
    end;
   end else begin
    result:=RoundAbsDown;
   end;
  end;
 begin
  i:=BitPos shr WordBitShift;
  p:=BitPos and WordBitMask;
  Bit:=WordTopBit shr p;
  case RoundingMode of
   rmNearest:begin
    result:=RoundNear;
   end;
   rmTruncate:begin
    result:=RoundAbsDown;
   end;
   rmUp:begin
    if Negative then begin
     result:=RoundAbsDown;
    end else begin
     result:=RoundTowardsInfinity;
    end;
   end;
   rmDown:begin
    if Negative then begin
     result:=RoundTowardsInfinity;
    end else begin
     result:=RoundAbsDown;
    end;
   end;
   else begin
    result:=false;
   end;
  end;
 end;
 function CountLeadingZeros32(a:TPasDblStrUtilsUInt32):TPasDblStrUtilsInt32;
 const CountLeadingZerosHigh:array[TPasDblStrUtilsUInt8] of TPasDblStrUtilsUInt8=(8,7,6,6,5,5,5,5,4,4,4,4,4,4,4,4,
                                                  3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                                  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                                  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
 begin
  result:=0;
  if a<$10000 then begin
   inc(result,16);
   a:=a shl 16;
  end;
  if a<$1000000 then begin
   inc(result,8);
   a:=a shl 8;
  end;
  inc(result,CountLeadingZerosHigh[a shr 24]);
 end;
 function CountLeadingZeros64(a:TPasDblStrUtilsInt64):TPasDblStrUtilsInt32;
 begin
 if a<TPasDblStrUtilsInt64($100000000) then begin
   result:=32;
  end else begin
   result:=0;
   a:=a shr 32;
  end;
  inc(result,CountLeadingZeros32(a));
 end;
begin
 if assigned(OK) then begin
  OK^:=false;
 end;
 ResultCasted:=pointer(@result);
 ResultCasted^.Hi:=$7ff80000;
 ResultCasted^.Lo:=$00000000;
 i:=1;
 while (i<=length(StringValue)) and (StringValue[i] in [#0..#32]) do begin
  inc(i);
 end;
 if (i<=length(StringValue)) and ((StringValue[i]='-') or (StringValue[i]='+')) then begin
  Negative:=StringValue[i]='-';
  inc(i);
 end else begin
  Negative:=false;
 end;
 HasDigits:=false;
 if ((i+7)<=length(StringValue)) and ((StringValue[i]='I') and (StringValue[i+1]='n') and (StringValue[i+2]='f') and (StringValue[i+3]='i') and (StringValue[i+4]='n') and (StringValue[i+5]='i') and (StringValue[i+6]='t') and (StringValue[i+7]='y')) then begin
  if Negative then begin
   ResultCasted^.Hi:=$fff00000;
   ResultCasted^.Lo:=$00000000;
  end else begin
   ResultCasted^.Hi:=$7ff00000;
   ResultCasted^.Lo:=$00000000;
  end;
  if assigned(OK) then begin
   OK^:=true;
  end;
 end else if ((i+2)<=length(StringValue)) and ((StringValue[i]='N') and (StringValue[i+1]='a') and (StringValue[i+2]='N')) then begin
  ResultCasted^.Hi:=$7ff80000;
  ResultCasted^.Lo:=$00000000;
  if assigned(OK) then begin
   OK^:=true;
  end;
 end else if (Base in [2,4,8,16,32]) or ((not (Base in [2..36])) and ((((i+1)<=length(StringValue)) and ((StringValue[i]='0') and (StringValue[i+1] in ['b','o','x']))))) then begin
  ResultCasted^.Hi:=$00000000;
  ResultCasted^.Lo:=$00000000;
  case Base of
   2:begin
    Shift:=1;
   end;
   4:begin
    Shift:=2;
   end;
   8:begin
    Shift:=3;
   end;
   16:begin
    Shift:=4;
   end;
   32:begin
    Shift:=5;
   end;
   else begin
    inc(i);
    case StringValue[i] of
     'b':begin
      Shift:=1;
     end;
     'o':begin
      Shift:=3;
     end;
     else {'x':}begin
      Shift:=4;
     end;
    end;
    inc(i);
   end;
  end;
  TwoPower:=1 shl Shift;
  Value:=0;
  Exponent:=0;
  while (i<=length(StringValue)) and (StringValue[i]='0') do begin
   HasDigits:=true;
   inc(i);
  end;
  if i<=length(StringValue) then begin
   //q:=0;
   ExponentPower:=1;
   Digit:=0;
   while i<=length(StringValue) do begin
    c:=StringValue[i];
    if c='.' then begin
     if ExponentPower>0 then begin
      ExponentPower:=0;
      inc(i);
      if i>length(StringValue) then begin
       Done:=true;
       if Done then begin
       end;
       break;
      end;
      continue;
     end else begin
      break;
     end;
    end else if (c in ['0'..'9']) and (ord(c)<=(ord('0')+TwoPower)) then begin
     Digit:=ord(c)-ord('0');
    end else if (TwoPower>10) and ((c in ['a'..'z']) and (ord(c)<=((ord('a')+TwoPower)-10))) then begin
     Digit:=(ord(c)-ord('a'))+10;
    end else if (TwoPower>10) and ((c in ['A'..'Z']) and (ord(c)<=((ord('A')+TwoPower)-10))) then begin
     Digit:=(ord(c)-ord('A'))+10;
    end else begin
     break;
    end;
    inc(i);
    HasDigits:=true;
    if ExponentPower<=0 then begin
     dec(ExponentPower);
    end;
    Value:=(Value shl Shift) or Digit;
    Overflow:=TPasDblStrUtilsInt32(TPasDblStrUtilsInt64(Value shr 53));
    if Overflow<>0 then begin
     OverflowBits:=1;
     while Overflow>1 do begin
      inc(OverflowBits);
      Overflow:=Overflow shr 1;
     end;
     DroppedBitsMask:=(1 shl OverflowBits)-1;
     DroppedBits:=Value and DroppedBitsMask;
     Value:=Value shr OverflowBits;
     Exponent:=OverflowBits;
     ZeroTail:=true;
     while i<=length(StringValue) do begin
      c:=StringValue[i];
      if (c in ['0'..'9']) and (ord(c)<=(ord('0')+TwoPower)) then begin
       Digit:=ord(c)-ord('0');
      end else if (TwoPower>10) and ((c in ['a'..'z']) and (ord(c)<=((ord('a')+TwoPower)-10))) then begin
       Digit:=(ord(c)-ord('a'))+10;
      end else if (TwoPower>10) and ((c in ['A'..'Z']) and (ord(c)<=((ord('A')+TwoPower)-10))) then begin
       Digit:=(ord(c)-ord('A'))+10;
      end else begin
       break;
      end;
      inc(i);
      if Digit<>0 then begin
       ZeroTail:=false;
      end;
      inc(Exponent,Shift);
     end;
     MiddleValue:=1 shl (OverflowBits-1);
     if DroppedBits>MiddleValue then begin
      inc(Value);
     end else if DroppedBits=MiddleValue then begin
      if ((Value and 1)<>0) or not ZeroTail then begin
       inc(Value);
      end;
     end;
     while (Value and Bit53)<>0 do begin
      Value:=Value shr 1;
      inc(Exponent);
     end;
     break;
    end;
   end;
   if ExponentPower>0 then begin
    ExponentPower:=0;
   end;
   ExponentValue:=0;
   ExponentNegative:=false;
   if (i<=length(StringValue)) and (StringValue[i] in ['e','E','p','P']) then begin
    inc(i);
    if (i<=length(StringValue)) and (StringValue[i] in ['+','-']) then begin
     ExponentNegative:=StringValue[i]='-';
     inc(i);
    end;
    HasDigits:=false;
    while (i<=length(StringValue)) and (StringValue[i] in ['0'..'9']) do begin
     ExponentValue:=(ExponentValue*10)+TPasDblStrUtilsInt32(ord(StringValue[i])-ord('0'));
     HasDigits:=true;
     inc(i);
    end;
   end;
   if ExponentNegative then begin
    dec(ExponentPower,ExponentValue);
   end else begin
    inc(ExponentPower,ExponentValue);
   end;
   inc(Exponent,Shift*ExponentPower);
   Shift:=CountLeadingZeros64(Value);
   ExponentValue:=$432-((Shift-IEEEFormatExponent)-Exponent);
   if (((ExponentValue>$34) and (ExponentValue<$7fe)) and (Exponent<IEEEFormatExponent)) and (Value<Bit53) then begin
    dec(Shift,IEEEFormatExponent);
    if Shift>=0 then begin
     Value:=Value shl Shift;
    end else begin
     Value:=Value shr (-Shift);
    end;
    ResultCasted^.Value64:=((TPasDblStrUtilsInt64(ExponentValue) shl 52)+Value) and TPasDblStrUtilsInt64($7fffffffffffffff);
   end else begin
    New(Mantissa);
    try
     FillChar(Mantissa^,sizeof(TWords),#0);
     Value:=Value shl Shift;
     inc(Exponent,64-Shift);
     Mantissa^[0]:=(Value shr 48) and $ffff;
     Mantissa^[1]:=(Value shr 32) and $ffff;
     Mantissa^[2]:=(Value shr 16) and $ffff;
     Mantissa^[3]:=(Value shr 0) and $ffff;
     if (Mantissa^[0] and WordTopBit)<>0 then begin
      dec(Exponent);
      if (Exponent>=(2-IEEEFormatExpMax)) and (Exponent<=IEEEFormatExpMax) then begin
       inc(Exponent,IEEEFormatExpMax-1);
       MantissaShiftRight(Mantissa^,IEEEFormatExponent+IEEEFormatExplicit);
       MantissaRound(Negative,Mantissa^,IEEEFormatBits);
       if MantissaTestBit(Mantissa^,IEEEFormatExponent+IEEEFormatExplicit-1) then begin
        MantissaShiftRight(Mantissa^,1);
        inc(Exponent);
       end;
       if Exponent>=(IEEEFormatExpMax shl 1)-1 then begin
        ResultCasted^.Hi:=$7ff00000;
        ResultCasted^.Lo:=$00000000;
       end else begin
        ResultCasted^.Hi:=(((Exponent shl 4) or (Mantissa^[0] and $f)) shl 16) or Mantissa^[1];
        ResultCasted^.Lo:=(Mantissa^[2] shl 16) or Mantissa^[3];
       end;
      end else if Exponent>0 then begin
       ResultCasted^.Hi:=$7ff00000;
       ResultCasted^.Lo:=$00000000;
      end else begin
       Shift:=IEEEFormatExplicit-(Exponent+(IEEEFormatExpMax-(2+IEEEFormatExponent)));
       MantissaShiftRight(Mantissa^,Shift);
       MantissaRound(Negative,Mantissa^,IEEEFormatBits);
       if (Mantissa^[IEEEFormatOnePos] and IEEEFormatOneMask)<>0 then begin
        Exponent:=1;
        if IEEEFormatExplicit=0 then begin
         Mantissa^[IEEEFormatOnePos]:=Mantissa^[IEEEFormatOnePos] and not IEEEFormatOneMask;
        end;
        Mantissa^[0]:=Mantissa^[0] or (Exponent shl (WordBitMask-IEEEFormatExponent));
        ResultCasted^.Hi:=(((Exponent shl 4) or (Mantissa^[0] and $f)) shl 16) or Mantissa^[1];
        ResultCasted^.Lo:=(Mantissa^[2] shl 16) or Mantissa^[3];
       end else begin
        if MantissaIsZero(Mantissa^) then begin
         ResultCasted^.Hi:=$00000000;
         ResultCasted^.Lo:=$00000000;
        end else begin
         ResultCasted^.Hi:=(Mantissa^[0] shl 16) or Mantissa^[1];
         ResultCasted^.Lo:=(Mantissa^[2] shl 16) or Mantissa^[3];
        end;
       end;
      end;
     end else begin
      ResultCasted^.Hi:=$00000000;
      ResultCasted^.Lo:=$00000000;
     end;
    finally
     Dispose(Mantissa);
    end;
   end;
  end else begin
   ResultCasted^.Hi:=$00000000;
   ResultCasted^.Lo:=$00000000;
  end;
  if Negative then begin
   ResultCasted^.Hi:=ResultCasted^.Hi or $80000000;
  end;
  if HasDigits then begin
   if assigned(OK) then begin
    OK^:=true;
   end;
  end;
 end else if Base in [2..9,11..36] then begin
  ResultCasted^.Hi:=$00000000;
  ResultCasted^.Lo:=$00000000;
  while (i<=length(StringValue)) and (StringValue[i]='0') do begin
   HasDigits:=true;
   inc(i);
  end;
  if i<=length(StringValue) then begin
   case RoundingMode of
    rmNearest:begin
     NewFPURoundingMode:=rmNearest;
    end;
    rmTruncate:begin
     NewFPURoundingMode:=rmTruncate;
    end;
    rmUp:begin
     NewFPURoundingMode:=rmUp;
    end;
    rmDown:begin
     NewFPURoundingMode:=rmDown;
    end;
    else begin
     NewFPURoundingMode:=rmNearest;
    end;
   end;
   OldFPUExceptionMask:=GetExceptionMask;
   OldFPUPrecisionMode:=GetPrecisionMode;
   OldFPURoundingMode:=GetRoundMode;
   try
    if OldFPUExceptionMask<>DtoAFPUExceptionMask then begin
     SetExceptionMask(DtoAFPUExceptionMask);
    end;
    if OldFPUPrecisionMode<>DtoAFPUPrecisionMode then begin
     SetPrecisionMode(DtoAFPUPrecisionMode);
    end;
    if OldFPURoundingMode<>NewFPURoundingMode then begin
     SetRoundMode(NewFPURoundingMode);
    end;
    Part:=0;
    Multiplier:=1;
    Digit:=0;
    Done:=false;
    ExponentPower:=1;
    while not Done do begin
     while true do begin
      c:=StringValue[i];
      if c='.' then begin
       if ExponentPower>0 then begin
        ExponentPower:=0;
        inc(i);
        if i>length(StringValue) then begin
         Done:=true;
         break;
        end;
        continue;
       end else begin
        Done:=true;
        break;
       end;
      end else if ((c>='0') and (c<='9')) and (ord(c)<=(ord('0')+Base)) then begin
       Digit:=ord(c)-ord('0');
      end else if (Base>10) and (((c>='a') and (c<='z')) and (ord(c)<=((ord('a')+Base)-10))) then begin
       Digit:=(ord(c)-ord('a'))+10;
      end else if (Base>10) and (((c>='A') and (c<='Z')) and (ord(c)<=((ord('A')+Base)-10))) then begin
       Digit:=(ord(c)-ord('A'))+10;
      end else begin
       Done:=true;
       break;
      end;
      HasDigits:=true;
      NextMultiplier:=Multiplier*TPasDblStrUtilsUInt32(Base);
      if NextMultiplier>MaximumMultiplier then begin
       break;
      end;
      if ExponentPower<=0 then begin
       dec(ExponentPower);
      end;
      Part:=(Part*TPasDblStrUtilsUInt32(Base))+TPasDblStrUtilsUInt32(Digit);
      Multiplier:=NextMultiplier;
      Assert(Multiplier>Part);
      inc(i);
      if i>length(StringValue) then begin
       Done:=true;
       break;
      end;
     end;
     ResultCasted^.Value:=(ResultCasted^.Value*Multiplier)+Part;
    end;
    if ExponentPower>0 then begin
     ExponentPower:=0;
    end;
    ExponentValue:=0;
    ExponentNegative:=false;
    if (i<=length(StringValue)) and (StringValue[i] in ['e','E','p','P']) then begin
     inc(i);
     if (i<=length(StringValue)) and (StringValue[i] in ['+','-']) then begin
      ExponentNegative:=StringValue[i]='-';
      inc(i);
     end;
     HasDigits:=false;
     while (i<=length(StringValue)) and (StringValue[i] in ['0'..'9']) do begin
      ExponentValue:=(ExponentValue*10)+TPasDblStrUtilsInt32(ord(StringValue[i])-ord('0'));
      HasDigits:=true;
      inc(i);
     end;
    end;
    if ExponentNegative then begin
     dec(ExponentPower,ExponentValue);
    end else begin
     inc(ExponentPower,ExponentValue);
    end;
    if ExponentPower<>0 then begin
     ResultCasted^.Value:=ResultCasted^.Value*power(Base,ExponentPower);
    end;
   finally
    if OldFPUExceptionMask<>DtoAFPUExceptionMask then begin
     SetExceptionMask(OldFPUExceptionMask);
    end;
    if OldFPUPrecisionMode<>DtoAFPUPrecisionMode then begin
     SetPrecisionMode(OldFPUPrecisionMode);
    end;
    if OldFPURoundingMode<>NewFPURoundingMode then begin
     SetRoundMode(OldFPURoundingMode);
    end;
   end;
  end else begin
   ResultCasted^.Hi:=$00000000;
   ResultCasted^.Lo:=$00000000;
  end;
  if Negative then begin
   ResultCasted^.Hi:=ResultCasted^.Hi or $80000000;
  end;
  if HasDigits then begin
   if assigned(OK) then begin
    OK^:=true;
   end;
  end;
 end else begin
  if RoundingMode=rmNearest then begin
   TemporaryOK:=false;
   result:=RyuStringToDouble(StringValue,@TemporaryOK,true);
   if TemporaryOK then begin
    if assigned(OK) then begin
     OK^:=true;
    end;
    exit;
   end;
  end;
  HasDigits:=false;
  Value:=0;
  StoredDigitPos:=i;
  DigitPos:=0;
  TenPower:=1;
  while (i<=length(StringValue)) and (StringValue[i]='0') do begin
   HasDigits:=true;
   inc(i);
  end;
  while i<=length(StringValue) do begin
   c:=StringValue[i];
   case c of
    '0'..'9':begin
     HasDigits:=true;
     Value:=(Value*10)+(ord(c)-ord('0'));
     inc(DigitPos);
     if (Value and InvBit53Mask)<>0 then begin
      HasDigits:=false;
      break;
     end;
     if TenPower<=0 then begin
      dec(TenPower);
      if TenPower>high(TenPowers) then begin
       HasDigits:=false;
       break;
      end;
     end;
    end;
    '.':begin
     if TenPower<=0 then begin
      HasDigits:=false;
      break;
     end else begin
      TenPower:=0;
     end;
    end;
    'e','E':begin
     break;
    end;
    else begin
     HasDigits:=false;
     break;
    end;
   end;
   inc(i);
  end;
  if HasDigits then begin
   if TenPower>0 then begin
    TenPower:=0;
   end;
   ExponentValue:=0;
   ExponentNegative:=false;
   if (i<=length(StringValue)) and (StringValue[i] in ['e','E']) then begin
    inc(i);
    if (i<=length(StringValue)) and (StringValue[i] in ['+','-']) then begin
     ExponentNegative:=StringValue[i]='-';
     inc(i);
    end;
    HasDigits:=false;
    while (i<=length(StringValue)) and (StringValue[i] in ['0'..'9']) do begin
     ExponentValue:=(ExponentValue*10)+TPasDblStrUtilsInt32(ord(StringValue[i])-ord('0'));
     HasDigits:=true;
     inc(i);
    end;
   end;
   if Value=0 then begin
    TenPower:=0;
   end else begin
    if ExponentNegative then begin
     dec(TenPower,ExponentValue);
    end else begin
     inc(TenPower,ExponentValue);
    end;
    if TenPower<>0 then begin
     if TenPower>0 then begin
      if ((DigitPos+TenPower)>MaxFastPathDigits) or (TenPower>high(TenPowers)) then begin
       HasDigits:=false;
      end else begin
       Value:=Value*TenPowers[TenPower];
       TenPower:=0;
       if (Value and InvBit53Mask)<>0 then begin
        HasDigits:=false;
       end;
      end;
     end else begin
      if (-TenPower)>high(TenPowers) then begin
       HasDigits:=false;
      end else begin
       i:=-TenPower;
       while (i>0) and (TenPower<0) do begin
        if (Value mod TenPowers[i])=0 then begin
         Value:=Value div TenPowers[i];
         inc(TenPower,i);
        end else begin
         if (i and 1)=0 then begin
          i:=i shr 1;
         end else begin
          dec(i);
         end;
        end;
       end;
      end;
     end;
    end;
   end;
  end;
  if HasDigits then begin
   if Value=0 then begin
    ResultCasted^.Hi:=$00000000;
    ResultCasted^.Lo:=$00000000;
   end else begin
    Shift:=CountLeadingZeros64(Value)-11;
    if Shift>=0 then begin
     Value:=Value shl Shift;
    end else begin
     Value:=Value shr (-Shift);
    end;
    ResultCasted^.Value64:=((TPasDblStrUtilsInt64($432-Shift) shl 52)+Value) and TPasDblStrUtilsInt64($7fffffffffffffff);
   end;
   if TenPower<>0 then begin
    case RoundingMode of
     rmNearest:begin
      NewFPURoundingMode:=rmNearest;
     end;
     rmTruncate:begin
      NewFPURoundingMode:=rmTruncate;
     end;
     rmUp:begin
      NewFPURoundingMode:=rmUp;
     end;
     rmDown:begin
      NewFPURoundingMode:=rmDown;
     end;
     else begin
      NewFPURoundingMode:=rmNearest;
     end;
    end;
    OldFPUExceptionMask:=GetExceptionMask;
    OldFPUPrecisionMode:=GetPrecisionMode;
    OldFPURoundingMode:=GetRoundMode;
    try
     if OldFPUExceptionMask<>DtoAFPUExceptionMask then begin
      SetExceptionMask(DtoAFPUExceptionMask);
     end;
     if OldFPUPrecisionMode<>DtoAFPUPrecisionMode then begin
      SetPrecisionMode(DtoAFPUPrecisionMode);
     end;
     if OldFPURoundingMode<>NewFPURoundingMode then begin
      SetRoundMode(NewFPURoundingMode);
     end;
     if TenPower>0 then begin
      while TenPower>high(TenPowers) do begin
       ResultCasted^.Value:=ResultCasted^.Value*TenPowers[high(TenPowers)];
       dec(TenPower,high(TenPowers));
      end;
      if TenPower>0 then begin
       ResultCasted^.Value:=ResultCasted^.Value*TenPowers[TenPower];
      end;
     end else begin
      TenPower:=-TenPower;
      while TenPower>high(TenPowers) do begin
       ResultCasted^.Value:=ResultCasted^.Value/TenPowers[high(TenPowers)];
       dec(TenPower,high(TenPowers));
      end;
      if TenPower>0 then begin
       ResultCasted^.Value:=ResultCasted^.Value/TenPowers[TenPower];
      end;
     end;
    finally
     if OldFPUExceptionMask<>DtoAFPUExceptionMask then begin
      SetExceptionMask(OldFPUExceptionMask);
     end;
     if OldFPUPrecisionMode<>DtoAFPUPrecisionMode then begin
      SetPrecisionMode(OldFPUPrecisionMode);
     end;
     if OldFPURoundingMode<>NewFPURoundingMode then begin
      SetRoundMode(OldFPURoundingMode);
     end;
    end;
   end;
   if Negative then begin
    ResultCasted^.Hi:=ResultCasted^.Hi or $80000000;
   end;
   if assigned(OK) then begin
    OK^:=true;
   end;
  end else begin
   i:=StoredDigitPos;
   GetMem(MantissaMultiplicator,SizeOf(TWords));
   GetMem(Mantissa,SizeOf(TWords));
   GetMem(Temp,SizeOf(TTemp));
   GetMem(Digits,SizeOf(TDigits));
   try
    FillChar(Digits^,SizeOf(TDigits),#0);

    DigitPos:=0;
    TenPower:=0;
    HasDigits:=false;
    Started:=false;
    ExponentNegative:=false;
    ExponentValue:=0;
    while (i<=length(StringValue)) and (StringValue[i]='0') do begin
     HasDigits:=true;
     inc(i);
    end;
    while (i<=length(StringValue)) and (StringValue[i] in ['0'..'9']) do begin
     HasDigits:=true;
     Started:=true;
     if DigitPos<=high(TDigits) then begin
      Digits^[DigitPos]:=ord(StringValue[i])-ord('0');
      inc(DigitPos);
     end;
     inc(TenPower);
     inc(i);
    end;
    if (i<=length(StringValue)) and (StringValue[i]='.') then begin
     inc(i);
     if not Started then begin
      while (i<=length(StringValue)) and (StringValue[i]='0') do begin
       HasDigits:=true;
       dec(TenPower);
       inc(i);
      end;
     end;
     while (i<=length(StringValue)) and (StringValue[i] in ['0'..'9']) do begin
      HasDigits:=true;
      if DigitPos<=high(TDigits) then begin
       Digits^[DigitPos]:=ord(StringValue[i])-ord('0');
       inc(DigitPos);
      end;
      inc(i);
     end;
    end;
    if HasDigits then begin
     if (i<=length(StringValue)) and (StringValue[i] in ['e','E']) then begin
      inc(i);
      if (i<=length(StringValue)) and (StringValue[i] in ['+','-']) then begin
       ExponentNegative:=StringValue[i]='-';
       inc(i);
      end;
      HasDigits:=false;
      while (i<=length(StringValue)) and (StringValue[i] in ['0'..'9']) do begin
       ExponentValue:=(ExponentValue*10)+TPasDblStrUtilsInt32(ord(StringValue[i])-ord('0'));
       HasDigits:=true;
       inc(i);
      end;
     end;
     if HasDigits then begin
      if ExponentNegative then begin
       dec(TenPower,ExponentValue);
      end else begin
       inc(TenPower,ExponentValue);
      end;

      if TenPower<=-324 then begin
       if Negative then begin
        ResultCasted^.Hi:=$80000000;
        ResultCasted^.Lo:=$00000000;
       end else begin
        ResultCasted^.Hi:=$00000000;
        ResultCasted^.Lo:=$00000000;
       end;
       if assigned(OK) then begin
        OK^:=true;
       end;
       exit;
      end else if TenPower>=310 then begin
       if Value=0 then begin
        if Negative then begin
         ResultCasted^.Hi:=$80000000;
         ResultCasted^.Lo:=$00000000;
        end else begin
         ResultCasted^.Hi:=$00000000;
         ResultCasted^.Lo:=$00000000;
        end;
       end else begin
        if Negative then begin
         ResultCasted^.Hi:=$fff00000;
         ResultCasted^.Lo:=$00000000;
        end else begin
         ResultCasted^.Hi:=$7ff00000;
         ResultCasted^.Lo:=$00000000;
        end;
       end;
       if assigned(OK) then begin
        OK^:=true;
       end;
       exit;
      end;

      FillChar(Mantissa^,sizeof(TWords),#0);

      Bit:=WordTopBit;
      StoredDigitPos:=0;
      Started:=false;
      TwoPower:=0;
      MantissaPosition:=0;
      while MantissaPosition<MantissaWords do begin
       Carry:=0;
       while (DigitPos>StoredDigitPos) and (Digits^[DigitPos-1]=0) do begin
        dec(DigitPos);
       end;
       if DigitPos<=StoredDigitPos then begin
        break;
       end;
       DigitPosBackwards:=DigitPos;
       while DigitPosBackwards>StoredDigitPos do begin
        dec(DigitPosBackwards);
        i:=(2*Digits^[DigitPosBackwards])+Carry;
        if i>=10 then begin
         dec(i,10);
         Carry:=1;
        end else begin
         Carry:=0;
        end;
        Digits^[DigitPosBackwards]:=i;
       end;
       if Carry<>0 then begin
        Mantissa^[MantissaPosition]:=Mantissa^[MantissaPosition] or Bit;
        Started:=true;
       end;
       if Started then begin
        if Bit=1 then begin
         Bit:=WordTopBit;
         inc(MantissaPosition);
        end else begin
         Bit:=Bit shr 1;
        end;
       end else begin
        dec(TwoPower);
       end;
      end;
      inc(TwoPower,TenPower);

      if TenPower<0 then begin
       for i:=0 to high(TWords)-1 do begin
        MantissaMultiplicator^[i]:=$cccc;
       end;
       MantissaMultiplicator^[high(TWords)]:=$cccd;
       ExtraTwos:=-2;
       TenPower:=-TenPower;
      end else if TenPower>0 then begin
       MantissaMultiplicator^[0]:=$a000;
       for i:=1 to high(TWords) do begin
        MantissaMultiplicator^[i]:=$0000;
       end;
       ExtraTwos:=3;
      end else begin
       ExtraTwos:=0;
      end;
      while TenPower<>0 do begin
       if (TenPower and 1)<>0 then begin
        inc(TwoPower,ExtraTwos+MantissaMultiply(Mantissa,MantissaMultiplicator));
       end;
       inc(ExtraTwos,ExtraTwos+MantissaMultiply(MantissaMultiplicator,MantissaMultiplicator));
       TenPower:=TenPower shr 1;
      end;

      Exponent:=TwoPower;
      if (Mantissa^[0] and WordTopBit)<>0 then begin
       dec(Exponent);

       if (Exponent>=(2-IEEEFormatExpMax)) and (Exponent<=IEEEFormatExpMax) then begin
        inc(Exponent,IEEEFormatExpMax-1);
        MantissaShiftRight(Mantissa^,IEEEFormatExponent+IEEEFormatExplicit);
        MantissaRound(Negative,Mantissa^,IEEEFormatBits);
        if MantissaTestBit(Mantissa^,IEEEFormatExponent+IEEEFormatExplicit-1) then begin
         MantissaShiftRight(Mantissa^,1);
         inc(Exponent);
        end;
        if Exponent>=(IEEEFormatExpMax shl 1)-1 then begin
         ResultCasted^.Hi:=$7ff00000;
         ResultCasted^.Lo:=$00000000;
        end else begin
         ResultCasted^.Hi:=(((Exponent shl 4) or (Mantissa^[0] and $f)) shl 16) or Mantissa^[1];
         ResultCasted^.Lo:=(Mantissa^[2] shl 16) or Mantissa^[3];
        end;
       end else if Exponent>0 then begin
        ResultCasted^.Hi:=$7ff00000;
        ResultCasted^.Lo:=$00000000;
       end else begin
        Shift:=IEEEFormatExplicit-(Exponent+(IEEEFormatExpMax-(2+IEEEFormatExponent)));
        MantissaShiftRight(Mantissa^,Shift);
        MantissaRound(Negative,Mantissa^,IEEEFormatBits);
        if (Mantissa^[IEEEFormatOnePos] and IEEEFormatOneMask)<>0 then begin
         Exponent:=1;
         if IEEEFormatExplicit=0 then begin
          Mantissa^[IEEEFormatOnePos]:=Mantissa^[IEEEFormatOnePos] and not IEEEFormatOneMask;
         end;
         Mantissa^[0]:=Mantissa^[0] or (Exponent shl (WordBitMask-IEEEFormatExponent));
         ResultCasted^.Hi:=(((Exponent shl 4) or (Mantissa^[0] and $f)) shl 16) or Mantissa^[1];
         ResultCasted^.Lo:=(Mantissa^[2] shl 16) or Mantissa^[3];
        end else begin
         if MantissaIsZero(Mantissa^) then begin
          ResultCasted^.Hi:=$00000000;
          ResultCasted^.Lo:=$00000000;
         end else begin
          ResultCasted^.Hi:=(Mantissa^[0] shl 16) or Mantissa^[1];
          ResultCasted^.Lo:=(Mantissa^[2] shl 16) or Mantissa^[3];
         end;
        end;
       end;
      end else begin
       ResultCasted^.Hi:=$00000000;
       ResultCasted^.Lo:=$00000000;
      end;
      if Negative then begin
       ResultCasted^.Hi:=ResultCasted^.Hi or $80000000;
      end;
      if assigned(OK) then begin
       OK^:=true;
      end;
     end;
    end;
   finally
    FreeMem(MantissaMultiplicator);
    FreeMem(Mantissa);
    FreeMem(Temp);
    FreeMem(Digits);
   end;
  end;
 end;
end;

function ConvertDoubleToString(const AValue:TPasDblStrUtilsDouble;const OutputMode:TPasDblStrUtilsOutputMode=omStandard;RequestedDigits:TPasDblStrUtilsInt32=-1):TPasDblStrUtilsString;
const SignificantMantissaSize=64;
      MinimalTargetExponent=-60;
      MaximalTargetExponent=-32;
      ModeShortest=0;
      ModeFixed=1;
      ModePrecision=2;
      BigNumMaxSignificantMantissaBits=3584;
      BigitChunkSize=32;
      BigitDoubleChunkSize=64;
      BigitSize=28;
      BigitMask=(1 shl BigitSize)-1;
      BigNumCapacity=(BigNumMaxSignificantMantissaBits+(BigitSize-1)) div BigitSize;
type TDoubleValue=record
      SignificantMantissa:TPasDblStrUtilsUInt64;
      Exponent:TPasDblStrUtilsInt32;
     end;
     TBigNumChunk=TPasDblStrUtilsUInt32;
     TBigNumDoubleChunk=TPasDblStrUtilsUInt64;
     TBigNum=record
      Bigits:array[0..BigNumCapacity] of TBigNumChunk;
      UsedDigits:TPasDblStrUtilsInt32;
      Exponent:TPasDblStrUtilsInt32;
     end;
 function QWordLessOrEqual(a,b:TPasDblStrUtilsUInt64):TPasDblStrUtilsBoolean;
 begin
  result:=(a=b) or (((a shr 32)<(b shr 32)) or (((a shr 32)=(b shr 32)) and ((a and $ffffffff)<(b and $ffffffff))));
 end;
 function QWordGreaterOrEqual(a,b:TPasDblStrUtilsUInt64):TPasDblStrUtilsBoolean;
 begin
  result:=(a=b) or (((a shr 32)>(b shr 32)) or (((a shr 32)=(b shr 32)) and ((a and $ffffffff)>(b and $ffffffff))));
 end;
 function QWordLess(a,b:TPasDblStrUtilsUInt64):TPasDblStrUtilsBoolean;
 begin
  result:=((a shr 32)<(b shr 32)) or (((a shr 32)=(b shr 32)) and ((a and $ffffffff)<(b and $ffffffff)));
 end;
 function QWordGreater(a,b:TPasDblStrUtilsUInt64):TPasDblStrUtilsBoolean;
 begin
  result:=((a shr 32)>(b shr 32)) or (((a shr 32)=(b shr 32)) and ((a and $ffffffff)>(b and $ffffffff)));
 end;
 function DoubleValue(SignificantMantissa:TPasDblStrUtilsUInt64=0;Exponent:TPasDblStrUtilsInt32=0):TDoubleValue;
 begin
  result.SignificantMantissa:=SignificantMantissa;
  result.Exponent:=Exponent;
 end;
 procedure SplitDouble(Value:TPasDblStrUtilsDouble;var SignificantMantissa:TPasDblStrUtilsUInt64;var Exponent:TPasDblStrUtilsInt32);
 var Casted:TPasDblStrUtilsUInt64 absolute Value;
 begin
  SignificantMantissa:=Casted and TPasDblStrUtilsUInt64($000fffffffffffff);
  if (Casted and TPasDblStrUtilsUInt64($7ff0000000000000))<>0 then begin
   inc(SignificantMantissa,TPasDblStrUtilsUInt64($0010000000000000));
   Exponent:=((Casted and TPasDblStrUtilsUInt64($7ff0000000000000)) shr 52)-($3ff+52);
  end else begin
   Exponent:=(-($3ff+52))+1;
  end;
 end;
 function DoubleValueGet(Value:TPasDblStrUtilsDouble):TDoubleValue;
 var SignificantMantissa:TPasDblStrUtilsUInt64;
     Exponent:TPasDblStrUtilsInt32;
 begin
  Assert(Value>0);
  SplitDouble(Value,SignificantMantissa,Exponent);
  while (SignificantMantissa and TPasDblStrUtilsUInt64($0010000000000000))=0 do begin
   SignificantMantissa:=SignificantMantissa shl 1;
   dec(Exponent);
  end;
  SignificantMantissa:=SignificantMantissa shl (SignificantMantissaSize-53);
  dec(Exponent,SignificantMantissaSize-53);
  result.SignificantMantissa:=SignificantMantissa;
  result.Exponent:=Exponent;
 end;
 procedure DoubleValueSubtract(var Left:TDoubleValue;const Right:TDoubleValue);
 begin
  Assert(Left.Exponent=Right.Exponent);
  Assert(QWordGreaterOrEqual(Left.SignificantMantissa,Right.SignificantMantissa));
  dec(Left.SignificantMantissa,Right.SignificantMantissa);
 end;
 function DoubleValueMinus(const Left,Right:TDoubleValue):TDoubleValue;
 begin
  Assert(Left.Exponent=Right.Exponent);
  Assert(QWordGreaterOrEqual(Left.SignificantMantissa,Right.SignificantMantissa));
  result.Exponent:=Left.Exponent;
  result.SignificantMantissa:=Left.SignificantMantissa-Right.SignificantMantissa;
 end;
 procedure DoubleValueMuliply(var Left:TDoubleValue;const Right:TDoubleValue);
 var a,b,c,d,ac,bc,ad,bd:TPasDblStrUtilsUInt64;
 begin
  a:=Left.SignificantMantissa shr 32;
  b:=Left.SignificantMantissa and $ffffffff;
  c:=Right.SignificantMantissa shr 32;
  d:=Right.SignificantMantissa and $ffffffff;
  ac:=a*c;
  bc:=b*c;
  ad:=a*d;
  bd:=b*d;
  inc(Left.Exponent,Right.Exponent+64);
  Left.SignificantMantissa:=ac+(ad shr 32)+(bc shr 32)+(TPasDblStrUtilsUInt64(((bd shr 32)+((ad and $ffffffff)+(bc and $ffffffff)))+(TPasDblStrUtilsUInt64(1) shl 31)) shr 32);
 end;
 function DoubleValueMul(const Left,Right:TDoubleValue):TDoubleValue;
 var a,b,c,d,ac,bc,ad,bd:TPasDblStrUtilsUInt64;
 begin
  a:=Left.SignificantMantissa shr 32;
  b:=Left.SignificantMantissa and $ffffffff;
  c:=Right.SignificantMantissa shr 32;
  d:=Right.SignificantMantissa and $ffffffff;
  ac:=a*c;
  bc:=b*c;
  ad:=a*d;
  bd:=b*d;
  result.Exponent:=Left.Exponent+(Right.Exponent+64);
  a:=((bd shr 32)+((ad and $ffffffff)+(bc and $ffffffff)))+(TPasDblStrUtilsUInt64(1) shl 31);
  result.SignificantMantissa:=ac+(ad shr 32)+(bc shr 32)+(a shr 32);
 end;
 procedure DoubleValueNormalize(var Value:TDoubleValue);
 var SignificantMantissa:TPasDblStrUtilsUInt64;
     Exponent:TPasDblStrUtilsInt32;
 begin
  Assert(Value.SignificantMantissa<>0);
  SignificantMantissa:=Value.SignificantMantissa;
  Exponent:=Value.Exponent;
  while (SignificantMantissa and TPasDblStrUtilsUInt64($ffc0000000000000))=0 do begin
   SignificantMantissa:=SignificantMantissa shl 10;
   dec(Exponent,10);
  end;
  while (SignificantMantissa and TPasDblStrUtilsUInt64($8000000000000000))=0 do begin
   SignificantMantissa:=SignificantMantissa shl 1;
   dec(Exponent);
  end;
  Value.SignificantMantissa:=SignificantMantissa;
  Value.Exponent:=Exponent;
 end;
 function DoubleValueNorm(const Value:TDoubleValue):TDoubleValue;
 var SignificantMantissa:TPasDblStrUtilsUInt64;
     Exponent:TPasDblStrUtilsInt32;
 begin
  Assert(Value.SignificantMantissa<>0);
  SignificantMantissa:=Value.SignificantMantissa;
  Exponent:=Value.Exponent;
  while (SignificantMantissa and TPasDblStrUtilsUInt64($ffc0000000000000))=0 do begin
   SignificantMantissa:=SignificantMantissa shl 10;
   dec(Exponent,10);
  end;
  while (SignificantMantissa and TPasDblStrUtilsUInt64($8000000000000000))=0 do begin
   SignificantMantissa:=SignificantMantissa shl 1;
   dec(Exponent);
  end;
  result.SignificantMantissa:=SignificantMantissa;
  result.Exponent:=Exponent;
 end;
 function BigNumNew:TBigNum;
 begin
  FillChar(result,sizeof(TBigNum),#0);
 end;
 procedure BigNumZero(var BigNum:TBigNum);
 begin
  BigNum.UsedDigits:=0;
  BigNum.Exponent:=0;
 end;
 procedure BigNumEnsureCapacity(var BigNum:TBigNum;Size:TPasDblStrUtilsInt32);
 begin
 end;
 procedure BigNumClamp(var BigNum:TBigNum);
 begin
  while (BigNum.UsedDigits>0) and (BigNum.Bigits[BigNum.UsedDigits-1]=0) do begin
   dec(BigNum.UsedDigits);
  end;
  if BigNum.UsedDigits=0 then begin
   BigNum.Exponent:=0;
  end;
 end;
 function BigNumIsClamped(const BigNum:TBigNum):TPasDblStrUtilsBoolean;
 begin
  result:=(BigNum.UsedDigits=0) or (BigNum.Bigits[BigNum.UsedDigits-1]<>0);
 end;
 procedure BigNumAlign(var BigNum:TBigNum;const Other:TBigNum);
 var ZeroDigits,i:TPasDblStrUtilsInt32;
 begin
  if BigNum.Exponent>Other.Exponent then begin
   ZeroDigits:=BigNum.Exponent-Other.Exponent;
   BigNumEnsureCapacity(BigNum,Bignum.UsedDigits+ZeroDigits);
   for i:=BigNum.UsedDigits-1 downto 0 do begin
    BigNum.Bigits[i+ZeroDigits]:=BigNum.Bigits[i];
   end;
   for i:=0 to ZeroDigits-1 do begin
    BigNum.Bigits[i]:=0;
   end;
   inc(BigNum.UsedDigits,ZeroDigits);
   dec(BigNum.Exponent,ZeroDigits);
   Assert(BigNum.UsedDigits>=0);
   Assert(BigNum.Exponent>=0);
  end;
 end;
 procedure BigNumAssignUInt16(var BigNum:TBigNum;Value:TPasDblStrUtilsUInt16);
 begin
  Assert(BigitSize>=(sizeof(TPasDblStrUtilsUInt16)*8));
  BigNumZero(BigNum);
  if Value<>0 then begin
   BigNumEnsureCapacity(BigNum,1);
   BigNum.Bigits[0]:=Value;
   BigNum.UsedDigits:=1;
  end;
 end;
 procedure BigNumAssignUInt64(var BigNum:TBigNum;Value:TPasDblStrUtilsUInt64);
 var i,j:TPasDblStrUtilsInt32;
 begin
  BigNumZero(BigNum);
  if Value<>0 then begin
   j:=(64 div BigitSize)+1;
   BigNumEnsureCapacity(BigNum,j);
   for i:=0 to j-1 do begin
    BigNum.Bigits[i]:=Value and BigitMask;
    Value:=Value shr BigitSize;
   end;
   BigNum.UsedDigits:=j;
   BigNumClamp(BigNum);
  end;
 end;
 procedure BigNumAssignBigNum(var BigNum:TBigNum;const Other:TBigNum);
 begin
  BigNum.Exponent:=Other.Exponent;
  BigNum.Bigits:=Other.Bigits;
  BigNum.UsedDigits:=Other.UsedDigits;
 end;
 procedure BigNumAddBigNum(var BigNum:TBigNum;const Other:TBigNum);
 var Carry,Sum:TBigNumChunk;
     BigitPos,i:TPasDblStrUtilsInt32;
 begin
  Assert(BigNumIsClamped(BigNum));
  Assert(BigNumIsClamped(Other));
  BigNumAlign(BigNum,Other);
  BigNumEnsureCapacity(BigNum,BigNum.UsedDigits+Other.UsedDigits);
  BigitPos:=Other.Exponent-BigNum.Exponent;
  Assert(BigitPos>=0);
  Carry:=0;
  for i:=0 to Other.UsedDigits-1 do begin
   Sum:=BigNum.Bigits[BigitPos]+Other.Bigits[i]+Carry;
   BigNum.Bigits[BigitPos]:=Sum and BigitMask;
   Carry:=Sum shr BigitSize;
   inc(BigitPos);
  end;
  while Carry<>0 do begin
   Sum:=BigNum.Bigits[BigitPos]+Carry;
   BigNum.Bigits[BigitPos]:=Sum and BigitMask;
   Carry:=Sum shr BigitSize;
   inc(BigitPos);
  end;
  if BigNum.UsedDigits<BigitPos then begin
   BigNum.UsedDigits:=BigitPos;
  end;
  Assert(BigNumIsClamped(BigNum));
 end;
 procedure BigNumAddUInt64(var BigNum:TBigNum;const Value:TPasDblStrUtilsUInt64);
 var Other:TBigNum;
 begin
  Other:=BigNumNew;
  BigNumAssignUInt64(Other,Value);
  BigNumAddBigNum(BigNum,Other);
 end;
 function BigNumBigitAt(const BigNum:TBigNum;Index:TPasDblStrUtilsInt32):TBigNumChunk;
 begin
  if (Index<BigNum.Exponent) or (Index>=(BigNum.UsedDigits+BigNum.Exponent)) then begin
   result:=0;
  end else begin
   result:=BigNum.Bigits[Index-BigNum.Exponent];
  end;
 end;
 function BigNumCompare(const a,b:TBigNum):TPasDblStrUtilsInt32;
 var la,lb,i,j:TPasDblStrUtilsInt32;
     ba,bb:TBigNumChunk;
 begin
  Assert(BigNumIsClamped(a));
  Assert(BigNumIsClamped(b));
  la:=a.UsedDigits+a.Exponent;
  lb:=b.UsedDigits+b.Exponent;
  if la<lb then begin
   result:=-1;
  end else if la>lb then begin
   result:=1;
  end else begin
   if a.Exponent<b.Exponent then begin
    j:=a.Exponent;
   end else begin
    j:=b.Exponent;
   end;
   result:=0;
   for i:=la-1 downto j do begin
    ba:=BigNumBigItAt(a,i);
    bb:=BigNumBigItAt(b,i);
    if ba<bb then begin
     result:=-1;
     break;
    end else if ba>bb then begin
     result:=1;
     break;
    end;
   end;
  end;
 end;
 function BigNumPlusCompare(const a,b,c:TBigNum):TPasDblStrUtilsInt32;
 var la,lb,lc,i,j:TPasDblStrUtilsInt32;
     ba,bb,bc,br,Sum:TBigNumChunk;
 begin
  Assert(BigNumIsClamped(a));
  Assert(BigNumIsClamped(b));
  Assert(BigNumIsClamped(c));
  la:=a.UsedDigits+a.Exponent;
  lb:=b.UsedDigits+b.Exponent;
  lc:=c.UsedDigits+c.Exponent;
  if la<lb then begin
   result:=BigNumPlusCompare(b,a,c);
  end else begin
   if (la+1)<lc then begin
    result:=-1;
   end else if la>lc then begin
    result:=1;
   end else if (a.Exponent>=lb) and (la<lc) then begin
    result:=-1;
   end else begin
    if a.Exponent<b.Exponent then begin
     if a.Exponent<c.Exponent then begin
      j:=a.Exponent;
     end else begin
      j:=c.Exponent;
     end;
    end else begin
     if b.Exponent<c.Exponent then begin
      j:=b.Exponent;
     end else begin
      j:=c.Exponent;
     end;
    end;
    br:=0;
    for i:=lc-1 downto j do begin
     ba:=BigNumBigItAt(a,i);
     bb:=BigNumBigItAt(b,i);
     bc:=BigNumBigItAt(c,i);
     Sum:=ba+bb;
     if Sum>(bc+br) then begin
      result:=1;
      exit;
     end else begin
      br:=(bc+br)-Sum;
      if br>1 then begin
       result:=-1;
       exit;
      end;
      br:=br shl BigitSize;
     end;
    end;
    if br=0 then begin
     result:=0;
    end else begin
     result:=-1;
    end;
   end;
  end;
 end;
 procedure BigNumSubtractBigNum(var BigNum:TBigNum;const Other:TBigNum);
 var Borrow,Difference:TBigNumChunk;
     i,Offset:TPasDblStrUtilsInt32;
 begin
  Assert(BigNumIsClamped(BigNum));
  Assert(BigNumIsClamped(Other));
  Assert(BigNumCompare(Other,BigNum)<=0);
  BigNumAlign(BigNum,Other);
  Offset:=Other.Exponent-BigNum.Exponent;
  Borrow:=0;
  for i:=0 to Other.UsedDigits-1 do begin
   Assert((Borrow=0) or (Borrow=1));
   Difference:=(BigNum.Bigits[i+Offset]-Other.Bigits[i])-Borrow;
   BigNum.Bigits[i+Offset]:=Difference and BigitMask;
   Borrow:=Difference shr (BigitChunkSize-1);
  end;
  i:=Other.UsedDigits;
  while Borrow<>0 do begin
   Difference:=BigNum.Bigits[i+Offset]-Borrow;
   BigNum.Bigits[i+Offset]:=Difference and BigitMask;
   Borrow:=Difference shr (BigitChunkSize-1);
   inc(i);
  end;
  BigNumClamp(BigNum);
 end;
 procedure BigNumBigitsShiftLeft(var BigNum:TBigNum;Shift:TPasDblStrUtilsInt32);
 var Carry,NextCarry:TBigNumChunk;
     i:TPasDblStrUtilsInt32;
 begin
  Assert(Shift<BigitSize);
  Assert(Shift>=0);
  Carry:=0;
  for i:=0 to BigNum.UsedDigits-1 do begin
   NextCarry:=BigNum.Bigits[i] shr (BigitSize-Shift);
   BigNum.Bigits[i]:=((BigNum.Bigits[i] shl Shift)+Carry) and BigitMask;
   Carry:=NextCarry;
  end;
  if Carry<>0 then begin
   BigNum.Bigits[BigNum.UsedDigits]:=Carry;
   inc(BigNum.UsedDigits);
  end;
 end;
 procedure BigNumBigitsShiftRight(var BigNum:TBigNum;Shift:TPasDblStrUtilsInt32);
 var Carry,NextCarry:TBigNumChunk;
     i:TPasDblStrUtilsInt32;
 begin
  Assert(Shift<BigitSize);
  Assert(Shift>=0);
  if BigNum.UsedDigits>0 then begin
   Carry:=0;
   for i:=BigNum.UsedDigits-1 downto 1 do begin
    NextCarry:=BigNum.Bigits[i] shl (BigitSize-Shift);
    BigNum.Bigits[i]:=((BigNum.Bigits[i] shr Shift)+Carry) and BigitMask;
    Carry:=NextCarry;
   end;
   BigNum.Bigits[0]:=(BigNum.Bigits[0] shr Shift)+Carry;
  end;
  BigNumClamp(BigNum);
 end;
 procedure BignumSubtractTimes(var BigNum:TBigNum;const Other:TBigNum;Factor:TPasDblStrUtilsInt32);
 var i,ExponentDiff:TPasDblStrUtilsInt32;
     Borrow,Difference:TBigNumChunk;
     Product,Remove:TBigNumDoubleChunk;
 begin
  Assert(BigNum.Exponent<=Other.Exponent);
  if Factor<3 then begin
   for i:=1 to Factor do begin
    BigNumSubtractBignum(BigNum,Other);
   end;
  end else begin
   Borrow:=0;
   ExponentDiff:=Other.Exponent-BigNum.Exponent;
   for i:=0 to Other.UsedDigits-1 do begin
    Product:=TBigNumDoubleChunk(Factor)*Other.Bigits[i];
    Remove:=Borrow+Product;
    Difference:=BigNum.Bigits[i+ExponentDiff]-TBigNumChunk(Remove and BigitMask);
    BigNum.Bigits[i+ExponentDiff]:=Difference and BigitMask;
    Borrow:=TBigNumChunk((Difference shr (BigitChunkSize-1))+(Remove shr BigitSize));
   end;
   for i:=Other.UsedDigits+ExponentDiff to BigNum.UsedDigits-1 do begin
    if Borrow=0 then begin
     exit;
    end;
    Difference:=BigNum.Bigits[i]-Borrow;
    BigNum.Bigits[i]:=Difference and BigitMask;
    Borrow:=TBigNumChunk(Difference shr (BigitChunkSize-1));
   end;
   BigNumClamp(BigNum);
  end;
 end;
 procedure BigNumShiftLeft(var BigNum:TBigNum;Shift:TPasDblStrUtilsInt32);
 begin
  if BigNum.UsedDigits<>0 then begin
   inc(BigNum.Exponent,Shift div BigitSize);
   BignumEnsureCapacity(BigNum,BigNum.UsedDigits+1);
   BigNumBigitsShiftLeft(BigNum,Shift mod BigitSize);
  end;
 end;
 procedure BigNumShiftRight(var BigNum:TBigNum;Shift:TPasDblStrUtilsInt32);
 begin
  if BigNum.UsedDigits<>0 then begin
   dec(BigNum.Exponent,Shift div BigitSize);
   BignumEnsureCapacity(BigNum,BigNum.UsedDigits);
   BigNumBigitsShiftRight(BigNum,Shift mod BigitSize);
  end;
 end;
 procedure BigNumMultiplyByUInt32(var BigNum:TBigNum;Factor:TPasDblStrUtilsUInt16);
 var Carry,Product:TPasDblStrUtilsUInt64;
     i:TPasDblStrUtilsInt32;
 begin
  if Factor=0 then begin
   BigNumZero(BigNum);
  end else if Factor<>1 then begin
   Assert(BigitSize<32);
   Carry:=0;
   for i:=0 to BigNum.UsedDigits-1 do begin
    Product:=(Factor*BigNum.Bigits[i])+Carry;
    BigNum.Bigits[i]:=Product and BigitMask;
    Carry:=Product shr BigitSize;
   end;
   while Carry<>0 do begin
    BigNumEnsureCapacity(BigNum,BigNum.UsedDigits+1);
    BigNum.Bigits[BigNum.UsedDigits]:=Carry and BigitMask;
    inc(BigNum.UsedDigits);
    Carry:=Carry shr BigitSize;
   end;
  end;
 end;
 procedure BigNumMultiplyByUInt64(var BigNum:TBigNum;Factor:TPasDblStrUtilsUInt64);
 var Carry,Low,High,ProductLow,ProductHigh,Tmp:TPasDblStrUtilsUInt64;
     i:TPasDblStrUtilsInt32;
 begin
  if Factor=0 then begin
   BigNumZero(BigNum);
  end else if Factor<>1 then begin
   Assert(BigitSize<32);
   Carry:=0;
   Low:=Factor and $ffffffff;
   High:=Factor shr 32;
   for i:=0 to BigNum.UsedDigits-1 do begin
    ProductLow:=Low*BigNum.Bigits[i];
    ProductHigh:=High*BigNum.Bigits[i];
    Tmp:=(Carry and BigitMask)+ProductLow;
    BigNum.Bigits[i]:=Tmp and BigitMask;
    Carry:=(Carry shr BigitSize)+(Tmp shr BigitSize)+(ProductHigh shl (32-BigitSize));
   end;
   while Carry<>0 do begin
    BigNumEnsureCapacity(BigNum,BigNum.UsedDigits+1);
    BigNum.Bigits[BigNum.UsedDigits]:=Carry and BigitMask;
    inc(BigNum.UsedDigits);
    Carry:=Carry shr BigitSize;
   end;
  end;
 end;
 procedure BigNumSquare(var BigNum:TBigNum);
 var ProductLength,CopyOffset,i,BigitIndex1,BigitIndex2:TPasDblStrUtilsInt32;
     Accumulator:TBigNumDoubleChunk;
     Chunk1,Chunk2:TBigNumChunk;
 begin
  Assert(BigNumIsClamped(BigNum));
  ProductLength:=2*BigNum.UsedDigits;
  BigNumEnsureCapacity(BigNum,ProductLength);
  Assert(not ((1 shl (2*(BigItChunkSize-BigitSize)))<=BigNum.UsedDigits));
  Accumulator:=0;
  CopyOffset:=BigNum.UsedDigits;
  for i:=0 to BigNum.UsedDigits-1 do begin
   BigNum.Bigits[i+CopyOffset]:=BigNum.Bigits[i];
  end;
  for i:=0 to BigNum.UsedDigits-1 do begin
   BigitIndex1:=i;
   BigitIndex2:=0;
   while BigitIndex1>=0 do begin
    Chunk1:=BigNum.Bigits[CopyOffset+BigitIndex1];
    Chunk2:=BigNum.Bigits[CopyOffset+BigitIndex2];
    inc(Accumulator,TBigNumDoubleChunk(Chunk1)*Chunk2);
    dec(BigitIndex1);
    inc(BigitIndex2);
   end;
   BigNum.Bigits[i]:=Accumulator and BigitMask;
   Accumulator:=Accumulator shr BigitSize;
  end;
  for i:=BigNum.UsedDigits-1 to ProductLength-1 do begin
   BigitIndex1:=BigNum.UsedDigits-1;
   BigitIndex2:=i-BigitIndex1;
   while BigitIndex2<BigNum.UsedDigits do begin
    Chunk1:=BigNum.Bigits[CopyOffset+BigitIndex1];
    Chunk2:=BigNum.Bigits[CopyOffset+BigitIndex2];
    inc(Accumulator,TBigNumDoubleChunk(Chunk1)*Chunk2);
    dec(BigitIndex1);
    inc(BigitIndex2);
   end;
   BigNum.Bigits[i]:=Accumulator and BigitMask;
   Accumulator:=Accumulator shr BigitSize;
  end;
  Assert(Accumulator=0);
  BigNum.UsedDigits:=ProductLength;
  inc(BigNum.Exponent,BigNum.Exponent);
  BigNumClamp(BigNum);
 end;
 procedure BigNumAssignPowerUInt16(var BigNum:TBigNum;Base:TPasDblStrUtilsUInt16;PowerExponent:TPasDblStrUtilsInt32);
 var Shifts,BitSize,TmpBase,FinalSize,Mask:TPasDblStrUtilsInt32;
     ThisValue:TPasDblStrUtilsUInt64;
     DelayedMultipliciation:TPasDblStrUtilsBoolean;
 begin
  Assert(Base<>0);
  Assert(PowerExponent>=0);
  if PowerExponent=0 then begin
   BigNumAssignUInt16(BigNum,1);
  end else begin
   BigNumZero(BigNum);
   Shifts:=0;
   while (Base and 1)=0 do begin
    Base:=Base shr 1;
    inc(Shifts);
   end;
   BitSize:=0;
   TmpBase:=Base;
   while TmpBase<>0 do begin
    TmpBase:=TmpBase shr 1;
    inc(BitSize);
   end;
   FinalSize:=BitSize*PowerExponent;
   BigNumEnsureCapacity(BigNum,FinalSize);
   Mask:=1;
   while Mask<=PowerExponent do begin
    inc(Mask,Mask);
   end;
   Mask:=Mask shr 2;
   ThisValue:=Base;
   DelayedMultipliciation:=false;
   while (Mask<>0) and (ThisValue<=$ffffffff) do begin
    ThisValue:=ThisValue*ThisValue;
    if (PowerExponent and Mask)<>0 then begin
     if (ThisValue and not ((TPasDblStrUtilsUInt64(1) shl (64-BitSize))-1))=0 then begin
      ThisValue:=ThisValue*Base;
     end else begin
      DelayedMultipliciation:=true;
     end;
    end;
    Mask:=Mask shr 1;
   end;
   BigNumAssignUInt64(BigNum,ThisValue);
   if DelayedMultipliciation then begin
    BigNumMultiplyByUInt32(BigNum,Base);
   end;
   while Mask<>0 do begin
    BigNumSquare(BigNum);
    if (PowerExponent and Mask)<>0 then begin
     BigNumMultiplyByUInt32(BigNum,Base);
    end;
    Mask:=Mask shr 1;
   end;
   BigNumShiftLeft(BigNum,Shifts*PowerExponent);
  end;
 end;
 function BigNumDivideModuloIntBigNum(var BigNum:TBigNum;const Other:TBigNum):TPasDblStrUtilsUInt16;
 var ThisBigit,OtherBigit:TBigNumChunk;
     Quotient,DivisionEstimate:TPasDblStrUtilsUInt32;
 begin
  Assert(BigNumIsClamped(BigNum));
  Assert(BigNumIsClamped(Other));
  Assert(Other.UsedDigits>0);
  result:=0;
  if (BigNum.UsedDigits+BigNum.Exponent)>=(Other.UsedDigits+Other.Exponent) then begin
   BigNumAlign(BigNum,Other);
   while (BigNum.UsedDigits+BigNum.Exponent)>(Other.UsedDigits+Other.Exponent) do begin
    Assert(Other.Bigits[Other.UsedDigits-1]>=((1 shl BigitSize) div 16));
    inc(result,BigNum.Bigits[BigNum.UsedDigits-1]);
    BigNumSubtractTimes(BigNum,Other,BigNum.Bigits[BigNum.UsedDigits-1]);
   end;
   Assert((BigNum.UsedDigits+BigNum.Exponent)=(Other.UsedDigits+Other.Exponent));
   ThisBigit:=BigNum.Bigits[BigNum.UsedDigits-1];
   OtherBigit:=Other.Bigits[Other.UsedDigits-1];
   if Other.UsedDigits=1 then begin
    Quotient:=ThisBigit div OtherBigit;
    BigNum.Bigits[BigNum.UsedDigits-1]:=ThisBigit-(OtherBigit*Quotient);
    inc(result,Quotient);
    BigNumClamp(BigNum);
   end else begin
    DivisionEstimate:=ThisBigit div (OtherBigit+1);
    inc(result,DivisionEstimate);
    BigNumSubtractTimes(BigNum,Other,DivisionEstimate);
    if (OtherBigit*(DivisionEstimate+1))<=ThisBigit then begin
     while BigNumCompare(Other,BigNum)<=0 do begin
      BigNumSubtractBigNum(BigNum,Other);
      inc(result);
     end;
    end;
   end;
  end;
 end;
 function BigNumDivideModuloInt(var BigNum:TBigNum;Divisor:TPasDblStrUtilsUInt16):TPasDblStrUtilsUInt16;
 var q0,r0,q1,r1:TPasDblStrUtilsUInt64;
     i:integer;
 begin
  Assert(BigNumIsClamped(BigNum));
  q0:=0;
  for i:=BigNum.UsedDigits-1 downto 1 do begin
   q1:=(BigNum.Bigits[i] div Divisor)+q0;
   r1:=((BigNum.Bigits[i] mod Divisor) shl 16)+(BigNum.Bigits[i-1] shr 16);
   q0:=((r1 div Divisor) shl 16);
   r0:=r1 mod Divisor;
   BigNum.Bigits[i]:=q1;
   BigNum.Bigits[i-1]:=(r0 shl 16)+(BigNum.Bigits[i-1] and $ffff);
  end;
  q1:=(BigNum.Bigits[0] div Divisor)+q0;
  r1:=BigNum.Bigits[0] mod Divisor;
  BigNum.Bigits[0]:=q1;
  result:=r1;
  BigNumClamp(BigNum);
 end;
 function NormalizedExponent(SignificantMantissa:TPasDblStrUtilsUInt64;Exponent:TPasDblStrUtilsInt32):TPasDblStrUtilsInt32;
 begin
  Assert(SignificantMantissa<>0);
  while (SignificantMantissa and TPasDblStrUtilsUInt64($0010000000000000))=0 do begin
   SignificantMantissa:=SignificantMantissa shl 1;
   dec(Exponent);
  end;
  result:=Exponent;
 end;
 function GetEstimatePower(Exponent:TPasDblStrUtilsInt32):TPasDblStrUtilsInt32;
 begin
  result:=TPasDblStrUtilsInt32(TPasDblStrUtilsInt64(((Exponent+52)*TPasDblStrUtilsInt64(1292913986))-$1000) shr 32)+1; // result:=System.Trunc(Math.Ceil(((Exponent+52)*0.30102999566398114)-(1e-10)));
 end;
 function GetEstimatePowerOf(Exponent,Radix:TPasDblStrUtilsInt32):TPasDblStrUtilsInt32;
 begin
  result:=TPasDblStrUtilsInt32(TPasDblStrUtilsInt64(((Exponent+52)*DoubleToStringEstimatePowerFactorTable[Radix])-$1000) shr 32)+1; // result:=System.Trunc(Math.Ceil(((Exponent+52)*(ln(2)/ln(Radix)))-(1e-10)));
 end;
 procedure GenerateShortestDigits(var Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;IsEven:TPasDblStrUtilsBoolean;var Buffer:TPasDblStrUtilsString;var Len:TPasDblStrUtilsInt32);
 var Digit,Compare:TPasDblStrUtilsInt32;
     InDeltaRoomMinus,InDeltaRoomPlus:TPasDblStrUtilsBoolean;
 begin
  Len:=0;
  while true do begin
   Digit:=BigNumDivideModuloIntBigNum(Numerator,Denominator);
   Assert((Digit>=0) and (Digit<=9));
   inc(Len);
   if Len>=length(Buffer) then begin
    SetLength(Buffer,Len*2);
   end;
   Buffer[Len]:=TPasDblStrUtilsChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))+Digit));
   if IsEven then begin
    InDeltaRoomMinus:=BigNumCompare(Numerator,DeltaMinus)<=0;
   end else begin
    InDeltaRoomMinus:=BigNumCompare(Numerator,DeltaMinus)<0;
   end;
   if IsEven then begin
    InDeltaRoomPlus:=BigNumPlusCompare(Numerator,DeltaPlus,Denominator)>=0;
   end else begin
    InDeltaRoomPlus:=BigNumPlusCompare(Numerator,DeltaPlus,Denominator)>0;
   end;
   if (not InDeltaRoomMinus) and (not InDeltaRoomPlus) then begin
    BigNumMultiplyByUInt32(Numerator,10);
    BigNumMultiplyByUInt32(DeltaMinus,10);
    BigNumMultiplyByUInt32(DeltaPlus,10);
   end else if InDeltaRoomMinus and InDeltaRoomPlus then begin
    Compare:=BigNumPlusCompare(Numerator,Numerator,Denominator);
    if Compare<0 then begin
    end else if Compare>0 then begin
     Assert(Buffer[Len]<>'9');
     inc(Buffer[Len]);
    end else begin
     if ((ord(Buffer[Len])-ord('0')) and 1)<>0 then begin
      Assert(Buffer[Len]<>'9');
      inc(Buffer[Len]);
     end;
    end;
    exit;
   end else if InDeltaRoomMinus then begin
    exit;
   end else begin
    Assert(Buffer[Len]<>'9');
    inc(Buffer[Len]);
    exit;
   end;
  end;
 end;
 procedure GenerateCountedDigits(Count:TPasDblStrUtilsInt32;var DecimalPoint:TPasDblStrUtilsInt32;var Numerator,Denominator:TBigNum;var Buffer:TPasDblStrUtilsString;var Len:TPasDblStrUtilsInt32);
 var i,Digit:TPasDblStrUtilsInt32;
 begin
  Assert(Count>=0);
  for i:=1 to Count-1 do begin
   Digit:=BigNumDivideModuloIntBigNum(Numerator,Denominator);
   Assert((Digit>=0) and (Digit<=9));
   inc(Len);
   if Len>=length(Buffer) then begin
    SetLength(Buffer,Len*2);
   end;
   Buffer[Len]:=TPasDblStrUtilsChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))+Digit));
   BigNumMultiplyByUInt32(Numerator,10);
  end;
  Digit:=BigNumDivideModuloIntBigNum(Numerator,Denominator);
  if BigNumPlusCompare(Numerator,Numerator,Denominator)>=0 then begin
   inc(Digit);
  end;
  inc(Len);
  if Len>=length(Buffer) then begin
   SetLength(Buffer,Len*2);
  end;
  Buffer[Len]:=TPasDblStrUtilsChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))+Digit));
  for i:=Len downto 2 do begin
   if ord(Buffer[i])<>(ord('0')+10) then begin
    break;
   end;
   Buffer[i]:='0';
   inc(Buffer[i-1]);
  end;
  if ord(Buffer[1])=(ord('0')+10) then begin
   Buffer[1]:='1';
   inc(DecimalPoint);
  end;
 end;
 procedure GenerateFixedDigits(RequestedDigits:TPasDblStrUtilsInt32;var DecimalPoint:TPasDblStrUtilsInt32;var Numerator,Denominator:TBigNum;var Buffer:TPasDblStrUtilsString;var Len:TPasDblStrUtilsInt32);
 begin
  if (-DecimalPoint)>RequestedDigits then begin
   DecimalPoint:=-RequestedDigits;
   Len:=0;
  end else if (-DecimalPoint)=RequestedDigits then begin
   Assert(DecimalPoint=(-RequestedDigits));
   BigNumMultiplyByUInt32(Denominator,10);
   if BigNumPlusCompare(Numerator,Numerator,Denominator)>=0 then begin
    Buffer:='1';
    Len:=1;
  end else begin
    Len:=0;
   end;
  end else begin
   GenerateCountedDigits(DecimalPoint+RequestedDigits,DecimalPoint,Numerator,Denominator,Buffer,Len);
  end;
 end;
 procedure FixupMultiplyBase(EstimatedPower:TPasDblStrUtilsInt32;IsEven:TPasDblStrUtilsBoolean;var DecimalPoint:TPasDblStrUtilsInt32;var Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;Base:TPasDblStrUtilsInt32);
 var InRange:TPasDblStrUtilsBoolean;
 begin
  if IsEven then begin
   InRange:=BigNumPlusCompare(Numerator,DeltaPlus,Denominator)>=0;
  end else begin
   InRange:=BigNumPlusCompare(Numerator,DeltaPlus,Denominator)>0;
  end;
  if InRange then begin
   DecimalPoint:=EstimatedPower+1;
  end else begin
   DecimalPoint:=EstimatedPower;
   BigNumMultiplyByUInt32(Numerator,Base);
   if BigNumCompare(DeltaMinus,DeltaPlus)=0 then begin
    BigNumMultiplyByUInt32(DeltaMinus,Base);
    BigNumAssignBigNum(DeltaPlus,DeltaMinus);
   end else begin
    BigNumMultiplyByUInt32(DeltaMinus,Base);
    BigNumMultiplyByUInt32(DeltaPlus,Base);
   end;
  end;
 end;
 procedure InitialScaledStartValuesPositiveExponent(Casted,SignificantMantissa:TPasDblStrUtilsUInt64;Exponent:TPasDblStrUtilsInt32;EstimatedPower:TPasDblStrUtilsInt32;NeedBoundaryDeltas:TPasDblStrUtilsBoolean;var Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;Base:TPasDblStrUtilsInt32);
 begin
  Assert(EstimatedPower>=0);

  BigNumAssignUInt64(Numerator,SignificantMantissa);
  BigNumShiftLeft(Numerator,Exponent);
  BigNumAssignPowerUInt16(Denominator,Base,EstimatedPower);

  if NeedBoundaryDeltas then begin
   BigNumShiftLeft(Numerator,1);
   BigNumShiftLeft(Denominator,1);

   BigNumAssignUInt16(DeltaPlus,1);
   BigNumShiftLeft(DeltaPlus,Exponent);

   BigNumAssignUInt16(DeltaMinus,1);
   BigNumShiftLeft(DeltaMinus,Exponent);

   if (Casted and TPasDblStrUtilsUInt64($000fffffffffffff))=0 then begin
    BigNumShiftLeft(Numerator,1);
    BigNumShiftLeft(Denominator,1);
    BigNumShiftLeft(DeltaPlus,1);
   end;
  end;
 end;
 procedure InitialScaledStartValuesNegativeExponentPositivePower(Casted,SignificantMantissa:TPasDblStrUtilsUInt64;Exponent:TPasDblStrUtilsInt32;EstimatedPower:TPasDblStrUtilsInt32;NeedBoundaryDeltas:TPasDblStrUtilsBoolean;var Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;Base:TPasDblStrUtilsInt32);
 begin
  BigNumAssignUInt64(Numerator,SignificantMantissa);
  BigNumAssignPowerUInt16(Denominator,Base,EstimatedPower);
  BigNumShiftLeft(Denominator,-Exponent);

  if NeedBoundaryDeltas then begin
   BigNumShiftLeft(Numerator,1);
   BigNumShiftLeft(Denominator,1);

   BigNumAssignUInt16(DeltaPlus,1);
   BigNumAssignUInt16(DeltaMinus,1);

   if (Casted and TPasDblStrUtilsUInt64($000fffffffffffff))=0 then begin
    BigNumShiftLeft(Numerator,1);
    BigNumShiftLeft(Denominator,1);
    BigNumShiftLeft(DeltaPlus,1);
   end;
  end;
 end;
 procedure InitialScaledStartValuesNegativeExponentNegativePower(Casted,SignificantMantissa:TPasDblStrUtilsUInt64;Exponent:TPasDblStrUtilsInt32;EstimatedPower:TPasDblStrUtilsInt32;NeedBoundaryDeltas:TPasDblStrUtilsBoolean;var Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;Base:TPasDblStrUtilsInt32);
 begin
  BigNumAssignPowerUInt16(Numerator,Base,-EstimatedPower);
  if NeedBoundaryDeltas then begin
   BigNumAssignBigNum(DeltaPlus,Numerator);
   BigNumAssignBigNum(DeltaMinus,Numerator);
  end;
  BigNumMultiplyByUInt64(Numerator,SignificantMantissa);

  BigNumAssignUInt16(Denominator,1);
  BigNumShiftLeft(Denominator,-Exponent);

  if NeedBoundaryDeltas then begin
   BigNumShiftLeft(Numerator,1);
   BigNumShiftLeft(Denominator,1);
   if ((Casted and TPasDblStrUtilsUInt64($000fffffffffffff))=0) and ((Casted and TPasDblStrUtilsUInt64($7ff0000000000000))<>TPasDblStrUtilsUInt64($0010000000000000)) then begin
    BigNumShiftLeft(Numerator,1);
    BigNumShiftLeft(Denominator,1);
    BigNumShiftLeft(DeltaPlus,1);
   end;
  end;
 end;
 procedure InitialScaledStartValues(Casted,SignificantMantissa:TPasDblStrUtilsUInt64;Exponent:TPasDblStrUtilsInt32;EstimatedPower:TPasDblStrUtilsInt32;NeedBoundaryDeltas:TPasDblStrUtilsBoolean;var Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;Base:TPasDblStrUtilsInt32);
 begin
  if Exponent>=0 then begin
   InitialScaledStartValuesPositiveExponent(Casted,SignificantMantissa,Exponent,EstimatedPower,NeedBoundaryDeltas,Numerator,Denominator,DeltaMinus,DeltaPlus,Base);
  end else if EstimatedPower>=0 then begin
   InitialScaledStartValuesNegativeExponentPositivePower(Casted,SignificantMantissa,Exponent,EstimatedPower,NeedBoundaryDeltas,Numerator,Denominator,DeltaMinus,DeltaPlus,Base);
  end else begin
   InitialScaledStartValuesNegativeExponentNegativePower(Casted,SignificantMantissa,Exponent,EstimatedPower,NeedBoundaryDeltas,Numerator,Denominator,DeltaMinus,DeltaPlus,Base);
  end;
 end;
 procedure DoubleToDecimal(Value:TPasDblStrUtilsDouble;Mode,RequestedDigits:TPasDblStrUtilsInt32;var Buffer:TPasDblStrUtilsString;var Len,DecimalPoint:TPasDblStrUtilsInt32);
 var Casted:TPasDblStrUtilsUInt64 absolute Value;
     SignificantMantissa:TPasDblStrUtilsUInt64;
     Exponent,EstimatedPower:TPasDblStrUtilsInt32;
     Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;
     IsEven,NeedBoundaryDeltas:TPasDblStrUtilsBoolean;
 begin
  Assert(Value>0);
  Assert(IsFinite(Value));
  SplitDouble(Value,SignificantMantissa,Exponent);
  IsEven:=(SignificantMantissa and 1)=0;
  EstimatedPower:=GetEstimatePower(NormalizedExponent(SignificantMantissa,Exponent));
  if (Mode=ModeFixed) and (((-EstimatedPower)-1)>RequestedDigits) then begin
   Buffer:='';
   Len:=0;
   DecimalPoint:=-RequestedDigits;
  end else begin
   Assert(BigNumMaxSignificantMantissaBits>=(324*4));
   NeedBoundaryDeltas:=Mode=ModeShortest;
   InitialScaledStartValues(Casted,SignificantMantissa,Exponent,EstimatedPower,NeedBoundaryDeltas,Numerator,Denominator,DeltaMinus,DeltaPlus,10);
   FixupMultiplyBase(EstimatedPower,IsEven,DecimalPoint,Numerator,Denominator,DeltaMinus,DeltaPlus,10);
   case Mode of
    ModeShortest:begin
     GenerateShortestDigits(Numerator,Denominator,DeltaMinus,DeltaPlus,IsEven,Buffer,Len);
    end;
    ModeFixed:begin
     GenerateFixedDigits(RequestedDigits,DecimalPoint,Numerator,Denominator,Buffer,Len);
    end;
    else {ModePrecision:}begin
     GenerateCountedDigits(RequestedDigits,DecimalPoint,Numerator,Denominator,Buffer,Len);
    end;
   end;
  end;
 end;
 procedure GenerateRadixDigits(var Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;IsEven:TPasDblStrUtilsBoolean;var Buffer:TPasDblStrUtilsString;var Len:TPasDblStrUtilsInt32;Radix:TPasDblStrUtilsInt32);
 const Base36:array[0..36] of TPasDblStrUtilsChar='0123456789abcdefghijklmnopqrstuvwxyz{';
 var Digit,Compare,MaxDigit:TPasDblStrUtilsInt32;
     InDeltaRoomMinus,InDeltaRoomPlus:TPasDblStrUtilsBoolean;
  function ValueOf(c:TPasDblStrUtilsChar):TPasDblStrUtilsInt32;
  begin
   case c of
    '0'..'9':begin
     result:=ord(c)-ord('0');
    end;
    else begin
     result:=(ord(c)-ord('a'))+$a;
    end;
   end;
  end;
 begin
  Len:=0;
  MaxDigit:=Radix-1;
  while true do begin
   Digit:=BigNumDivideModuloIntBigNum(Numerator,Denominator);
   Assert((Digit>=0) and (Digit<=MaxDigit));
   inc(Len);
   if Len>=length(Buffer) then begin
    SetLength(Buffer,Len*2);
   end;
   Buffer[Len]:=Base36[Digit];
   BigNumClamp(Numerator);
   BigNumClamp(DeltaMinus);
   BigNumClamp(DeltaPlus);
   if IsEven then begin
    InDeltaRoomMinus:=BigNumCompare(Numerator,DeltaMinus)<=0;
   end else begin
    InDeltaRoomMinus:=BigNumCompare(Numerator,DeltaMinus)<0;
   end;
   if IsEven then begin
    InDeltaRoomPlus:=BigNumPlusCompare(Numerator,DeltaPlus,Denominator)>=0;
   end else begin
    InDeltaRoomPlus:=BigNumPlusCompare(Numerator,DeltaPlus,Denominator)>0;
   end;
   if (not InDeltaRoomMinus) and (not InDeltaRoomPlus) then begin
    BigNumMultiplyByUInt32(Numerator,Radix);
    BigNumMultiplyByUInt32(DeltaMinus,Radix);
    BigNumMultiplyByUInt32(DeltaPlus,Radix);
   end else if InDeltaRoomMinus and InDeltaRoomPlus then begin
    Compare:=BigNumPlusCompare(Numerator,Numerator,Denominator);
    if Compare<0 then begin
    end else if Compare>0 then begin
     Assert(ValueOf(Buffer[Len])<>MaxDigit);
     Buffer[Len]:=Base36[ValueOf(Buffer[Len])+1];
    end else begin
     if (ValueOf(Buffer[Len]) and 1)<>0 then begin
      Assert(ValueOf(Buffer[Len])<>MaxDigit);
      Buffer[Len]:=Base36[ValueOf(Buffer[Len])+1];
     end;
    end;
    exit;
   end else if InDeltaRoomMinus then begin
    exit;
   end else begin
    Assert(ValueOf(Buffer[Len])<>MaxDigit);
    Buffer[Len]:=Base36[ValueOf(Buffer[Len])+1];
    exit;
   end;
  end;
 end;
 procedure DoubleToRadix(Value:TPasDblStrUtilsDouble;Radix:TPasDblStrUtilsInt32;var Buffer:TPasDblStrUtilsString;var Len,DecimalPoint:TPasDblStrUtilsInt32);
 var Casted:TPasDblStrUtilsUInt64 absolute Value;
     SignificantMantissa:TPasDblStrUtilsUInt64;
     Exponent,EstimatedPower:TPasDblStrUtilsInt32;
     Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;
     IsEven,NeedBoundaryDeltas:TPasDblStrUtilsBoolean;
 begin
  Assert(Value>0);
  Assert(IsFinite(Value));
  SplitDouble(Value,SignificantMantissa,Exponent);
  IsEven:=(SignificantMantissa and 1)=0;
  EstimatedPower:=GetEstimatePowerOf(NormalizedExponent(SignificantMantissa,Exponent),Radix);
  Assert(BigNumMaxSignificantMantissaBits>=(324*4));
  NeedBoundaryDeltas:=true;
  InitialScaledStartValues(Casted,SignificantMantissa,Exponent,EstimatedPower,NeedBoundaryDeltas,Numerator,Denominator,DeltaMinus,DeltaPlus,Radix);
  FixupMultiplyBase(EstimatedPower,IsEven,DecimalPoint,Numerator,Denominator,DeltaMinus,DeltaPlus,Radix);
  GenerateRadixDigits(Numerator,Denominator,DeltaMinus,DeltaPlus,IsEven,Buffer,Len,Radix);
 end;
 {$warnings off}
 procedure FastDoubleToRadix(v:TPasDblStrUtilsDouble;Radix:TPasDblStrUtilsInt32;var Buffer:TPasDblStrUtilsString;var Len,DecimalPoint:TPasDblStrUtilsInt32);
 const Base36:array[0..35] of TPasDblStrUtilsChar='0123456789abcdefghijklmnopqrstuvwxyz';
       DtoAFPUExceptionMask:TFPUExceptionMask=[exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision];
       DtoAFPUPrecisionMode:TFPUPrecisionMode=pmDOUBLE;
       DtoAFPURoundingMode:TFPURoundingMode=rmNEAREST;
 var IntPart,FracPart,Old,Epsilon:TPasDblStrUtilsDouble;
     Digit,i,j:TPasDblStrUtilsInt32;
     TempBuffer:TPasDblStrUtilsString;
     OldFPUExceptionMask:TFPUExceptionMask;
     OldFPUPrecisionMode:TFPUPrecisionMode;
     OldFPURoundingMode:TFPURoundingMode;
     IntPart64:TPasDblStrUtilsInt64;
 begin
  if (Radix<2) or (Radix>36) then begin
   result:='';
  end else begin
   OldFPUExceptionMask:=GetExceptionMask;
   OldFPUPrecisionMode:=GetPrecisionMode;
   OldFPURoundingMode:=GetRoundMode;
   try
    if OldFPUExceptionMask<>DtoAFPUExceptionMask then begin
     SetExceptionMask(DtoAFPUExceptionMask);
    end;
    if OldFPUPrecisionMode<>DtoAFPUPrecisionMode then begin
     SetPrecisionMode(DtoAFPUPrecisionMode);
    end;
    if OldFPURoundingMode<>DtoAFPURoundingMode then begin
     SetRoundMode(DtoAFPURoundingMode);
    end;
    try
     TempBuffer:='';
     IntPart:=System.Int(v);
     FracPart:=System.Frac(v);
     if IntPart=0 then begin
      result:='0';
     end else begin
      if IntPart<4294967295.0 then begin
       IntPart64:=trunc(IntPart);
       while IntPart64>0 do begin
        Digit:=IntPart64 mod Radix;
        Assert((Digit>=0) and (Digit<Radix));
        IntPart64:=IntPart64 div Radix;
        inc(Len);
        if Len>=length(TempBuffer) then begin
         SetLength(TempBuffer,Len*2);
        end;
        TempBuffer[Len]:=Base36[Digit];
       end;
      end else begin
       while IntPart>0 do begin
        Old:=IntPart;
        IntPart:=System.Int(IntPart/Radix);
        Digit:=trunc(Old-(IntPart*Radix));
        Assert((Digit>=0) and (Digit<Radix));
        inc(Len);
        if Len>=length(TempBuffer) then begin
         SetLength(TempBuffer,Len*2);
        end;
        TempBuffer[Len]:=Base36[Digit];
       end;
      end;
      SetLength(Buffer,Len);
      j:=1;
      for i:=Len downto 1 do begin
       Buffer[j]:=TempBuffer[i];
       inc(j);
      end;
     end;
     if FracPart<>0 then begin
      inc(Len);
      if Len>=length(Buffer) then begin
       SetLength(Buffer,Len*2);
      end;
      Buffer[Len]:='.';
      Epsilon:=0.001/Radix;
      while (FracPart>=Epsilon) and (Len<32) do begin
       FracPart:=FracPart*Radix;
       Digit:=trunc(FracPart);
       FracPart:=System.Frac(FracPart);
       Assert((Digit>=0) and (Digit<Radix));
       inc(Len);
       if Len>=length(Buffer) then begin
        SetLength(Buffer,Len*2);
       end;
       Buffer[Len]:=Base36[Digit];
      end;
     end;
    finally
     TempBuffer:='';
    end;
   finally
    if OldFPUExceptionMask<>DtoAFPUExceptionMask then begin
     SetExceptionMask(OldFPUExceptionMask);
    end;
    if OldFPUPrecisionMode<>DtoAFPUPrecisionMode then begin
     SetPrecisionMode(OldFPUPrecisionMode);
    end;
    if OldFPURoundingMode<>DtoAFPURoundingMode then begin
     SetRoundMode(OldFPURoundingMode);
    end;
   end;
  end;
 end;
 {$warnings on}
 function GetCachedPowerForBinaryExponentRange(MinExponent,MaxExponent:TPasDblStrUtilsInt32;var Power:TDoubleValue;var DecimalExponent:TPasDblStrUtilsInt32):TPasDblStrUtilsBoolean;
 var Index:TPasDblStrUtilsInt32;
 begin
  result:=false;
  if (low(DoubleToStringPowerOfTenBinaryExponentTable)<=MinExponent) and (MinExponent<=high(DoubleToStringPowerOfTenBinaryExponentTable)) then begin
   Index:=DoubleToStringPowerOfTenBinaryExponentTable[MinExponent];
   if ((Index>=0) and (Index<length(DoubleToStringPowerOfTenTable))) and ((MinExponent<=DoubleToStringPowerOfTenTable[Index,1]) and (DoubleToStringPowerOfTenTable[Index,1]<=MaxExponent)) then begin
    Power.SignificantMantissa:=DoubleToStringPowerOfTenTable[Index,0];
    Power.Exponent:=DoubleToStringPowerOfTenTable[Index,1];
    DecimalExponent:=DoubleToStringPowerOfTenTable[Index,2];
    result:=true;
   end;
  end;
 end;
 function GetCachedPowerForDecimalExponent(RequestedExponent:TPasDblStrUtilsInt32;var Power:TDoubleValue;var FoundExponent:TPasDblStrUtilsInt32):TPasDblStrUtilsBoolean;
 var Index:TPasDblStrUtilsInt32;
 begin
  result:=false;
  if (low(DoubleToStringPowerOfTenDecimalExponentTable)<=RequestedExponent) and (RequestedExponent<=high(DoubleToStringPowerOfTenDecimalExponentTable)) then begin
   Index:=DoubleToStringPowerOfTenDecimalExponentTable[RequestedExponent];
   if (Index>=0) and (Index<length(DoubleToStringPowerOfTenTable)) then begin
    Power.SignificantMantissa:=DoubleToStringPowerOfTenTable[Index,0];
    Power.Exponent:=DoubleToStringPowerOfTenTable[Index,1];
    FoundExponent:=DoubleToStringPowerOfTenTable[Index,2];
    result:=true;
   end;
  end;
 end;
 function RoundWeed(var Buffer:TPasDblStrUtilsString;Len:TPasDblStrUtilsInt32;DistanceTooHighW,UnsafeInterval,Rest,TenCapacity,UnitValue:TPasDblStrUtilsUInt64):TPasDblStrUtilsBoolean;
 var SmallDistance,BigDistance:TPasDblStrUtilsUInt64;
 begin
  SmallDistance:=DistanceTooHighW-UnitValue;
  BigDistance:=DistanceTooHighW+UnitValue;
  Assert(QWordLessOrEqual(Rest,UnsafeInterval));
  while (QWordLess(Rest,SmallDistance) and (QWordGreaterOrEqual(UnsafeInterval-Rest,TenCapacity))) and (QWordLess(Rest+TenCapacity,SmallDistance) or QWordGreaterOrEqual(SmallDistance-Rest,((Rest+TenCapacity)-SmallDistance))) do begin
   dec(Buffer[Len]);
   inc(Rest,TenCapacity);
  end;
  if ((QWordLess(Rest,BigDistance) and QWordGreaterOrEqual(UnsafeInterval-Rest,TenCapacity)) and (QWordLess(Rest+TenCapacity,BigDistance) or QWordGreater(BigDistance-Rest,((Rest+TenCapacity)-BigDistance)))) then begin
   result:=false;
  end else begin
   result:=(QWordLessOrEqual(2*UnitValue,Rest) and QWordLessOrEqual(Rest,UnsafeInterval-(4*UnitValue)));
  end;
 end;
 function RoundWeedCounted(var Buffer:TPasDblStrUtilsString;Len:TPasDblStrUtilsInt32;Rest,TenCapacity,UnitValue:TPasDblStrUtilsUInt64;var Capacity:TPasDblStrUtilsInt32):TPasDblStrUtilsBoolean;
 var i:TPasDblStrUtilsInt32;
 begin
  Assert(QWordLess(Rest,TenCapacity));
  result:=false;
  if QWordGreater(TenCapacity-UnitValue,UnitValue) then begin
   result:=QWordGreater(TenCapacity-Rest,Rest) and QWordGreaterOrEqual(TenCapacity-(2*Rest),2*UnitValue);
   if not result then begin
    result:=QWordGreater(Rest,UnitValue) and QWordLessOrEqual(TenCapacity-(Rest-UnitValue),Rest-UnitValue);
    if result then begin
     inc(Buffer[Len]);
     for i:=Len downto 2 do begin
      if ord(Buffer[i])<>(ord('0')+10) then begin
       break;
      end;
      Buffer[i]:='0';
      inc(Buffer[i-1]);
     end;
    end;
    if ord(Buffer[1])=(ord('0')+10) then begin
     Buffer[1]:='1';
     inc(Capacity);
    end;
   end;
  end;
 end;
 function BiggestPowerTen(Number:TPasDblStrUtilsUInt32;NumberBits:TPasDblStrUtilsInt32;var Power:TPasDblStrUtilsUInt32;var Exponent:TPasDblStrUtilsInt32):TPasDblStrUtilsBoolean;
 label c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11;
 begin
  result:=true;
  case NumberBits of
   30,31,32:begin
    c1:
    if 1000000000<=Number then begin
     Power:=1000000000;
     Exponent:=9;
    end else begin
     goto c2;
    end;
   end;
   27,28,29:begin
    c2:
    if 100000000<=Number then begin
     Power:=100000000;
     Exponent:=8;
    end else begin
     goto c3;
    end;
   end;
   24,25,26:begin
    c3:
    if 10000000<=Number then begin
     Power:=10000000;
     Exponent:=7;
    end else begin
     goto c4;
    end;
   end;
   20,21,22,23:begin
    c4:
    if 1000000<=Number then begin
     Power:=1000000;
     Exponent:=6;
    end else begin
     goto c5;
    end;
   end;
   17,18,19:begin
    c5:
    if 100000<=Number then begin
     Power:=100000;
     Exponent:=5;
    end else begin
     goto c6;
    end;
   end;
   14,15,16:begin
    c6:
    if 10000<=Number then begin
     Power:=10000;
     Exponent:=4;
    end else begin
     goto c7;
    end;
   end;
   10,11,12,13:begin
    c7:
    if 1000<=Number then begin
     Power:=1000;
     Exponent:=3;
    end else begin
     goto c8;
    end;
   end;
   7,8,9:begin
    c8:
    if 100<=Number then begin
     Power:=100;
     Exponent:=2;
    end else begin
     goto c9;
    end;
   end;
   4,5,6:begin
    c9:
    if 10<=Number then begin
     Power:=10;
     Exponent:=1;
    end else begin
     goto c10;
    end;
   end;
   1,2,3:begin
    c10:
    if 1<=Number then begin
     Power:=1;
     Exponent:=0;
    end else begin
     goto c11;
    end;
   end;
   0:begin
    c11:
    Power:=0;
    Exponent:=-1;
   end;
   else begin
    Power:=0;
    Exponent:=0;
    result:=false;
   end;
  end;
 end;
 function DigitGen(Low,w,High:TDoubleValue;var Buffer:TPasDblStrUtilsString;var Len,Capacity:TPasDblStrUtilsInt32):TPasDblStrUtilsBoolean;
 var UnitValue,Fractionals,Rest:TPasDblStrUtilsUInt64;
     TooLow,TooHigh,UnsafeInterval,One:TDoubleValue;
     Integrals,Divisor,Digit:TPasDblStrUtilsUInt32;
     DivisorExponent:TPasDblStrUtilsInt32;
 begin
  result:=false;
  if ((Low.Exponent=w.Exponent) and (w.Exponent=High.Exponent)) and (QWordLessOrEqual(Low.SignificantMantissa+1,High.SignificantMantissa-1) and
     ((MinimalTargetExponent<=w.Exponent) and (w.Exponent<=MaximalTargetExponent))) then begin
   UnitValue:=1;
   TooLow.SignificantMantissa:=Low.SignificantMantissa-UnitValue;
   TooLow.Exponent:=Low.Exponent;
   TooHigh.SignificantMantissa:=High.SignificantMantissa+UnitValue;
   TooHigh.Exponent:=High.Exponent;
   UnsafeInterval:=DoubleValueMinus(TooHigh,TooLow);
   One.SignificantMantissa:=TPasDblStrUtilsUInt64(1) shl (-w.Exponent);
   One.Exponent:=w.Exponent;
   Integrals:=TooHigh.SignificantMantissa shr (-One.Exponent);
   Fractionals:=TooHigh.SignificantMantissa and (One.SignificantMantissa-1);
   Divisor:=0;
   DivisorExponent:=0;
   if BiggestPowerTen(Integrals,SignificantMantissaSize-(-One.Exponent),Divisor,DivisorExponent) then begin
    Capacity:=DivisorExponent+1;
    Len:=0;
    while Capacity>0 do begin
     Digit:=Integrals div Divisor;
     Integrals:=Integrals mod Divisor;
     inc(Len);
     if Len>=length(Buffer) then begin
      SetLength(Buffer,Len*2);
     end;
     Buffer[Len]:=TPasDblStrUtilsChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))+Digit));
     dec(Capacity);
     Rest:=TPasDblStrUtilsUInt64(TPasDblStrUtilsUInt64(Integrals) shl (-One.Exponent))+Fractionals;
     if QWordLess(Rest,UnsafeInterval.SignificantMantissa) then begin
      result:=RoundWeed(Buffer,Len,DoubleValueMinus(TooHigh,w).SignificantMantissa,UnsafeInterval.SignificantMantissa,Rest,TPasDblStrUtilsUInt64(Divisor) shl (-One.Exponent),UnitValue);
      exit;
     end;
     Divisor:=Divisor div 10;
    end;
    if (One.Exponent>=-60) and (QWordLess(Fractionals,One.SignificantMantissa) and QWordGreaterOrEqual(TPasDblStrUtilsUInt64($1999999999999999),One.SignificantMantissa)) then begin
     while true do begin
      Fractionals:=Fractionals*10;
      UnitValue:=UnitValue*10;
      UnsafeInterval.SignificantMantissa:=UnsafeInterval.SignificantMantissa*10;
      Digit:=Fractionals shr (-One.Exponent);
      inc(Len);
      if Len>=length(Buffer) then begin
       SetLength(Buffer,Len*2);
      end;
      Buffer[Len]:=TPasDblStrUtilsChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))+Digit));
      dec(Capacity);
      Fractionals:=Fractionals and (One.SignificantMantissa-1);
      if QWordLess(Fractionals,UnsafeInterval.SignificantMantissa) then begin
       result:=RoundWeed(Buffer,Len,DoubleValueMinus(TooHigh,w).SignificantMantissa*UnitValue,UnsafeInterval.SignificantMantissa,Fractionals,One.SignificantMantissa,UnitValue);
       exit;
      end;
     end;
    end;
   end;
  end;
 end;
 function DigitGenCounted(w:TDoubleValue;RequestedDigits:TPasDblStrUtilsInt32;var Buffer:TPasDblStrUtilsString;var Len,Capacity:TPasDblStrUtilsInt32):TPasDblStrUtilsBoolean;
 var wError,Fractionals,Rest:TPasDblStrUtilsUInt64;
     One:TDoubleValue;
     Integrals,Divisor,Digit:TPasDblStrUtilsUInt32;
     DivisorExponent:TPasDblStrUtilsInt32;
 begin
  result:=false;
  if ((MinimalTargetExponent<=w.Exponent) and (w.Exponent<=MaximalTargetExponent)) and ((MinimalTargetExponent>=-60) and (MaximalTargetExponent<=-32)) then begin
   wError:=1;
   One.SignificantMantissa:=TPasDblStrUtilsUInt64(1) shl (-w.Exponent);
   One.Exponent:=w.Exponent;
   Integrals:=w.SignificantMantissa shr (-One.Exponent);
   Fractionals:=w.SignificantMantissa and (One.SignificantMantissa-1);
   Divisor:=0;
   DivisorExponent:=0;
   if BiggestPowerTen(Integrals,SignificantMantissaSize-(-One.Exponent),Divisor,DivisorExponent) then begin
    Capacity:=DivisorExponent+1;
    Len:=0;
    while Capacity>0 do begin
     Digit:=Integrals div Divisor;
     Integrals:=Integrals mod Divisor;
     inc(Len);
     if Len>=length(Buffer) then begin
      SetLength(Buffer,Len*2);
     end;
     Buffer[Len]:=TPasDblStrUtilsChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))+Digit));
     dec(RequestedDigits);
     dec(Capacity);
     if RequestedDigits=0 then begin
      break;
     end;
     Divisor:=Divisor div 10;
    end;
    if RequestedDigits=0 then begin
     Rest:=TPasDblStrUtilsUInt64(TPasDblStrUtilsUInt64(Integrals) shl (-One.Exponent))+Fractionals;
     result:=RoundWeedCounted(Buffer,Len,Rest,TPasDblStrUtilsUInt64(Divisor) shl (-One.Exponent),wError,Capacity);
     exit;
    end;
    if ((One.Exponent>=-60) and QWordLess(Fractionals,One.SignificantMantissa)) and QWordGreaterOrEqual(TPasDblStrUtilsUInt64($1999999999999999),One.SignificantMantissa) then begin
     while (RequestedDigits>0) and (Fractionals>wError) do begin
      Fractionals:=Fractionals*10;
      Digit:=Fractionals shr (-One.Exponent);
      inc(Len);
      if Len>=length(Buffer) then begin
       SetLength(Buffer,Len*2);
      end;
      Buffer[Len]:=TPasDblStrUtilsChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))+Digit));
      dec(RequestedDigits);
      dec(Capacity);
      Fractionals:=Fractionals and (One.SignificantMantissa-1);
     end;
     if RequestedDigits=0 then begin
      result:=RoundWeedCounted(Buffer,Len,Fractionals,One.SignificantMantissa,wError,Capacity);
     end else begin
      result:=false;
     end;
    end;
   end;
  end;
 end;
 procedure NormalizedBoundaries(Value:TPasDblStrUtilsDouble;var BoundaryMinus,BoundaryPlus:TDoubleValue);
 var v:TDoubleValue;
     SignificantMantissaIsZero:TPasDblStrUtilsBoolean;
 begin
  Assert(not IsNegative(Value));
  Assert(IsFinite(Value));
  SplitDouble(Value,v.SignificantMantissa,v.Exponent);
  SignificantMantissaIsZero:=v.SignificantMantissa=TPasDblStrUtilsUInt64($0010000000000000);
  BoundaryPlus.SignificantMantissa:=(v.SignificantMantissa shl 1)+1;
  BoundaryPlus.Exponent:=v.Exponent-1;
  DoubleValueNormalize(BoundaryPlus);
  if SignificantMantissaIsZero and (v.Exponent<>((-($3ff+52))+1)) then begin
   BoundaryMinus.SignificantMantissa:=(v.SignificantMantissa shl 2)-1;
   BoundaryMinus.Exponent:=v.Exponent-2;
  end else begin
   BoundaryMinus.SignificantMantissa:=(v.SignificantMantissa shl 1)-1;
   BoundaryMinus.Exponent:=v.Exponent-1;
  end;
  BoundaryMinus.SignificantMantissa:=BoundaryMinus.SignificantMantissa shl (BoundaryMinus.Exponent-BoundaryPlus.Exponent);
  BoundaryMinus.Exponent:=BoundaryPlus.Exponent;
 end;
 function DoFastShortest(Value:TPasDblStrUtilsDouble;var Buffer:TPasDblStrUtilsString;var Len,DecimalExponent:TPasDblStrUtilsInt32):TPasDblStrUtilsBoolean;
 var w,BoundaryMinus,BoundaryPlus,TenMK,ScaledW,ScaledBoundaryMinus,ScaledBoundaryPlus:TDoubleValue;
     mK,TenMKMinimalBinaryExponent,TenMKMaximalBinaryExponent,Capacity:TPasDblStrUtilsInt32;
 begin
  result:=false;
  w:=DoubleValueGet(Value);
  NormalizedBoundaries(Value,BoundaryMinus,BoundaryPlus);
  Assert(BoundaryPlus.Exponent=w.Exponent);
  TenMKMinimalBinaryExponent:=MinimalTargetExponent-(w.Exponent+SignificantMantissaSize);
  TenMKMaximalBinaryExponent:=MaximalTargetExponent-(w.Exponent+SignificantMantissaSize);
  if GetCachedPowerForBinaryExponentRange(TenMKMinimalBinaryExponent,TenMKMaximalBinaryExponent,TenMK,mK) then begin
   if (MinimalTargetExponent<=(w.Exponent+TenMK.Exponent+SignificantMantissaSize)) and (MaximalTargetExponent>=(w.Exponent+TenMK.Exponent+SignificantMantissaSize)) then begin
    ScaledW:=DoubleValueMul(w,TenMK);
    if ScaledW.Exponent=(BoundaryPlus.Exponent+TenMK.Exponent+SignificantMantissaSize) then begin
     ScaledBoundaryMinus:=DoubleValueMul(BoundaryMinus,TenMK);
     ScaledBoundaryPlus:=DoubleValueMul(BoundaryPlus,TenMK);
     Capacity:=0;
     result:=DigitGen(ScaledBoundaryMinus,ScaledW,ScaledBoundaryPlus,Buffer,Len,Capacity);
     DecimalExponent:=Capacity-mK;
    end;
   end;
  end;
 end;
 function DoFastPrecision(Value:TPasDblStrUtilsDouble;RequestedDigits:TPasDblStrUtilsInt32;var Buffer:TPasDblStrUtilsString;var Len,DecimalExponent:TPasDblStrUtilsInt32):TPasDblStrUtilsBoolean;
 var w,TenMK,ScaledW:TDoubleValue;
     mK,TenMKMinimalBinaryExponent,TenMKMaximalBinaryExponent,Capacity:TPasDblStrUtilsInt32;
 begin
  result:=false;
  w:=DoubleValueGet(Value);
  TenMKMinimalBinaryExponent:=MinimalTargetExponent-(w.Exponent+SignificantMantissaSize);
  TenMKMaximalBinaryExponent:=MaximalTargetExponent-(w.Exponent+SignificantMantissaSize);
  if GetCachedPowerForBinaryExponentRange(TenMKMinimalBinaryExponent,TenMKMaximalBinaryExponent,TenMK,mK) then begin
   if (MinimalTargetExponent<=(w.Exponent+TenMK.Exponent+SignificantMantissaSize)) and (MaximalTargetExponent>=(w.Exponent+TenMK.Exponent+SignificantMantissaSize)) then begin
    ScaledW:=DoubleValueMul(w,TenMK);
    Capacity:=0;
    result:=DigitGenCounted(ScaledW,RequestedDigits,Buffer,Len,Capacity);
    DecimalExponent:=Capacity-mK;
   end;
  end;
 end;
 function DoFastFixed(Value:TPasDblStrUtilsDouble;FracitionalCount:TPasDblStrUtilsInt32;var Buffer:TPasDblStrUtilsString;var Len,DecimalPoint:TPasDblStrUtilsInt32):TPasDblStrUtilsBoolean;
 const Five17=$b1a2bc2ec5; // 5^17
 type TInt128=record
       High,Low:TPasDblStrUtilsUInt64;
      end;
  procedure Int128Mul(var a:TInt128;const Multiplicand:TPasDblStrUtilsUInt32);
  var Accumulator:TPasDblStrUtilsUInt64;
      Part:TPasDblStrUtilsUInt32;
  begin
   Accumulator:=(a.Low and $ffffffff)*Multiplicand;
   Part:=Accumulator and $ffffffff;
   Accumulator:=(Accumulator shr 32)+((a.Low shr 32)*Multiplicand);
   a.Low:=(Accumulator shl 32)+Part;
   Accumulator:=(Accumulator shr 32)+((a.High and $ffffffff)*Multiplicand);
   Part:=Accumulator and $ffffffff;
   Accumulator:=(Accumulator shr 32)+((a.High shr 32)*Multiplicand);
   a.High:=(Accumulator shl 32)+Part;
   Assert((Accumulator shr 32)=0);
  end;
  procedure Int128Shift(var a:TInt128;const Shift:TPasDblStrUtilsInt32);
  begin
   Assert(((-64)<=Shift) and (Shift<=64));
   if Shift<>0 then begin
    if Shift=-64 then begin
     a.High:=a.Low;
     a.Low:=0;
    end else if Shift=64 then begin
     a.Low:=a.High;
     a.High:=0;
    end else if Shift<=0 then begin
     a.High:=(a.High shl (-Shift))+(a.Low shr (64+Shift));
     a.Low:=a.Low shl (-Shift);
    end else begin
     a.Low:=(a.Low shr Shift)+(a.High shl (64-Shift));
     a.High:=a.High shr Shift;
    end;
   end;
  end;
  function Int128DivModPowerOfTwo(var a:TInt128;const Power:TPasDblStrUtilsInt32):TPasDblStrUtilsInt32;
  begin
   if Power>=64 then begin
    result:=a.High shr (Power-64);
    dec(a.High,result shl (Power-64));
   end else begin
    result:=(a.Low shr Power)+(a.High shl (64-Power));
    a.High:=0;
    dec(a.Low,(a.Low shr Power) shl Power);
   end;
  end;
  function Int128IsZero(const a:TInt128):TPasDblStrUtilsBoolean;
  begin
   result:=(a.High=0) and (a.Low=0);
  end;
  function Int128BitAt(const a:TInt128;const Position:TPasDblStrUtilsInt32):TPasDblStrUtilsBoolean;
  begin
   if Position>=64 then begin
    result:=((a.High shr (Position-64)) and 1)<>0;
   end else begin
    result:=((a.LOw shr Position) and 1)<>0;
   end;
  end;
  procedure FillDigits32FixedLength(Number:TPasDblStrUtilsUInt32;RequestedLength:TPasDblStrUtilsInt32;var Buffer:TPasDblStrUtilsString;var Len:TPasDblStrUtilsInt32);
  var i,l:TPasDblStrUtilsInt32;
  begin
   l:=Len;
   inc(Len,RequestedLength);
   if Len>=length(Buffer) then begin
    SetLength(Buffer,Len*2);
   end;
   for i:=RequestedLength downto 1 do begin
    Buffer[l+i]:=TPasDblStrUtilsChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))+(Number mod 10)));
    Number:=Number div 10;
   end;
  end;
  procedure FillDigits32(Number:TPasDblStrUtilsUInt32;var Buffer:TPasDblStrUtilsString;var Len:TPasDblStrUtilsInt32);
  var NumberLength,i,l:TPasDblStrUtilsInt32;
      OldNumber:TPasDblStrUtilsUInt32;
  begin
   OldNumber:=Number;
   NumberLength:=0;
   while Number<>0 do begin
    Number:=Number div 10;
    inc(NumberLength);
   end;
   if NumberLength<>0 then begin
    l:=Len;
    inc(Len,NumberLength);
    if Len>=length(Buffer) then begin
     SetLength(Buffer,Len*2);
    end;
    Number:=OldNumber;
    for i:=NumberLength downto 1 do begin
     Buffer[l+i]:=TPasDblStrUtilsChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))+(Number mod 10)));
     Number:=Number div 10;
    end;
   end;
  end;
  procedure FillDigits64FixedLength(Number:TPasDblStrUtilsUInt64;RequestedLength:TPasDblStrUtilsInt32;var Buffer:TPasDblStrUtilsString;var Len:TPasDblStrUtilsInt32);
  var p0,p1,p2:TPasDblStrUtilsUInt32;
  begin
   p2:=Number mod 10000000;
   Number:=Number div 10000000;
   p1:=Number mod 10000000;
   p0:=Number div 10000000;
   FillDigits32FixedLength(p0,3,Buffer,Len);
   FillDigits32FixedLength(p1,7,Buffer,Len);
   FillDigits32FixedLength(p2,7,Buffer,Len);
  end;
  procedure FillDigits64(Number:TPasDblStrUtilsUInt64;var Buffer:TPasDblStrUtilsString;var Len:TPasDblStrUtilsInt32);
  var p0,p1,p2:TPasDblStrUtilsUInt32;
  begin
   p2:=Number mod 10000000;
   Number:=Number div 10000000;
   p1:=Number mod 10000000;
   p0:=Number div 10000000;
   if p0<>0 then begin
    FillDigits32(p0,Buffer,Len);
    FillDigits32FixedLength(p1,7,Buffer,Len);
    FillDigits32FixedLength(p2,7,Buffer,Len);
   end else if p1<>0 then begin
    FillDigits32(p1,Buffer,Len);
    FillDigits32FixedLength(p2,7,Buffer,Len);
   end else begin
    FillDigits32(p2,Buffer,Len);
   end;
  end;
  procedure RoundUp(var Buffer:TPasDblStrUtilsString;var Len,DecimalPoint:TPasDblStrUtilsInt32);
  var i:TPasDblStrUtilsInt32;
  begin
   if Len=0 then begin
    Buffer:='1';
    Len:=1;
    DecimalPoint:=1;
   end else begin
    inc(Buffer[Len]);
    for i:=Len downto 2 do begin
     if ord(Buffer[i])<>(ord('0')+10) then begin
      exit;
     end;
     Buffer[i]:='0';
     inc(Buffer[i-1]);
    end;
    if ord(Buffer[1])=(ord('0')+10) then begin
     Buffer[1]:='1';
     inc(DecimalPoint);
    end;
   end;
  end;
  procedure FillFractionals(Fractionals:TPasDblStrUtilsUInt64;Exponent:TPasDblStrUtilsInt32;FractionalCount:TPasDblStrUtilsInt32;var Buffer:TPasDblStrUtilsString;var Len,DecimalPoint:TPasDblStrUtilsInt32);
  var Point,i,Digit:TPasDblStrUtilsInt32;
      Fractionals128:TInt128;
  begin
   Assert(((-128)<=Exponent) and (Exponent<=0));
   if (-Exponent)<=64 then begin
    Assert((Fractionals shr 56)=0);
    Point:=-Exponent;
    for i:=1 to FracitionalCount do begin
     Fractionals:=Fractionals*5;
     dec(Point);
     Digit:=Fractionals shr Point;
     inc(Len);
     if Len>=length(Buffer) then begin
      SetLength(Buffer,Len*2);
     end;
     Buffer[Len]:=TPasDblStrUtilsChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))+Digit));
     dec(Fractionals,TPasDblStrUtilsUInt64(Digit) shl Point);
    end;
    if ((Fractionals shr (Point-1)) and 1)<>0 then begin
     RoundUp(Buffer,Len,DecimalPoint);
    end;
   end else begin
    Assert((64<(-Exponent)) and ((-Exponent)<=128));
    Fractionals128.High:=Fractionals;
    Fractionals128.Low:=0;
    Int128Shift(Fractionals128,(-Exponent)-64);
    Point:=128;
    for i:=1 to FracitionalCount do begin
     if Int128IsZero(Fractionals128) then begin
      break;
     end;
     Int128Mul(Fractionals128,5);
     dec(Point);
     Digit:=Int128DivModPowerOfTwo(Fractionals128,Point);
     inc(Len);
     if Len>=length(Buffer) then begin
      SetLength(Buffer,Len*2);
     end;
     Buffer[Len]:=TPasDblStrUtilsChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))+Digit));
    end;
    if Int128BitAt(Fractionals128,Point-1) then begin
     RoundUp(Buffer,Len,DecimalPoint);
    end;
   end;
  end;
  procedure TrimZeros(var Buffer:TPasDblStrUtilsString;var Len,DecimalPoint:TPasDblStrUtilsInt32);
  var i:TPasDblStrUtilsInt32;
  begin
   while (Len>0) and (Buffer[Len]='0') do begin
    dec(Len);
   end;
   i:=0;
   while (i<Len) and (Buffer[i+1]='0') do begin
    inc(i);
   end;
   if i<>0 then begin
    Delete(Buffer,1,i);
    dec(Len,i);
    dec(DecimalPoint,i);
   end;
  end;
 var SignificantMantissa,Divisor,Dividend,Remainder,Integrals,Fractionals:TPasDblStrUtilsUInt64;
     Exponent,DivisorPower:TPasDblStrUtilsInt32;
     Quotient:TPasDblStrUtilsUInt32;
 begin
  result:=false;
  SplitDouble(Value,SignificantMantissa,Exponent);
  if (Exponent<=20) and (FracitionalCount<=20) then begin
   Len:=0;
   if (Exponent+53)>74 then begin
    Divisor:=Five17;
    DivisorPower:=17;
    Dividend:=SignificantMantissa;
    if Exponent>DivisorPower then begin
     Dividend:=Dividend shl (Exponent-DivisorPower);
     Quotient:=Dividend div Divisor;
     Remainder:=(Dividend mod Divisor) shl DivisorPower;
    end else begin
     Dividend:=Dividend shl (DivisorPower-Exponent);
     Quotient:=Dividend div Divisor;
     Remainder:=(Dividend mod Divisor) shl Exponent;
    end;
    FillDigits32(Quotient,Buffer,Len);
    FillDigits64FixedLength(Remainder,DivisorPower,Buffer,Len);
    DecimalPoint:=Len;
   end else if Exponent>=0 then begin
    SignificantMantissa:=SignificantMantissa shl Exponent;
    FillDigits64(SignificantMantissa,Buffer,Len);
    DecimalPoint:=Len;
   end else if Exponent>-53 then begin
    Integrals:=SignificantMantissa shr (-Exponent);
    Fractionals:=SignificantMantissa-(Integrals shl (-Exponent));
    if Integrals>$ffffffff then begin
     FillDigits64(Integrals,Buffer,Len);
    end else begin
     FillDigits32(Integrals,Buffer,Len);
    end;
    DecimalPoint:=Len;
    FillFractionals(Fractionals,Exponent,FracitionalCount,Buffer,Len,DecimalPoint);
   end else if Exponent<-128 then begin
    Assert(FracitionalCount>=20);
    Buffer:='';
    Len:=0;
    DecimalPoint:=-FracitionalCount;
   end else begin
    DecimalPoint:=0;
    FillFractionals(SignificantMantissa,Exponent,FracitionalCount,Buffer,Len,DecimalPoint);
   end;
   TrimZeros(Buffer,Len,DecimalPoint);
   SetLength(Buffer,Len);
   if Len=0 then begin
    DecimalPoint:=-FracitionalCount;
   end;
   result:=true;
  end;
 end;
var OK,Fast:TPasDblStrUtilsBoolean;
    Len,DecimalPoint,ZeroPrefixLength,ZeroPostfixLength,i:TPasDblStrUtilsInt32;
    LocalOutputMode:TPasDblStrUtilsOutputMode;
begin
 if IsNaN(AValue) then begin
  result:='NaN';
 end else if IsZero(AValue) then begin
  result:='0';
 end else if IsNegInfinite(AValue) then begin
  result:='-Infinity';
 end else if IsNegative(AValue) then begin
  result:='-'+ConvertDoubleToString(DoubleAbsolute(AValue),OutputMode,RequestedDigits);
 end else if IsInfinite(AValue) then begin
  result:='Infinity';
 end else begin
  result:='0';
  if AValue<>0 then begin
   Len:=0;
   DecimalPoint:=0;
   OK:=false;
   Fast:=false;
   if ((OutputMode=omFixed) and (AValue>=1e21)) or ((OutputMode=omRadix) and (RequestedDigits=10)) then begin
    LocalOutputMode:=omStandard;
   end else begin
    LocalOutputMode:=OutputMode;
   end;
   case LocalOutputMode of
    omStandard:begin
     result:=RyuDoubleToString(aValue,false);
     OK:=true;
    end;
    omStandardExponential:begin
     result:=RyuDoubleToString(aValue,true);
     OK:=true;
    end;
    else begin
    end;
   end;
   if not OK then begin
    case LocalOutputMode of
     omStandard,omStandardExponential:begin
      OK:=DoFastShortest(AValue,result,Len,DecimalPoint);
      inc(DecimalPoint,Len);
     end;
     omFixed:begin
      OK:=DoFastFixed(AValue,RequestedDigits,result,Len,DecimalPoint);
     end;
     omExponential,omPrecision:begin
      if RequestedDigits<=0 then begin
       OK:=DoFastShortest(AValue,result,Len,DecimalPoint);
       inc(DecimalPoint,Len);
       RequestedDigits:=Len-1;
      end else begin
       OK:=DoFastPrecision(AValue,RequestedDigits,result,Len,DecimalPoint);
       inc(DecimalPoint,Len);
      end;
      Assert((Len>0) and (Len<=(RequestedDigits+1)));
     end;
     omRadix:begin
      if ((RequestedDigits>=2) and (RequestedDigits<=36)) and (IsFinite(AValue) and (AValue<4294967295.0) and (System.Int(AValue)=AValue)) then begin
       FastDoubleToRadix(AValue,RequestedDigits,result,Len,DecimalPoint);
       Fast:=true;
       OK:=true;
      end;
     end;
    end;
    if not OK then begin
     case LocalOutputMode of
      omStandard,omStandardExponential:begin
       DoubleToDecimal(AValue,ModeShortest,RequestedDigits,result,Len,DecimalPoint);
       OK:=true;
      end;
      omFixed:begin
       DoubleToDecimal(AValue,ModeFixed,RequestedDigits,result,Len,DecimalPoint);
       OK:=true;
      end;
      omExponential,omPrecision:begin
       if RequestedDigits<=0 then begin
        DoubleToDecimal(AValue,ModeShortest,RequestedDigits,result,Len,DecimalPoint);
        OK:=true;
        RequestedDigits:=Len-1;
       end else begin
        DoubleToDecimal(AValue,ModePrecision,RequestedDigits,result,Len,DecimalPoint);
        OK:=true;
       end;
       Assert((Len>0) and (Len<=(RequestedDigits+1)));
      end;
      omRadix:begin
       if (RequestedDigits>=2) and (RequestedDigits<=36) then begin
        DoubleToRadix(AValue,RequestedDigits,result,Len,DecimalPoint);
        OK:=true;
       end;
      end;
     end;
    end;
    if OK then begin
     SetLength(result,Len);
     case LocalOutputMode of
      omStandard:begin
       if (Len<=DecimalPoint) and (DecimalPoint<=21) then begin
        SetLength(result,DecimalPoint);
        FillChar(result[Len+1],DecimalPoint-Len,'0');
       end else if (0<DecimalPoint) and (DecimalPoint<=21) then begin
        Insert('.',result,DecimalPoint+1);
       end else if (DecimalPoint<=0) and (DecimalPoint>-6) then begin
        for i:=1 to -DecimalPoint do begin
         result:='0'+result;
        end;
        result:='0.'+result;
       end else begin
        if Len<>1 then begin
         Insert('.',result,2);
        end;
        if DecimalPoint>=0 then begin
         result:=result+'e+'+TPasDblStrUtilsString(IntToStr(abs(DecimalPoint-1)));
        end else begin
         result:=result+'e-'+TPasDblStrUtilsString(IntToStr(abs(DecimalPoint-1)));
        end;
       end;
      end;
      omStandardExponential:begin
       if Len<>1 then begin
        Insert('.',result,2);
       end;
       if DecimalPoint>=0 then begin
        result:=result+'e+'+TPasDblStrUtilsString(IntToStr(abs(DecimalPoint-1)));
       end else begin
        result:=result+'e-'+TPasDblStrUtilsString(IntToStr(abs(DecimalPoint-1)));
       end;
      end;
      omFixed:begin
       ZeroPrefixLength:=0;
       ZeroPostfixLength:=0;
       if DecimalPoint<=0 then begin
        ZeroPrefixLength:=(-DecimalPoint)+1;
        DecimalPoint:=1;
       end;
       if (ZeroPrefixLength+Len)<(DecimalPoint+RequestedDigits) then begin
        ZeroPostfixLength:=((DecimalPoint+RequestedDigits)-Len)-ZeroPrefixLength;
       end;
       for i:=1 to ZeroPrefixLength do begin
        result:='0'+result;
       end;
       for i:=1 to ZeroPostfixLength do begin
        result:=result+'0';
       end;
       if (RequestedDigits>0) and (DecimalPoint>0) and (DecimalPoint<=length(result)) then begin
        Insert('.',result,DecimalPoint+1);
       end;
      end;
      omExponential:begin
       if RequestedDigits<1 then begin
        RequestedDigits:=1;
       end;
       if RequestedDigits<>1 then begin
        Insert('.',result,2);
        for i:=Len+1 to RequestedDigits do begin
         result:=result+'0';
        end;
       end else begin
        SetLength(result,1);
       end;
       if DecimalPoint>=0 then begin
        result:=result+'e+'+TPasDblStrUtilsString(IntToStr(abs(DecimalPoint-1)));
       end else begin
        result:=result+'e-'+TPasDblStrUtilsString(IntToStr(abs(DecimalPoint-1)));
       end;
      end;
      omPrecision:begin
       if RequestedDigits<1 then begin
        RequestedDigits:=1;
       end;
       if (DecimalPoint<-6) or (DecimalPoint>=RequestedDigits) then begin
        if RequestedDigits<>1 then begin
         Insert('.',result,2);
         for i:=Len+1 to RequestedDigits do begin
          result:=result+'0';
         end;
        end else begin
         SetLength(result,1);
        end;
        if DecimalPoint>=0 then begin
         result:=result+'e+'+TPasDblStrUtilsString(IntToStr(abs(DecimalPoint-1)));
        end else begin
         result:=result+'e-'+TPasDblStrUtilsString(IntToStr(abs(DecimalPoint-1)));
        end;
       end else begin
        if DecimalPoint<=0 then begin
         for i:=1 to -DecimalPoint do begin
          result:='0'+result;
         end;
         result:='0.'+result;
         for i:=Len+1 to RequestedDigits do begin
          result:=result+'0';
         end;
        end else begin
         SetLength(result,RequestedDigits);
         for i:=Len+1 to RequestedDigits do begin
          result[i]:='0';
         end;
         if DecimalPoint<RequestedDigits then begin
          if Len<>1 then begin
           Insert('.',result,DecimalPoint+1);
          end;
         end;
        end;
       end;
      end;
      omRadix:begin
       if not Fast then begin
        if (Len<=DecimalPoint) and (DecimalPoint<=21) then begin
         SetLength(result,DecimalPoint);
         FillChar(result[Len+1],DecimalPoint-Len,'0');
        end else if (0<DecimalPoint) and (DecimalPoint<=21) then begin
         Insert('.',result,DecimalPoint+1);
        end else if (DecimalPoint<=0) and (DecimalPoint>-6) then begin
         for i:=1 to -DecimalPoint do begin
          result:='0'+result;
         end;
         result:='0.'+result;
        end else begin
         if Len<>1 then begin
          Insert('.',result,2);
         end;
         if DecimalPoint>=0 then begin
          result:=result+'p+'+TPasDblStrUtilsString(IntToStr(abs(DecimalPoint-1)));
         end else begin
          result:=result+'p-'+TPasDblStrUtilsString(IntToStr(abs(DecimalPoint-1)));
         end;
        end;
        while (length(result)>1) and ((result[1]='0') and (result[2] in ['0'..'9','a'..'f'])) do begin
         Delete(result,1,1);
        end;
       end;
      end;
     end;
    end else begin
     result:='';
    end;
   end;
  end;
 end;
end;

initialization
finalization
end.
