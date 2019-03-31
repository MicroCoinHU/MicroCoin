unit UChunk;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

{ 
  Copyright (c) Albert Molina 2016 - 2018 original code from PascalCoin https://pascalcoin.org/

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.   

  If you like it, consider a donation using BitCoin:
    16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }


interface

uses
  Classes, SysUtils,  ZLib, {$IFDEF FPC} zStream, {$ENDIF}
  ULog, MicroCoin.Common.Config, UCrypto, MicroCoin.Account.AccountKey, MicroCoin.Account.Storage;

type

  { TPCChunk }

  TPCChunk = Class
  private
  public
    class function SaveChunkFromAccountStorage(AAccountStorageStream, ADestinationStream : TStream; AFromBlock, AToBlock : Cardinal; var RErrors : AnsiString) : Boolean;
    class function LoadAccountStorageFromChunk(AChunk, ADestinationStream : TStream; var AAccountStorageHeader : TAccountStorageHeader; var RErrors : AnsiString) : Boolean;
  end;

implementation

uses MicroCoin.Common.Stream;

{ TPCChunk }

class function TPCChunk.SaveChunkFromAccountStorage(AAccountStorageStream, ADestinationStream : TStream; AFromBlock, AToBlock: Cardinal; var RErrors : AnsiString) : Boolean;
Var
  c: Cardinal;
  cs : Tcompressionstream;
  auxStream : TStream;
  iPosSize, iAux : Int64;
  initialSbPos : Int64;
  sbHeader : TAccountStorageHeader;
begin
  Result := false; RErrors := '';
  // Chunk struct:
  // - Header:
  //   - Magic value  (fixed AnsiString)
  //   - SafeBox version (2 bytes)
  //   - Uncompressed size (4 bytes)
  //   - Compressed size (4 bytes)
  // - Data:
  //   - Compressed data using ZLib
  initialSbPos :=AAccountStorageStream.Position;
  try
     try
      sbHeader := TAccountStorageHeader.LoadFromStream(AAccountStorageStream);
     except on e:Exception
     do begin
        RErrors := 'SafeBoxStream is not a valid SafeBox!';
        exit;
      end;
     end;
    if (sbHeader.StartBlock>AFromBlock) Or (sbHeader.EndBlock<AToBlock) Or (AFromBlock>AToBlock)
    then begin
      RErrors := Format('Cannot save a chunk from %d to %d on a stream with %d to %d!',[AFromBlock,AToBlock,sbHeader.StartBlock,sbHeader.EndBlock]);
      exit;
    end;
    LogDebug(ClassName,Format('Saving safebox chunk from %d to %d (current blockscount: %d)',[AFromBlock,AToBlock,sbHeader.BlocksCount]));

    // Header:
    ADestinationStream.WriteAnsiString(CT_AccountChunkIdentificator);
    ADestinationStream.Write(cAccountStorageVersion,SizeOf(cAccountStorageVersion));
    //
    auxStream := TMemoryStream.Create;
    try
      AAccountStorageStream.Position:=initialSbPos;
      if not TAccountStorage.CopyChunk(AAccountStorageStream,auxStream,AFromBlock,AToBlock,RErrors) then exit;
      auxStream.Position:=0;
      // Save uncompressed size
      c := auxStream.Size;
      ADestinationStream.Write(c,SizeOf(c));
      // Save compressed size ... later
      iPosSize := ADestinationStream.Position;
      c := $FFFFFFFF;
      ADestinationStream.Write(c,SizeOf(c)); // Save 4 random bytes, latter will be changed
      //
      // Zip it and add to Stream
      cs := TCompressionstream.create(cldefault,ADestinationStream);
      try
        cs.CopyFrom(auxStream,auxStream.Size); // compressing
      finally
        cs.Free;
      end;
    finally
      auxStream.Free;
    end;
    //
    iAux := ADestinationStream.Position;
    c := ADestinationStream.Position - iPosSize - 4; // Save data size
    ADestinationStream.Position:=iPosSize;
    ADestinationStream.Write(c,SizeOf(c));
    ADestinationStream.Position := iAux; // Back to last position
    Result := True; RErrors := '';
  finally
    AAccountStorageStream.Position:=initialSbPos;
  end;
end;

class function TPCChunk.LoadAccountStorageFromChunk(AChunk, ADestinationStream: TStream; var AAccountStorageHeader : TAccountStorageHeader; var RErrors: AnsiString): Boolean;
var s : AnsiString;
  w : Word;
  cUncompressed, cCompressed : Cardinal;
  ds : Tdecompressionstream;
  dbuff : array[1..2048] of byte;
  r : integer;
  destInitialPos, auxPos : Int64;
begin
  Result := false;
  AAccountStorageHeader := TAccountStorageHeader.Empty;
  // Header:
  RErrors := 'Invalid stream header';
  AChunk.ReadAnsiString(s);
  If (s<>CT_AccountChunkIdentificator)
  then exit;
  AChunk.Read(w,sizeof(w));
  if (w<>cAccountStorageVersion)
  then begin
    RErrors := RErrors + ' Invalid version '+IntToStr(w);
    exit;
  end;
  // Size
  AChunk.Read(cUncompressed,SizeOf(cUncompressed)); // Uncompressed size
  AChunk.Read(cCompressed,SizeOf(cCompressed)); // Compressed size
  if (AChunk.Size - AChunk.Position < cCompressed)
  then begin
    RErrors := Format('Not enough LZip bytes Stream.size:%d Stream.position:%d (avail %d) LZipSize:%d',[AChunk.Size,AChunk.Position,AChunk.Size - AChunk.Position,cCompressed]);
    exit;
  end;
  //
  destInitialPos:=ADestinationStream.Position;
  ds := Tdecompressionstream.create(AChunk);
  try
    repeat
      r := ds.read(dbuff,SizeOf(dbuff));
      if (r>0)
      then ADestinationStream.Write(dbuff,r);
    until r < SizeOf(dbuff);
  finally
    ds.Free;
  end;

  if (ADestinationStream.Size-destInitialPos)<>cUncompressed
  then begin
    RErrors := Format('Uncompressed size:%d <> saved:%d',[(ADestinationStream.Size-destInitialPos),cUncompressed]);
    exit;
  end;

  auxPos := ADestinationStream.Position;
  ADestinationStream.Position:=destInitialPos;
  try
    AAccountStorageHeader := TAccountStorageHeader.LoadFromStream(ADestinationStream);
  except on e:Exception do begin
      RErrors:= 'Invalid extracted stream!';
      exit;
    end;
  end;
  ADestinationStream.Position:=auxPos;
  Result := true;
end;

end.

