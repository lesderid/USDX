{* UltraStar Deluxe - Karaoke Game
 *
 * UltraStar Deluxe is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING. If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/media/UAudioInput_Portaudio.pas $
 * $Id: UAudioInput_Portaudio.pas 2665 2010-10-14 08:00:23Z k-m_schindler $
 *}

unit UAudioInput_Portaudio;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I ../switches.inc}

uses
  Classes,
  SysUtils,
  UMusic,
  URecord;

implementation

uses
  portaudio,
  ctypes,
  UAudioCore_Portaudio,
  UUnicodeUtils,
  UTextEncoding,
  UIni,
  ULog,
  UMain;

type
  TAudioInput_Portaudio = class(TAudioInputBase)
    private
      AudioCore: TAudioCore_Portaudio;
    public
      function GetName: string; override;
      function InitializeRecord: boolean; override;
      function FinalizeRecord: boolean; override;
  end;

{ TAudioInput_Portaudio }

function TAudioInput_Portaudio.GetName: String;
begin
  result := 'Portaudio';
end;

function TAudioInput_Portaudio.InitializeRecord(): boolean;
begin
  Result := False;
  AudioCore := TAudioCore_Portaudio.GetInstance();

  // Initialize Portaudio
  if not AudioCore.Initialize() then
    Exit;

  // Set self as recording handler in AudioCore
  AudioCore.SetRecordingHandler(Self);

  // Set audio format
  AudioFormat := TAudioFormatInfo.Create(
    1,        // Input channels
    44100,    // Sample rate
    asfS16    // Sample format
  );

  Result := True;
end;

function TAudioInput_Portaudio.FinalizeRecord: boolean;
begin
  AudioCore.SetRecordingHandler(nil);
  Result := AudioCore.Terminate();
end;

initialization
  MediaManager.add(TAudioInput_Portaudio.Create);

end.