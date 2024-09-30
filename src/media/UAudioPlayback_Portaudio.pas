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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/media/UAudioPlayback_Portaudio.pas $
 * $Id: UAudioPlayback_Portaudio.pas 2475 2010-06-10 18:27:53Z brunzelchen $
 *}

unit UAudioPlayback_Portaudio;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  Classes,
  SysUtils,
  UMusic;

implementation

uses
  portaudio,
  UAudioCore_Portaudio,
  UAudioPlayback_SoftMixer,
  ULog,
  UIni,
  UMain;

type
  TAudioPlayback_Portaudio = class(TAudioPlayback_SoftMixer)
  private
    AudioCore: TAudioCore_Portaudio;
  protected
    function InitializeAudioPlaybackEngine(): Boolean; override;
    function StartAudioPlaybackEngine(): Boolean; override;
    procedure StopAudioPlaybackEngine(); override;
    function FinalizeAudioPlaybackEngine(): Boolean; override;
    function GetLatency(): Double; override;
  public
    function GetName: String; override;
  end;

{ TAudioPlayback_Portaudio }

function TAudioPlayback_Portaudio.GetName: String;
begin
  Result := 'Portaudio_Playback';
end;

function TAudioPlayback_Portaudio.InitializeAudioPlaybackEngine(): boolean;
begin
  Result := False;
  AudioCore := TAudioCore_Portaudio.GetInstance();

  // Initialize Portaudio
  if not AudioCore.Initialize() then
    Exit;

  // Set self as playback handler in AudioCore
  AudioCore.SetPlaybackHandler(Self);

  // Set format info
  FormatInfo := TAudioFormatInfo.Create(
    2,        // Output channels
    44100,    // Sample rate
    asfS16    // Sample format
  );

  Result := True;
end;

function TAudioPlayback_Portaudio.StartAudioPlaybackEngine(): boolean;
begin
  Result := AudioCore.StartStream();
end;

procedure TAudioPlayback_Portaudio.StopAudioPlaybackEngine();
begin
  AudioCore.StopStream();
end;

function TAudioPlayback_Portaudio.FinalizeAudioPlaybackEngine(): boolean;
begin
  AudioCore.SetPlaybackHandler(nil);
  Result := AudioCore.Terminate();
end;

function TAudioPlayback_Portaudio.GetLatency(): double;
begin
  Result := AudioCore.GetLatency();
end;

initialization
  MediaManager.Add(TAudioPlayback_Portaudio.Create);

end.