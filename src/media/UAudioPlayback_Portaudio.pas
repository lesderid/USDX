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
      paStream:  PPaStream;
      AudioCore: TAudioCore_Portaudio;
      Latency: double;
      function OpenDevice(deviceIndex: TPaDeviceIndex): boolean;
      function EnumDevices(): boolean;
    protected
      function InitializeAudioPlaybackEngine(): boolean; override;
      function StartAudioPlaybackEngine(): boolean;      override;
      procedure StopAudioPlaybackEngine();               override;
      function FinalizeAudioPlaybackEngine(): boolean;   override;
      function GetLatency(): double;                     override;
    public
      function GetName: String;                          override;
  end;

  TPortaudioOutputDevice = class(TAudioOutputDevice)
    private
      PaDeviceIndex:  TPaDeviceIndex;
  end;


{ TAudioPlayback_Portaudio }

function PortaudioAudioCallback(input: Pointer; output: Pointer; frameCount: Longword;
    timeInfo: PPaStreamCallbackTimeInfo; statusFlags: TPaStreamCallbackFlags;
    userData: Pointer): Integer; cdecl;
var
  Engine: TAudioPlayback_Portaudio;
begin
  Engine := TAudioPlayback_Portaudio(userData);
  // update latency
  Engine.Latency := timeInfo.outputBufferDacTime - timeInfo.currentTime;
  // call superclass callback
  Engine.AudioCallback(output, frameCount * Engine.FormatInfo.FrameSize);
  Result := paContinue;
end;

function TAudioPlayback_Portaudio.GetName: String;
begin
  Result := 'Portaudio_Playback';
end;

function TAudioPlayback_Portaudio.OpenDevice(deviceIndex: TPaDeviceIndex): boolean;
var
  DeviceInfo : PPaDeviceInfo;
  SampleRate : double;
  OutParams  : TPaStreamParameters;
  StreamInfo : PPaStreamInfo;
  err        : TPaError;
begin
  Result := false;

  DeviceInfo := Pa_GetDeviceInfo(deviceIndex);

  Log.LogInfo('Audio-Output Device: ' + DeviceInfo^.name, 'TAudioPlayback_Portaudio.OpenDevice');

  SampleRate := DeviceInfo^.defaultSampleRate;

  with OutParams do
  begin
    device := deviceIndex;
    channelCount := 2;
    sampleFormat := paInt16;
    suggestedLatency := DeviceInfo^.defaultLowOutputLatency;
    hostApiSpecificStreamInfo := nil;
  end;

  // check souncard and adjust sample-rate
  if not AudioCore.TestDevice(nil, @OutParams, SampleRate) then
  begin
    Log.LogStatus('TestDevice failed!', 'TAudioPlayback_Portaudio.OpenDevice');
    Exit;
  end;

  // open output stream
  err := Pa_OpenStream(paStream, nil, @OutParams, SampleRate,
          paFramesPerBufferUnspecified,
          paNoFlag, @PortaudioAudioCallback, Self);
  if(err <> paNoError) then
  begin
    Log.LogStatus(Pa_GetErrorText(err), 'TAudioPlayback_Portaudio.OpenDevice');
    paStream := nil;
    Exit;
  end;

  // get estimated latency (will be updated with real latency in the callback)
  StreamInfo := Pa_GetStreamInfo(paStream);
  if (StreamInfo <> nil) then
    Latency := StreamInfo^.outputLatency
  else
    Latency := 0;

  FormatInfo := TAudioFormatInfo.Create(
    OutParams.channelCount,
    SampleRate,
    asfS16 // FIXME: is paInt16 system-dependant or -independant?
  );

  Result := true;
end;

function TAudioPlayback_Portaudio.EnumDevices(): boolean;
var
  i:           integer;
  paApiIndex:  TPaHostApiIndex;
  paApiInfo:   PPaHostApiInfo;
  deviceName:  string;
  deviceIndex: TPaDeviceIndex;
  deviceInfo:  PPaDeviceInfo;
  channelCnt:  integer;
  SC:          integer; // soundcard
  err:         TPaError;
  errMsg:      string;
  paDevice:    TPortaudioOutputDevice;
  outputParams: TPaStreamParameters;
  stream:      PPaStream;
  streamInfo:  PPaStreamInfo;
  sampleRate:  double;
  latency:     TPaTime;
  cbPolls: integer;
  cbWorks: boolean;
begin
  Result := true;
end;

function TAudioPlayback_Portaudio.InitializeAudioPlaybackEngine(): boolean;
var
  paApiIndex      : TPaHostApiIndex;
  paApiInfo       : PPaHostApiInfo;
  paOutDevice     : TPaDeviceIndex;
begin
  Result := false;
  AudioCore := TAudioCore_Portaudio.GetInstance();

  // initialize portaudio
  if (not AudioCore.Initialize()) then
    Exit;

  paApiIndex := AudioCore.GetPreferredApiIndex();
  if (paApiIndex = -1) then
  begin
    Log.LogError('No working Audio-API found', 'TAudioPlayback_Portaudio.InitializeAudioPlaybackEngine');
    Exit;
  end;

  EnumDevices();

  paApiInfo := Pa_GetHostApiInfo(paApiIndex);
  Log.LogInfo('Audio-Output API-Type: ' + paApiInfo^.name, 'TAudioPlayback_Portaudio.OpenDevice');

  paOutDevice := paApiInfo^.defaultOutputDevice;
  if (not OpenDevice(paOutDevice)) then
  begin
    Exit;
  end;

  Result := true;
end;

function TAudioPlayback_Portaudio.StartAudioPlaybackEngine(): boolean;
var
  err: TPaError;
begin
  Result := false;

  if (paStream = nil) then
    Exit;

  err := Pa_StartStream(paStream);
  if(err <> paNoError) then
  begin
    Log.LogStatus('Pa_StartStream: '+Pa_GetErrorText(err), 'UAudioPlayback_Portaudio');
    Exit;
  end;

  Result := true;
end;

procedure TAudioPlayback_Portaudio.StopAudioPlaybackEngine();
begin
  if (paStream <> nil) then
  begin
    Pa_CloseStream(paStream);
    // wait until stream is closed, otherwise Terminate() might cause a segfault
    while (Pa_IsStreamActive(paStream) = 1) do
      ;
    paStream := nil;
  end;
end;

function TAudioPlayback_Portaudio.FinalizeAudioPlaybackEngine(): boolean;
begin
  StopAudioPlaybackEngine();
  Result := true;
  if assigned(AudioCore) then
     Result := AudioCore.Terminate();
end;

function TAudioPlayback_Portaudio.GetLatency(): double;
begin
  Result := Latency;
end;


initialization
  MediaManager.Add(TAudioPlayback_Portaudio.Create);

end.
