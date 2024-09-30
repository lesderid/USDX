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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/media/UAudioCore_Portaudio.pas $
 * $Id: UAudioCore_Portaudio.pas 2475 2010-06-10 18:27:53Z brunzelchen $
 *}

unit UAudioCore_Portaudio;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I ../switches.inc}

uses
  Classes,
  SysUtils,
  portaudio,
  UAudioPlayback_SoftMixer,
  URecord;

type
  TAudioCore_Portaudio = class
    private
      InitCount: integer; ///< keeps track of the number of Initialize/Terminate calls
      SharedStream: PPaStream;
      FPlaybackHandler: TAudioPlayback_SoftMixer;
      FRecordingHandler: TAudioInputBase;
      FLatency: Double;
      function OpenSharedStream(): Boolean;
      function CloseSharedStream(): Boolean;
    public
      constructor Create();
      class function GetInstance(): TAudioCore_Portaudio;
      function Initialize(): boolean;
      function Terminate(): boolean;
      function GetPreferredApiIndex(): TPaHostApiIndex;
      function TestDevice(inParams, outParams: PPaStreamParameters; var sampleRate: double): boolean;
      procedure SetPlaybackHandler(handler: TAudioPlayback_SoftMixer);
      procedure SetRecordingHandler(handler: TAudioInputBase);
      function GetLatency(): Double;
      function StartStream(): Boolean;
      procedure StopStream();
  end;

implementation

uses
  ULog;

{*
 * The default API used by Portaudio is the least common denominator
 * and might lack efficiency. In addition it might not even work.
 * We use an array named ApiPreferenceOrder with which we define the order of
 * preferred APIs to use. The first API-type in the list is tried first.
 * If it is not available the next one is tried and so on ...
 * If none of the preferred APIs was found the default API (detected by
 * portaudio) is used.
 *
 * Pascal does not permit zero-length static arrays, so you must use paDefaultApi
 * as an array's only member if you do not have any preferences.
 * You can also append paDefaultApi to a non-zero length preferences array but
 * this is optional because the default API is always used as a fallback.
 *}
const
  paDefaultApi = -1;
const
  ApiPreferenceOrder:
{$IF Defined(MSWINDOWS)}
    array[0..2] of TPaHostApiTypeId = ( paASIO, paWASAPI, paDirectSound );
{$ELSEIF Defined(DARWIN)}
    array[0..0] of TPaHostApiTypeId = ( paDefaultApi ); // paCoreAudio
{$ELSEIF Defined(UNIX)}
    array[0..2] of TPaHostApiTypeId = ( paALSA, paJACK, paOSS );
{$ELSE}
    array[0..0] of TPaHostApiTypeId = ( paDefaultApi );
{$IFEND}
  
function PortaudioFullDuplexCallback(input: Pointer; output: Pointer; frameCount: LongWord;
  timeInfo: PPaStreamCallbackTimeInfo; statusFlags: TPaStreamCallbackFlags; this: TAudioCore_Portaudio): cint; cdecl; forward;

{ TAudioInput_Portaudio }

var
  Instance: TAudioCore_Portaudio;

constructor TAudioCore_Portaudio.Create();
begin
  inherited;
  InitCount := 0;
  SharedStream := nil;
  FLatency := 0;
end;

class function TAudioCore_Portaudio.GetInstance(): TAudioCore_Portaudio;
begin
  if not assigned(Instance) then
    Instance := TAudioCore_Portaudio.Create();
  Result := Instance;
end;

function TAudioCore_Portaudio.Initialize(): boolean;
var
  Err: TPaError;
begin
  // initialize only once
  if (InitCount > 0) then
  begin
    Inc(InitCount);
    Result := true;
    Exit;
  end;

  // init Portaudio
  Err := Pa_Initialize();
  if (Err <> paNoError) then
  begin
    Log.LogError(Pa_GetErrorText(Err), 'TAudioCore_Portaudio.Initialize');
    Result := false;
    Exit;
  end;

  // only increment on success
  Inc(InitCount);
  Result := true;
end;

function TAudioCore_Portaudio.Terminate(): boolean;
var
  Err: TPaError;
begin
  // decrement usage count
  Dec(InitCount);
  if (InitCount > 0) then
  begin
    // do not terminate yet
    Result := true;
    Exit;
  end;

  // terminate if usage count is 0
  Err := Pa_Terminate();
  if (Err <> paNoError) then
  begin
    Log.LogError(Pa_GetErrorText(Err), 'TAudioCore_Portaudio.Terminate');
    Result := false;
    Exit;
  end;

  Result := true;
end;

function TAudioCore_Portaudio.GetPreferredApiIndex(): TPaHostApiIndex;
var
  i:        integer;
  apiIndex: TPaHostApiIndex;
  apiInfo:  PPaHostApiInfo;
begin
  result := -1;

  // select preferred sound-API
  for i:= 0 to High(ApiPreferenceOrder) do
  begin
    if (ApiPreferenceOrder[i] <> paDefaultApi) then
    begin
      // check if API is available
      apiIndex := Pa_HostApiTypeIdToHostApiIndex(ApiPreferenceOrder[i]);
      if (apiIndex >= 0) then
      begin
        // we found an API but we must check if it works
        // (on linux portaudio might detect OSS but does not provide
        // any devices if ALSA is enabled)
        apiInfo := Pa_GetHostApiInfo(apiIndex);
        if (apiInfo^.deviceCount > 0) then
        begin
          Result := apiIndex;
          break;
        end;
      end;
    end;
  end;

  // None of the preferred APIs is available -> use default
  if (result < 0) then
  begin
    result := Pa_GetDefaultHostApi();
  end;
end;

{*
 * Portaudio test callback used by TestDevice().
 *}
function TestCallback(input: pointer; output: pointer; frameCount: longword;
      timeInfo: PPaStreamCallbackTimeInfo; statusFlags: TPaStreamCallbackFlags;
      inputDevice: pointer): integer; cdecl;
begin
  // this callback is called only once
  result := paAbort;
end;

(*
 * Tests if the callback works. Some devices can be opened without
 * an error but the callback is never called. Calling Pa_StopStream() on such
 * a stream freezes USDX then. Probably because the callback-thread is deadlocked
 * due to some bug in portaudio. The blocking Pa_ReadStream() and Pa_WriteStream()
 * block forever too and though can't be used for testing.
 *
 * To avoid freezing Pa_AbortStream (or Pa_CloseStream which calls Pa_AbortStream)
 * can be used to force the stream to stop. But for some reason this stops debugging
 * in gdb with a "no process found" message.
 *
 * Because freezing devices are non-working devices we test the devices here to
 * be able to exclude them from the device-selection list.
 *
 * Portaudio does not provide any test to check this error case (probably because
 * it should not even occur). So we have to open the device, start the stream and
 * check if the callback is called (the stream is stopped if the callback is called
 * for the first time, so we can poll until the stream is stopped).
 *
 * Another error that occurs is that some devices (even the default device) might
 * work at the beginning but stop after a few calls (maybe 50) of the callback.
 * For me this problem occurs with the default output-device. The "dmix" or "front"
 * device must be selected instead. Another problem is that (due to a bug in
 * portaudio or ALSA) the "front" device is not detected every time portaudio
 * is started. Sometimes it needs two or more restarts.
 *
 * There is no reasonable way to test for these errors. For the first error-case
 * we could test if the callback is called 50 times but this can take a second
 * for each device and it can fail in the 51st or even 100th callback call then.
 *
 * The second error-case cannot be tested at all. How should we now that one
 * device is missing if portaudio is not even able to detect it.
 * We could start and terminate Portaudio for several times and see if the device
 * count changes but this is ugly.
 *
 * Conclusion: We are not able to autodetect a working device with
 *   portaudio (at least not with the newest v19_20071207) at the moment.
 *   So we have to provide the possibility to manually select an output device
 *   in the UltraStar options if we want to use portaudio instead of SDL.
 *)
function TAudioCore_Portaudio.TestDevice(inParams, outParams: PPaStreamParameters; var sampleRate: double): boolean;
const
  altSampleRates: array[0..1] of double = (44100, 48000); // alternative sample-rates
var
  stream:  PPaStream;
  err:     TPaError;
  cbWorks: boolean;
  cbPolls: integer;
  i:       integer;
begin
  Result := false;

  if (sampleRate <= 0) then
    sampleRate := 44100;

  // check if device supports our input-format
  err := Pa_IsFormatSupported(inParams, outParams, sampleRate);
  if (err <> paNoError) then
  begin
    // we cannot fix the error -> exit
    if (err <> paInvalidSampleRate) then
    begin
      Log.LogError('Error checking format: ' + Pa_GetErrorText(err), 'TAudioCore_Portaudio.TestDevice');
      Exit;
    end;

    // try alternative sample-rates to the detected one
    sampleRate := 0;
    for i := 0 to High(altSampleRates) do
    begin
      // do not check the detected sample-rate twice
      if (altSampleRates[i] = sampleRate) then
        continue;
      // check alternative
      err := Pa_IsFormatSupported(inParams, outParams, altSampleRates[i]);
      if (err = paNoError) then
      begin
        // sample-rate works
        sampleRate := altSampleRates[i];
        break;
      end;
    end;
    // no working sample-rate found
    if (sampleRate = 0) then
    begin
      Log.LogError('Error checking format: ' + Pa_GetErrorText(err), 'TAudioCore_Portaudio.TestDevice');
      Exit;
    end;
  end;

  // FIXME: for some reason gdb stops after a call of Pa_AbortStream()
  // which is implicitely called by Pa_CloseStream().
  // gdb's stops with the message: "ptrace: no process found".
  // Probably because the callback-thread is killed what confuses gdb.
  {$IF Defined(Debug) and Defined(Linux)}
  cbWorks := true;
  {$ELSE}
  // open device for testing
  err := Pa_OpenStream(stream, inParams, outParams, sampleRate,
          paFramesPerBufferUnspecified,
          paNoFlag, @TestCallback, nil);
  if (err <> paNoError) then
  begin
    Log.LogError('Error opening stream: ' + Pa_GetErrorText(err), 'TAudioCore_Portaudio.TestDevice');
    exit;
  end;

  // start the callback
  err := Pa_StartStream(stream);
  if (err <> paNoError) then
  begin
    Log.LogError('Error starting stream: ' + Pa_GetErrorText(err), 'TAudioCore_Portaudio.TestDevice');
    Pa_CloseStream(stream);
    exit;
  end;

  cbWorks := false;
  // check if the callback was called (poll for max. 1500ms)
  for cbPolls := 1 to 150 do
  begin
    // if the test-callback was called it should be aborted now
    if (Pa_IsStreamActive(stream) = 0) then
    begin
      cbWorks := true;
      break;
    end;
    // not yet aborted, wait and try (poll) again
    Pa_Sleep(10);
  end;

  if cbWorks = false then
    Log.LogError('Error: Audio callback is not called', 'TAudioCore_Portaudio.TestDevice');

  // finally abort the stream
  Pa_CloseStream(stream);
  {$IFEND}

  Result := cbWorks;
end;

function TAudioCore_Portaudio.OpenSharedStream(): Boolean;
var
  Err: TPaError;
  InParams, OutParams: TPaStreamParameters;
  SampleRate: Double;
  PaApiIndex: TPaHostApiIndex;
  PaApiInfo: PPaHostApiInfo;
  DeviceInfo: PPaDeviceInfo;
begin
  Result := False;

  PaApiIndex := GetPreferredApiIndex();
  if PaApiIndex = -1 then
  begin
    Log.LogError('No working Audio-API found', 'TAudioCore_Portaudio.OpenSharedStream');
    Exit;
  end;

  PaApiInfo := Pa_GetHostApiInfo(PaApiIndex);

  // Set up input parameters
  InParams.device := PaApiInfo^.defaultInputDevice;
  InParams.channelCount := 1;
  InParams.sampleFormat := paInt16;
  DeviceInfo := Pa_GetDeviceInfo(InParams.device);
  InParams.suggestedLatency := DeviceInfo^.defaultLowInputLatency;
  InParams.hostApiSpecificStreamInfo := nil;

  // Set up output parameters
  OutParams.device := PaApiInfo^.defaultOutputDevice;
  OutParams.channelCount := 2;
  OutParams.sampleFormat := paInt16;
  DeviceInfo := Pa_GetDeviceInfo(OutParams.device);
  OutParams.suggestedLatency := DeviceInfo^.defaultLowOutputLatency;
  OutParams.hostApiSpecificStreamInfo := nil;

  // Use default sample rate
  SampleRate := 44100;

  // Open full-duplex stream
  Err := Pa_OpenStream(@SharedStream, @InParams, @OutParams, SampleRate,
    paFramesPerBufferUnspecified, paNoFlag, @PortaudioFullDuplexCallback, pointer(Self));

  if Err <> paNoError then
  begin
    Log.LogError('Error opening shared stream: ' + Pa_GetErrorText(Err), 'TAudioCore_Portaudio.OpenSharedStream');
    SharedStream := nil;
    Exit;
  end;

  Result := True;
end;

function TAudioCore_Portaudio.CloseSharedStream(): Boolean;
var
  Err: TPaError;
begin
  Result := False;
  if SharedStream = nil then
  begin
    Result := True;
    Exit;
  end;

  Err := Pa_CloseStream(SharedStream);
  if Err <> paNoError then
  begin
    Log.LogError('Error closing shared stream: ' + Pa_GetErrorText(Err), 'TAudioCore_Portaudio.CloseSharedStream');
    Exit;
  end;

  SharedStream := nil;
  Result := True;
end;

function PortaudioFullDuplexCallback(input: Pointer; output: Pointer; frameCount: LongWord;
  timeInfo: PPaStreamCallbackTimeInfo; statusFlags: TPaStreamCallbackFlags; this: TAudioCore_Portaudio): cint; cdecl;
begin
  // Update latency
  this.FLatency := timeInfo^.outputBufferDacTime - timeInfo^.currentTime;

  // Handle recording
  if Assigned(this.FRecordingHandler) then
  begin
    AudioInputProcessor.HandleMicrophoneData(input, frameCount * this.FRecordingHandler.AudioFormat.FrameSize, this.FRecordingHandler);
  end;

  // Handle playback
  if Assigned(this.FPlaybackHandler) then
  begin
    this.FPlaybackHandler.AudioCallback(output, frameCount * this.FPlaybackHandler.FormatInfo.FrameSize);
  end
  else
  begin
    // If no playback handler, fill output with silence
    FillChar(output^, frameCount * 2 * SizeOf(SmallInt), 0);
  end;

  Result := paContinue;
end;

procedure TAudioCore_Portaudio.SetPlaybackHandler(handler: TAudioPlayback_SoftMixer);
begin
  FPlaybackHandler := handler;
end;

procedure TAudioCore_Portaudio.SetRecordingHandler(handler: TAudioInputBase);
begin
  FRecordingHandler := handler;
end;

function TAudioCore_Portaudio.GetLatency(): Double;
begin
  Result := FLatency;
end;

function TAudioCore_Portaudio.StartStream(): Boolean;
var
  Err: TPaError;
begin
  Result := False;
  if SharedStream = nil then
  begin
    if not OpenSharedStream() then
      Exit;
  end;

  Err := Pa_StartStream(SharedStream);
  if Err <> paNoError then
  begin
    Log.LogError('Error starting shared stream: ' + Pa_GetErrorText(Err), 'TAudioCore_Portaudio.StartStream');
    Exit;
  end;

  Result := True;
end;

procedure TAudioCore_Portaudio.StopStream();
var
  Err: TPaError;
begin
  if SharedStream <> nil then
  begin
    Err := Pa_StopStream(SharedStream);
    if Err <> paNoError then
    begin
      Log.LogError('Error stopping shared stream: ' + Pa_GetErrorText(Err), 'TAudioCore_Portaudio.StopStream');
    end;
    CloseSharedStream();
  end;
end;

end.
