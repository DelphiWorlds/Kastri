# AudioPlayer demo

## Description

This demonstrates the use of [`TAudioPlayer`](https://github.com/DelphiWorlds/Kastri/tree/master/Features/AudioPlayer).

## Supported Delphi versions

Delphi 12, Delphi 11.x. It _should_ also work in Delphi 10.4.2, and perhaps earlier.

## Usage

Call `LoadFromFile` to set the filename - this method also prepares the media for playing, on the relevant platforms.

Other methods (`Play`, `Pause`, `Stop`, `SeekTo`) should be self-explanatory

The OnAudioState event indicates one of the following:

* `Ready`: Audio has been preprared for playing
* `PlayStart`: Play has been called, but the audio has not necessarily started playing
* `Playing`: Audio has actually started to play (may be a slight delay after Play is called)
* `Stopped`: Audio has finished playing

The `PlayStart` can be useful if there is a delay between calling `Play` and when the audio actually starts emitting, especially when attempting to syncnronize audio with video (separate to the audio). It has been found on Android, that there is a significant delay (sometimes in the order of 300-400ms) before the audio starts emitting.

The demo was designed to illustrate how multiple audio files can be playing at once.
