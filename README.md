# AK

AK protocol implementation. Mainly used for simulators but has a simple client as well.

## 1. Introduction

AK protocol used for communicating with certain types of devices. It is a very simple text
based protocol consisting of a fixed sized header followed by variable length data, framed with
the STX and ETX characters. 

Request format: `<STX> XXXX Knn ...<ETX>`
Reply format: `<STX> XXXX e ...<ETX>`
Where `XXXX` is a 4-character function code e.g. `ASTZ` (request status), `nn` is a channel number
typically `0`, `e` is a response status byte and `...` represents variable length request/response
data.

## 2. Usage

## 2.1 Client

Use the `CALL` method, passing a 4-character keyword command name and an integer channel number,
plus any number of command arguments.

```
(CONNECT 8000 #(127 0 0 1))
(CALL :ASTZ 0)
(CALL :AKON 0)
(DISCONNECT)
```

## 2.2 Server

Define a device simulator by subclassing `DEVICE` then define command handlers using `DEFCOMMAND`.

```
(defclass mydev (device)
  ())
(defcommand :akon ((d mydev) channel args)
  nil)
(defvar *d* (make-instance 'mydev))
(start-device *d*)
(stop-device *d*)
```

## 3. License
Licensed under the terms of the MIT license.

Frank James
September 2019.

