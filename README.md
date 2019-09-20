# AK

AK protocol implementation. Mainly used for simulators but has a simple client as well.

## 1. Introduction

AK protocol used for communicating with certain types of devices.

## 2. Usage

## 2.1 Client

Use the `CALL` method, passing a 4-character keyword command name and an integer channel number,
plus any number of command arguments.

```
(CALL :ASTZ 0)
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

