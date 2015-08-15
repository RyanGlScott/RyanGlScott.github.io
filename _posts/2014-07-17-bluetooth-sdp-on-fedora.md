---
layout: post
title: Bluetooth SDP registration issues on Fedora
---

Recently, I've been messing around a lot with Linux's Bluetooth libraries [BlueZ](http://www.bluez.org/). It's pretty much a breeze to use on Ubuntu, but on Fedora, I noticed some strange issues. I was able to pair devices with my computer just fine, but whenever I tried to do anything involving the [service discovery protocol](http://en.wikipedia.org/wiki/List_of_Bluetooth_protocols#Service_discovery_protocol_.28SDP.29) (SDP), I would experience problems.

As an example, the command `sdptool browse local` will find all Bluetooth services registered on your machine. If I ran it, I got the error:

    Failed to connect to SDP server on FF:FF:FF:00:00:00: No such file or directory

Huh? How come this works on Ubuntu but not Fedora?

As it turns out, the culprit is `bluetoothd`, the Bluetooth daemon. Using SDP with `bluetoothd` requires deprecated features for some silly reason, so to fix this, the daemon must be started with `bluetoothd -C` (or `bluetooth --compat`).

To have Fedora automatically do this on startup, first make sure Bluetooth is off by running `service bluetooth stop`. Then edit `/usr/lib/systemd/system/bluetooth.service` and look for `ExecStart=/usr/libexec/bluetooth/bluetoothd`. Put ` -C` at the end of this line, save, and then run `service bluetooth start`.

If all goes well, you should be able to run `sdptool browse local` successfully.

> You may need root access depending on what you're doing with the SDP. Personally, I use `sudo` liberally with Bluetooth stuff on Fedora just to be safe.)
