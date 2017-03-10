# JDPower

The JD Power system is a collection of tools which assists network administrators monitor the power input of a UPS (battery) and automatically shut down all computers which are connected upon power loss. This library consists of various other tools related to power monitoring and management, but is primarily focused on computers using a UPS for backup power, in case of a power outage.

# How it Works

There are multiple pieces to this project group, but the most important is "JDPowerServer.exe" which is a Windows Service application to be installed on all computers plugged into a UPS. One of these computers will be considered the "Master" which is directly plugged into the UPS via USB, and this computer is the one which will keep track of the battery power.

In the event that the UPS loses external power, the following two events will occur:

1. Windows will detect a change from AC Power to DC Power
2. Windows will detect the percentage of battery remaining as it drops

What this service will in turn watch is if the battery drops below a certain percentage (20% default) *and* computer remains on DC power. When it hits this point, this computer will decide to not only shut itself down, but also send a shutdown command to all the other computers which are connected to the same UPS. 
