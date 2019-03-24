## Version 0.1.1.0

* Keep silence going until end-of-track marker
* After quantization, move NoteOffs before NoteOns to conserve voices
* When voice limit exceeded, steal voices from sustained notes
* Use MUSIC SPEED to better handle triplets or unquantized music
* Use MUSIC VOLUME based on NoteOn velocity
* Clean up code
* Support changes in tempo or time signature in the middle of music
