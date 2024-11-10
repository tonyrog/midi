# midi

install libasound

    sudo apt install libasound2-dev

add group "audio" to your login user to be able
to open midi devices.

    sudo usermod -a -G audio $USER
