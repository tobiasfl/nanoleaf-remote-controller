# nanoleaf-remote-controller
This CLI tool lets you control your nanoleafs from you laptop when on the same wifi. I mostly made this for myself so the code only supports the version of the api that my Nanoleaf Shapes (triangles) kit used. 
I have sold my nanoleafs, leaving this project in its current unfinished state. 
## Dependencies
- avahi-browse command needed for MDNS (Finding the Nanoleafs)
### Audio based effect streaming dependencies
I fiddled around with sending UDP to the nanoleafs to make them react to e.g. music, these are needed for that functionality. 
- ALSA (Advanced Linux Sound Architecture)
- libasound2-dev package to set up ALSA sequencer port
