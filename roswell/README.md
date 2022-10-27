
## How to use Roswell to build and share binaries

From the project root:

Run as a script:

    chmod +x roswell/cc.ros
    ./roswell/cc.ros

Build a binary:

    ros build roswell/cc.ros

and run it:

    ./roswell/cc

Or install it in ~/.roswell/bin:

    ros install roswell/cc.ros

It creates the binary in ~/.roswell/bin/
Run it:

    ~/.roswell/bin/cc [name]~&

Your users can install the script with ros install lliaotonglang@gmail.com/cc

Use `+Q` if you don't have Quicklisp dependencies to save startup time.
Use `ros build --disable-compression` to save on startup time and loose on application size.


## See

- https://github.com/roswell/roswell/wiki/
- https://github.com/roswell/roswell/wiki/Reducing-Startup-Time
