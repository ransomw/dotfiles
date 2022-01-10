media
=====

There's much to be said about viewing
files as bytestreams in the digital
age.
`Take <http://fqa.9front.org/fqa6.html#6.1>`_
the 9front fqa eg.

hexdump
-------

the "canonical"
(near ``emacs`` ``hexl-mode``)
hex-as-text format of any file (bytestream)
is extracted with

.. code:: shell

  hexdump -C $file > $dest


for example

.. code:: shell

  $ hexdump -C file1
  00000000  77 65 6c 63 6f 6d 65 0a              |welcome.|
  00000008

``ffmepg`` video
--------------

capture frames

.. code:: shell

  ffmpeg -i orig.mp4 -r 1/1 frames/frame%03d.bmp

clip interval

.. code:: shell

  ffmpeg -ss 00:00:25 -t 00:00:00.04 -i orig.mp4 clip.mp4

