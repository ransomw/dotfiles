FreeBSD Mic & Camera (Audio and Video)
======================================


To prepare kernel module
`kldload cuse`

Start webcamd
-------------

Then see potential camera devices
`ls /dev/ugen*`, and try one (1)
`# webcamd -d ugenX.Y` to get a list
of indexes to descriptive strings.

Start `webcamd` as before with corrected indexes
as well as zeroing the interface and video outputs.
For example
`webcamd -i 0 -v 0 -d ugen1.5`
noting that `/dev/ugen*` is _not_ correct.

This (`webcamd`) is the Linux stack ripped into userspace.

View webcam
-----------

`pwcview` is the FreeBSD-specific program
offering most control over the webcam.

`pwcview /dev/video0` for example
will being streaming the webcam.


However, in the interest of consolidating
functionality into fewer user-interfaces,
```
mpv av://v4l2:/dev/video0 --profile=low-latency --untimed
```
provides another starting-point for all
save the most webcam-specific operations
offered by pwcview (screen-shot, etc.).
This usage of the `av://` option replaces `tv://`,
removed from `mplayer` in `mpv`.
The type `v4l2` is Video 4 Linux,
the output format of `webcamd`.

**todo** `rawvideo` output type
_status_ errors with
```
[ffmpeg] IMGUTILS: Picture size 0x0 is invalid
[lavf] avformat_open_input() failed
```


capture mic
-----------

where `<idx>` in `[0-9]+`, not a combination `X.Y`
```
ffmpeg -thread_queue_size 1024 -f oss -i /dev/dsp<idx> out.m4a
```
and `<idx>` is obtained from
`cat /dev/sndstat`
and capturing the `pcm<idx>` labelled `(rec)`


**more complicated**
mixing can be setup using
`virtual_oss` to create a new `/dev/custom_dsp`

```
typeset -a virtual_oss_default_args
virtual_oss_default_args=(
  -T /dev/sndstat
  -S
  -i 1
  -C 2
  -c 2
  -r 48000
  -b 24
  -s 8.0ms
  -O /dev/dsp0
  -R /dev/dsp1
  -d custom_dsp
  -w custom_dsp.wav
  -t custom_dsp.ctl
)
```

```
typeset -a voss_rec_args
voss_rec_args=(
   -T /dev/sndstat
   -S
   -C 2
   -c 2
   -r 44100
   -b 16
   -s 1024
   -f /dev/dsp
   -d dsp_rec
   -w dsp_rec.wav
   -l dsp_rec.rec
)
```
