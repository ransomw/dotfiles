package main

import (
	"fmt"
	"image"
	"image/draw"
	"io/ioutil"
	"os"
	"sync"

	"github.com/BurntSushi/xgb"
	mshm "github.com/BurntSushi/xgb/shm"
	"github.com/BurntSushi/xgb/xproto"
	"github.com/BurntSushi/xgbutil"
	"github.com/BurntSushi/xgbutil/ewmh"
	"github.com/BurntSushi/xgbutil/keybind"
	"github.com/BurntSushi/xgbutil/mousebind"
	"github.com/BurntSushi/xgbutil/xevent"
	"github.com/BurntSushi/xgbutil/xgraphics"
	"github.com/BurntSushi/xgbutil/xwindow"
	"github.com/gen2brain/shm"
)

const (
	none = 1 << iota
	loaded
	scaled
	drawn
)

type evInputX11 interface{}

type evMouseInputX11 struct{
	xu *xgbutil.XUtil
	e xevent.ButtonReleaseEvent
}
type evKeyInputX11 struct{
	xu *xgbutil.XUtil
	e xevent.KeyPressEvent
}

var (
	numImages int
	idx int // current image
	state int // ???
	X *xgbutil.XUtil
	win *xwindow.Window
)

func cbMouseInputX11(ev evMouseInputX11, ctl controlX11Input) {
	e := ev.e
		if e.Detail == 1 {
			if idx != numImages-1 {
				idx += 1
				state &= loaded
				state &= drawn
				ctl.update()
			}
		} else if e.Detail == 3 {
			if idx != 0 {
				idx -= 1
				state &= loaded
				state &= drawn
				ctl.update()
			}
		}
}

func cbKeyInputX11(ev evKeyInputX11, ctl controlX11Input) {
	var err error
	xu := ev.xu
	e := ev.e
		if keybind.KeyMatch(X, "Escape", e.State, e.Detail) || keybind.KeyMatch(X, "q", e.State, e.Detail) {
			xevent.Quit(X)
		}

		if keybind.KeyMatch(xu, "Left", e.State, e.Detail) || keybind.KeyMatch(xu, "Page_Up", e.State, e.Detail) || keybind.KeyMatch(xu, "k", e.State, e.Detail) {
			if idx != 0 {
				idx -= 1
				state &= loaded
				state &= drawn
				ctl.update()
			}
		} else if keybind.KeyMatch(xu, "Right", e.State, e.Detail) || keybind.KeyMatch(xu, "Page_Down", e.State, e.Detail) ||
			keybind.KeyMatch(xu, "j", e.State, e.Detail) || keybind.KeyMatch(xu, " ", e.State, e.Detail) {
			if idx != numImages-1 {
				idx += 1
				state &= loaded
				state &= drawn
				ctl.update()
			}
		}

		if keybind.KeyMatch(X, "F11", e.State, e.Detail) || keybind.KeyMatch(X, "f", e.State, e.Detail) || keybind.KeyMatch(X, "L1", e.State, e.Detail) {
			err = ewmh.WmStateReq(X, win.Id, ewmh.StateToggle, "_NET_WM_STATE_FULLSCREEN")
			if err != nil {
				fmt.Fprintf(os.Stderr, "WmStateReq: %s\n", err.Error())
			}
		}

		if keybind.KeyMatch(X, "[", e.State, e.Detail) {
			if idx-10 >= 0 {
				idx -= 10
				state &= loaded
				state &= drawn
				ctl.update()
			}
		} else if keybind.KeyMatch(X, "]", e.State, e.Detail) {
			if idx+10 <= numImages-1 {
				idx += 10
				state &= loaded
				state &= drawn
				ctl.update()
			}
		}

		if keybind.KeyMatch(X, ",", e.State, e.Detail) {
			idx = 0
			state &= loaded
			state &= drawn
			ctl.update()
		} else if keybind.KeyMatch(X, ".", e.State, e.Detail) {
			idx = numImages - 1
			state &= loaded
			state &= drawn
			ctl.update()
		}

// extracting metadata from image as well as displaying is..
// .. in scope? out of scope?
/*
		if keybind.KeyMatch(X, "Return", e.State, e.Detail) {
			fmt.Fprintf(os.Stdout, "%s\n", images[idx])
		}
*/


}

type controlX11Input struct{
	update func()
}

func handleX11Input(evC chan evInputX11, ctl controlX11Input) {
	for ev := range evC {
	switch v := ev.(type) {
	case evMouseInputX11:
		cbMouseInputX11(v, ctl)
	case evKeyInputX11:
		cbKeyInputX11(v, ctl)
	default:
		fmt.Println("unknown X11 input type")
	}
	}
}

// displayX11 displays images in X11 window.
func displayX11(imageC <-chan string, images []string, width, height int) {
	xgb.Logger.SetOutput(ioutil.Discard)
	xgbutil.Logger.SetOutput(ioutil.Discard)

	// set global state (aka hacks)
	numImages = len(images)



	var err error
	X, err = xgbutil.NewConn()
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s\n", err.Error())
		os.Exit(1)
	}

	keybind.Initialize(X)
	mousebind.Initialize(X)

	win, err = xwindow.Generate(X)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Generate: %s\n", err.Error())
		os.Exit(1)
	}

	defer win.Destroy()

	win.Create(X.RootWin(), 0, 0, width, height, xproto.CwBackPixel, 0x000000)
	win.Change(xproto.CwBackingStore, xproto.BackingStoreAlways)

	win.WMGracefulClose(func(w *xwindow.Window) {
		xevent.Detach(w.X, w.Id)
		keybind.Detach(w.X, w.Id)
		mousebind.Detach(w.X, w.Id)
		w.Destroy()
		xevent.Quit(w.X)
	})

	err = ewmh.WmWindowTypeSet(X, win.Id, []string{"_NET_WM_WINDOW_TYPE_DIALOG"})
	if err != nil {
		fmt.Fprintf(os.Stderr, "WmWindowTypeSet: %s\n", err.Error())
	}

	win.Listen(xproto.EventMaskKeyPress, xproto.EventMaskButtonRelease, xproto.EventMaskStructureNotify, xproto.EventMaskExposure)

	rect, err := win.Geometry()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Geometry: %s\n", err.Error())
	}

	var shmId int
	var seg mshm.Seg
	var data []byte

	var img image.Image
	var ximg *xgraphics.Image

	newImage := func() *xgraphics.Image {
		return xgraphics.New(X, image.Rect(0, 0, rect.Width(), rect.Height()))
	}

	ximg = newImage()

	loadSpecificImage := func(image string) {
		img, err = decode(image, rect.Width(), rect.Height())
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s\n", err.Error())
			return
		}

		state |= loaded
	}

	loadImage := func() {
		if numImages == 0 { return }
		loadSpecificImage(images[idx])
	}

	scaleImage := func() {
		if ximg != nil {
			ximg.Destroy()
			ximg = nil
		}

		ximg = newImage()

		i, err := scale(img, rect.Width(), rect.Height())
		if err != nil {
			fmt.Fprintf(os.Stderr, "scale: %s\n", err.Error())
		}

		offset := (rect.Width() - i.Bounds().Max.X) / 2
		draw.Draw(ximg, i.Bounds().Add(image.Pt(offset, 0)), i, image.ZP, draw.Over)

		state |= scaled
	}

	drawImage := func() {
		// "go-xiv" could include image info...
		err = ewmh.WmNameSet(ximg.X, win.Id, "go-xiv")
		if err != nil {
			fmt.Fprintf(os.Stderr, "WmNameSet: %s\n", err.Error())
		}


		err = ximg.CreatePixmap()
		if err != nil {
			fmt.Fprintf(os.Stderr, "CreatePixmap: %s\n", err.Error())
		}

		ximg.XDraw()
		ximg.XExpPaint(win.Id, 0, 0)

		state |= drawn
	}

	var stateLock sync.Mutex

	update := func() {
	stateLock.Lock()
		if state&loaded == 0 {
			loadImage()
			if img == nil {
				return
			}
		}
		if state&scaled == 0 {
			scaleImage()
		}
		if state&drawn == 0 {
			drawImage()
		}
	stateLock.Unlock()
	}

	go func() {
	for image := range imageC {
		state &= loaded
	stateLock.Lock()
		loadSpecificImage(image)
	stateLock.Unlock()
		if img == nil { continue }
		state = loaded
		update()
	}
	}()




	x11EvC := make(chan evInputX11)
	defer close(x11EvC)
	go handleX11Input(x11EvC, controlX11Input{update: update})
	cbKey := xevent.KeyPressFun(func(xu *xgbutil.XUtil, e xevent.KeyPressEvent) {
		x11EvC <- evKeyInputX11{ e: e, xu: xu }
	})
	cbBut := mousebind.ButtonReleaseFun(func(xu *xgbutil.XUtil, e xevent.ButtonReleaseEvent) {
		x11EvC <- evMouseInputX11{ e: e, xu: xu }
	})

	cbCfg := xevent.ConfigureNotifyFun(func(xu *xgbutil.XUtil, e xevent.ConfigureNotifyEvent) {
		if rect.Width() != int(e.Width) || rect.Height() != int(e.Height) {
			state &= drawn
			state &= scaled

			rect, err = win.Geometry()
			if err != nil {
				fmt.Fprintf(os.Stderr, "Geometry: %s\n", err.Error())
			}
		}
	})

	cbExp := xevent.ExposeFun(func(xu *xgbutil.XUtil, e xevent.ExposeEvent) {
		if e.ExposeEvent.Count == 0 {
			state &= drawn
			state &= scaled
			update()
		}
	})

	cbKey.Connect(X, win.Id)
	cbCfg.Connect(X, win.Id)
	cbBut.Connect(X, win.Id, "1", false, true)
	cbBut.Connect(X, win.Id, "3", false, true)
	cbExp.Connect(X, win.Id)

	win.Map()
	xevent.Main(X)


}
