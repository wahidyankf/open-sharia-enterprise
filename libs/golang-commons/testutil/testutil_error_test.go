package testutil

import (
	"errors"
	"os"
	"testing"
)

func TestCaptureStdout_PipeError(t *testing.T) {
	orig := osPipe
	osPipe = func() (*os.File, *os.File, error) {
		return nil, nil, errors.New("forced pipe error")
	}
	defer func() { osPipe = orig }()

	defer func() {
		r := recover()
		if r == nil {
			t.Error("expected panic when os.Pipe fails, got none")
		}
	}()

	CaptureStdout(t)
}
