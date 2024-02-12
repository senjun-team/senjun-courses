package main

import (
	"bytes"
	"io"
	"os"
	"strings"
	"testing"
)

func TestSwap(t *testing.T) {
	cases := []struct {
		firstString, secondString string
	}{
		{"val1", "val2"},
		{"val1", "val1"},
		{"", ""},
	}

	for _, c := range cases {
		oldFirst := c.firstString
		oldSecond := c.secondString

		swap(&c.firstString, &c.secondString)

		if oldFirst != c.secondString || oldSecond != c.firstString {
			t.Errorf("swap(&s, &s2) when s = %q and s2 = %q made s = %q and s2 = %q, wants s = %q and s2 = %q",
				oldFirst, oldSecond, c.firstString, c.secondString, oldSecond, oldFirst)
		}
	}
}

func TestMain(t *testing.T) {
	old := os.Stdout // keep backup of the real stdout
	r, w, _ := os.Pipe()
	os.Stdout = w

	outC := make(chan string)
	// copy the output in a separate goroutine so printing can't block indefinitely
	go func() {
		var buf bytes.Buffer
		io.Copy(&buf, r)
		outC <- buf.String()
	}()

	main()

	// back to normal state
	w.Close()
	os.Stdout = old // restoring the real stdout
	out := <-outC

	// reading our temp stdout
	if strings.TrimSpace(out) != "Hello, gophers!" {
		t.Fatalf("main() prints \"" + strings.TrimSpace(out) + "\", not \"Hello, gophers!\"")
	}

}
