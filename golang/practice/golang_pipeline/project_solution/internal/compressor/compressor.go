package compressor

import (
	"fmt"
	"strconv"
	"strings"
)

// The CompressRLE compresses
// the string s with RLE algorithm.
// The CompressRLE will return
// the string, for example: "a3b1c2".
func CompressRLE(s string) string {
	if len(s) == 0 {
		return ""
	}
	var b strings.Builder
	i := 0
	// Working with bytes - ASCII only.
	runes := []byte(s)
	for i < len(runes) {
		ch := runes[i]
		cnt := 1
		for i+cnt < len(runes) && runes[i+cnt] == ch {
			cnt++
		}
		// Here we write the symbol and it's number.
		b.WriteByte(ch)
		b.WriteString(strconv.Itoa(cnt))
		i += cnt
	}
	return b.String()
}

// DecompressRLE decompresses the string,
// compressed with CompressRLE.
func DecompressRLE(compressed string) (string, error) {
	if len(compressed) == 0 {
		return "", nil
	}
	var result strings.Builder
	runes := []byte(compressed)
	for i := 0; i < len(runes); {
		// Read the symbol.
		if i >= len(runes) {
			return "", fmt.Errorf("unexpected end of the string")
		}
		ch := runes[i]
		i++
		// Read the number of symbols.
		numStart := i
		for i < len(runes) && runes[i] >= '0' && runes[i] <= '9' {
			i++
		}
		if i == numStart {
			return "", fmt.Errorf("a number after the symbol was expected %c", rune(ch))
		}
		count, err := strconv.Atoi(string(runes[numStart:i]))
		if err != nil {
			return "", fmt.Errorf("failed to parse the number: %v", err)
		}
		// Write the symbol count number.
		for range count {
			result.WriteByte(ch)
		}
	}
	return result.String(), nil
}
