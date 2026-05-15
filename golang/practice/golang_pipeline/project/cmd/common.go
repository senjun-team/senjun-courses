package main

import (
	"fmt"
	"pipeline/internal/compressor"
	"pipeline/internal/encryptor"
	"sync/atomic"
	"time"
)

type job func(in, out chan any)

var (
	timeLockVal uint32 = 0
)

var timeLock = func() {
	for {
		if swapped := atomic.CompareAndSwapUint32(&timeLockVal, 0, 1); !swapped {
			fmt.Println("TimeLock was happened")
			time.Sleep(time.Second)
		} else {
			break
		}
	}
}

var timeUnlock = func() {
	for {
		if swapped := atomic.CompareAndSwapUint32(&timeLockVal, 1, 0); !swapped {
			fmt.Println("TimeUnlock was happened")
			time.Sleep(time.Second)
		} else {
			break
		}
	}
}

var compressorRLE = func(data string) string {
	timeLock()
	defer timeUnlock()
	compressed := compressor.CompressRLE(data)
	time.Sleep(10 * time.Millisecond)
	return compressed
}

var encryptorXOR = func(data string) string {
	encrypted := encryptor.EncryptXOR(data)
	time.Sleep(time.Second)
	return encrypted
}
