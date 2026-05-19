package main

import (
	"fmt"
	"sort"
	"strings"
	"sync"
)

func executePipeline(jobs ...job) {
	if len(jobs) == 0 {
		return
	}
	var wg sync.WaitGroup
	wg.Add(len(jobs))
	in := make(chan any)
	var out chan any
	for i, job := range jobs {
		if i%2 == 0 {
			out = make(chan any)
			go func(in chan any, out chan any) {
				defer wg.Done()
				job(in, out)
				close(out)
			}(in, out)
		} else {
			in = make(chan any)
			go func(in chan any, out chan any) {
				defer wg.Done()
				job(out, in)
				close(in)
			}(in, out)
		}
	}
	wg.Wait()
}

func encryptAndCompress(in, out chan any) {
	var wg sync.WaitGroup
	for data := range in {
		rle := compressorRLE(fmt.Sprintf("%v", data))
		wg.Add(1)
		go func() {
			defer wg.Done()
			begin := make(chan string)
			go func(b chan string) {
				b <- encryptorXOR(fmt.Sprintf("%v", data))
			}(begin)
			end := make(chan string)
			go func(e chan string) {
				e <- encryptorXOR(rle)
			}(end)
			out <- fmt.Sprintf("%s~%s", <-begin, <-end)
		}()
	}
	wg.Wait()
}

func multiEncrypt(in, out chan any) {
	var wg sync.WaitGroup
	for data := range in {
		wg.Add(1)
		go func(data any, out chan any) {
			defer wg.Done()
			const thMax = 6
			const resNumber = 5
			ch := make(chan string, resNumber)
			var xorStorage []string
			for th := range thMax {
				go func(ch chan string) {
					// Remember the number in the
					// beginning to sort later.
					// After that we'll remove
					// the number.
					ch <- fmt.Sprintf("%d%s", th,
						encryptorXOR(fmt.Sprintf("%d%s", th,
							data.(string))))
				}(ch)
			}
			for range thMax {
				xorStorage = append(xorStorage, <-ch)
			}
			sort.Strings(xorStorage)
			var res strings.Builder
			const digitNum = 1
			for _, x := range xorStorage {
				res.WriteString(x[digitNum:])
			}
			out <- res.String()
		}(data, out)
	}
	wg.Wait()
}

func generateResult(in, out chan any) {
	var allData []string
	for data := range in {
		allData = append(allData, data.(string))
	}
	sort.Strings(allData)
	var res strings.Builder
	for _, data := range allData {
		res.WriteString(data)
		res.WriteString("_")
	}
	s := res.String()
	if len(s) > 0 {
		s = s[:len(s)-1]
	}
	out <- s
}
