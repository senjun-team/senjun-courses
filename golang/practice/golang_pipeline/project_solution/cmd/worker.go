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
	var curIn chan string
	var wg sync.WaitGroup
	for _, currentJob := range jobs {
		curOut := make(chan string)
		wg.Add(1)
		go func(j job, in, out chan string) {
			defer wg.Done()
			defer close(out)
			j(in, out)
		}(currentJob, curIn, curOut)
		curIn = curOut
	}
	wg.Wait()
}

func encryptAndCompress(in, out chan string) {
	var wg sync.WaitGroup
	for data := range in {
		rle := compress(fmt.Sprintf("%v", data))
		wg.Add(1)
		go func() {
			defer wg.Done()
			begin := make(chan string)
			go func(b chan string) {
				b <- encrypt(fmt.Sprintf("%v", data))
			}(begin)
			end := make(chan string)
			go func(e chan string) {
				e <- encrypt(rle)
			}(end)
			out <- fmt.Sprintf("%s~%s", <-begin, <-end)
		}()
	}
	wg.Wait()
}

func multiEncrypt(in, out chan string) {
	var wg sync.WaitGroup
	for data := range in {
		wg.Add(1)
		go func(data string, out chan string) {
			defer wg.Done()
			const thMax = 6
			ch := make(chan string, thMax)
			var xorStorage []string
			for th := range thMax {
				go func(ch chan string) {
					// Remember the number in the
					// beginning to sort later.
					// After that we'll remove
					// the number.
					ch <- fmt.Sprintf("%d%s", th,
						encrypt(fmt.Sprintf("%d%s", th,
							data)))
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

func generateResult(in, out chan string) {
	var allData []string
	for data := range in {
		allData = append(allData, data)
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
