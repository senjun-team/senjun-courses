package main

import (
	"fmt"
	"log"
	"time"
)

func main() {
	var result string
	inputData := []string{"apple", "orange"}

	jobs := []job{
		job(func(in, out chan any) {
			for _, data := range inputData {
				out <- data
			}
		}),
		job(encryptAndCompress),
		job(multiEncrypt),
		job(generateResult),
		job(func(in, out chan any) {
			dataRaw := <-in
			data, ok := dataRaw.(string)
			if !ok {
				log.Fatal("failed to convert result data to string")
			}
			result = data
		}),
	}
	start := time.Now()
	executePipeline(jobs...)
	elapsedTime := time.Since(start)
	fmt.Printf("result: \n%s\n", result)
	fmt.Printf("elapsed time: %.2f seconds\n", elapsedTime.Seconds())
}
