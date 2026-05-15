package main

import (
	"sync/atomic"
	"testing"
	"time"
)

/*
The TestPipeline checks that pipeline is really a pipeline.
If you accumulate values before passing them to the next
function, it's a mistake. You should pass the result
as soon as possible.
*/
func TestPipeline(t *testing.T) {
	var ok = true
	var received uint32
	freeFlowJobs := []job{
		job(func(in, out chan any) {
			out <- 0
			time.Sleep(10 * time.Millisecond)
			currReceived := atomic.LoadUint32(&received)
			// Counter should increase in the next function
			// before the current function finished it's execution.

			if currReceived == 0 {
				ok = false
			}
		}),
		job(func(in, out chan any) {
			for range in {
				atomic.AddUint32(&received, 1)
			}
		}),
	}
	executePipeline(freeFlowJobs...)
	if !ok || received == 0 {
		t.Error("no value free flow in the ExecutePipeline - " +
			"don't collect them before passing to the next function")
	}
}

func TestWorker(t *testing.T) {

	testExpected := "QT8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=QD8CJzExMz" +
		"oGDDwSKC80CiIwOUc8MSUBNz4=Qz8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=" +
		"Qj8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=RT8CJzExMzoGDDwSKC80CiIwOU" +
		"c8MSUBNz4=RD8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=_QTIkESIxGTJYDDE" +
		"8KCE0MBwwNzI8Tw==QDIkESIxGTJYDDE8KCE0MBwwNzI8Tw==QzIkESIxGTJYD" +
		"DE8KCE0MBwwNzI8Tw==QjIkESIxGTJYDDE8KCE0MBwwNzI8Tw==RTIkESIxGTJ" +
		"YDDE8KCE0MBwwNzI8Tw==RDIkESIxGTJYDDE8KCE0MBwwNzI8Tw=="

	/*
		testExpected := "QT8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=QD8CJz" +
			"ExMzoGDDwSKC80CiIwOUc8MSUBNz4=Qz8CJzExMzoGDDwSKC80CiIwOUc8" +
			"MSUBNz4=Qj8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=RT8CJzExMzoGDD" +
			"wSKC80CiIwOUc8MSUBNz4=RD8CJzExMzoGDDwSKC80CiIwOUc8MSUBNz4=" +
			"_QTA3OzE4MDZYDDMsKDY0CiIwMEc8ICUOTEo=QDA3OzE4MDZYDDMsKDY0C" +
			"iIwMEc8ICUOTEo=QzA3OzE4MDZYDDMsKDY0CiIwMEc8ICUOTEo=QjA3OzE" +
			"4MDZYDDMsKDY0CiIwMEc8ICUOTEo=RTA3OzE4MDZYDDMsKDY0CiIwMEc8I" +
			"CUOTEo=RDA3OzE4MDZYDDMsKDY0CiIwMEc8ICUOTEo=_QTENFT48CRgADD" +
			"ISKDw0ChwwNDI8PiUBPz4=QDENFT48CRgADDISKDw0ChwwNDI8PiUBPz4=" +
			"QzENFT48CRgADDISKDw0ChwwNDI8PiUBPz4=QjENFT48CRgADDISKDw0Ch" +
			"wwNDI8PiUBPz4=RTENFT48CRgADDISKDw0ChwwNDI8PiUBPz4=RDENFT48" +
			"CRgADDISKDw0ChwwNDI8PiUBPz4=_QTEnATU4GQA8OgM0TAkjNy0zIA8jO" +
			"zUsKDk0CiIwOUc8ICUOTEo=QDEnATU4GQA8OgM0TAkjNy0zIA8jOzUsKDk" +
			"0CiIwOUc8ICUOTEo=QzEnATU4GQA8OgM0TAkjNy0zIA8jOzUsKDk0CiIwO" +
			"Uc8ICUOTEo=QjEnATU4GQA8OgM0TAkjNy0zIA8jOzUsKDk0CiIwOUc8ICU" +
			"OTEo=RTEnATU4GQA8OgM0TAkjNy0zIA8jOzUsKDk0CiIwOUc8ICUOTEo=R" +
			"DEnATU4GQA8OgM0TAkjNy0zIA8jOzUsKDk0CiIwOUc8ICUOTEo=_QTIkES" +
			"IxGTJYDDE8KCE0MBwwNzI8Tw==QDIkESIxGTJYDDE8KCE0MBwwNzI8Tw==" +
			"QzIkESIxGTJYDDE8KCE0MBwwNzI8Tw==QjIkESIxGTJYDDE8KCE0MBwwNz" +
			"I8Tw==RTIkESIxGTJYDDE8KCE0MBwwNzI8Tw==RDIkESIxGTJYDDE8KCE0" +
			"MBwwNzI8Tw==_QTYNOyI7GTIwNwwwKQkkGS04IAA3OzYsKDY0ChgwNEc8M" +
			"yUOKz4=QDYNOyI7GTIwNwwwKQkkGS04IAA3OzYsKDY0ChgwNEc8MyUOKz4" +
			"=QzYNOyI7GTIwNwwwKQkkGS04IAA3OzYsKDY0ChgwNEc8MyUOKz4=QjYNO" +
			"yI7GTIwNwwwKQkkGS04IAA3OzYsKDY0ChgwNEc8MyUOKz4=RTYNOyI7GTI" +
			"wNwwwKQkkGS04IAA3OzYsKDY0ChgwNEc8MyUOKz4=RDYNOyI7GTIwNwwwK" +
			"QkkGS04IAA3OzYsKDY0ChgwNEc8MyUOKz4=_QTYNOzg+MDI9DDUSKDY0Cg" +
			"QwNkc8IyUBAT4=QDYNOzg+MDI9DDUSKDY0CgQwNkc8IyUBAT4=QzYNOzg+" +
			"MDI9DDUSKDY0CgQwNkc8IyUBAT4=QjYNOzg+MDI9DDUSKDY0CgQwNkc8Iy" +
			"UBAT4=RTYNOzg+MDI9DDUSKDY0CgQwNkc8IyUBAT4=RDYNOzg+MDI9DDUS" +
			"KDY0CgQwNkc8IyUBAT4="
	*/
	var testResult string

	//inputData := []string{"apple", "orange", "elephant",
	//	"golang", "senjun", "heart", "september"}
	inputData := []string{"apple", "orange"}

	jobs := []job{
		job(func(in, out chan any) {
			for _, data := range inputData {
				out <- data
			}
		}),
		job(singleRes),
		job(multiRes),
		job(allResults),
		job(func(in, out chan any) {
			dataRaw := <-in
			data, ok := dataRaw.(string)
			if !ok {
				t.Error("failed to convert result data to string")
			}
			testResult = data
		}),
	}

	start := time.Now()
	executePipeline(jobs...)

	end := time.Since(start)

	expectedTime := 3 * time.Second

	if testExpected != testResult {
		t.Errorf("results not match\ngot: %v\nwant: %v", testResult, testExpected)
	}

	if end > expectedTime {
		t.Errorf("execution is too long\ngot: %s\nwant: <%s", end, expectedTime)
	}
}
