package main

// ваш код здесь

// Функция executePipeline реализует
// конвейер задач job
func executePipeline(jobs ...job) {
	// ваш код здесь
}

// Функция encryptAndCompress считает
// encrypt(data)+"~"+encrypt(compress(data))
func encryptAndCompress(in, out chan string) {
	// ваш код здесь
}

// Функция multiEncrypt считает
// encrypt(th+dataChunk), где
// th=0..5, а затем конкатенирует
// результаты в порядке расчета
func multiEncrypt(in, out chan string) {
	// ваш код здесь
}

// Функция generateResult сортирует
// строки по возрастанию и
// объединяет их через символ
// нижнего подчеркивания _
func generateResult(in, out chan string) {
	// ваш код здесь
}
