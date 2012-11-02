package main

import (
	"bufio"
	"bytes"
	"flag"
	"fmt"
	"os"
	"strconv"
)

var problemNumber = flag.Int(
	"problem", 18, "A problem number from projecteuler.net")
var dataFile = flag.String(
	"datafile", "", "Data file used to solve the problem")


// Sample run:
//
// <built-program> --problem=18 --datafile=<path-to-repo>/data/problem18


func solve18() {
	fd, err := os.Open(*dataFile)
	if err != nil {
		panic(fmt.Sprintf("Failed to open %s: %v", *dataFile, err))
	}
	defer fd.Close()

	reader := bufio.NewReader(fd)
	spaceSep := []byte(" ")
	fmt.Printf("Reading data from file ... %s\n", *dataFile)

	var numbers [][]int

	line, _, err := reader.ReadLine()
	for err == nil && len(line) > 0 && line[0] != '#' {
		// Add a new row of numbers
		var rowNumbers []int
		lineData := bytes.Split(line, spaceSep)
		for _, element := range lineData {
			number, _ := strconv.Atoi(string(element))
			rowNumbers = append(rowNumbers, number)
		}
		// Add the row to our collection and move to the next line
		numbers = append(numbers, rowNumbers)
		line, _, err = reader.ReadLine()
	}

	// Make sure we have some minimum amount of data
	if len(numbers) < 2 {
		panic(fmt.Sprintf("Need atleast two rows of data !"))
	}

	// File is read, now start building up partial sums
	for rowNum := len(numbers) - 2; rowNum >= 0; rowNum-- {
		for index, number := range numbers[rowNum] {
			// Update the number to be a partial sum
			sum1 := number + numbers[rowNum+1][index]
			// TODO(agam): Check that each row has the right
			// number of elements.
			sum2 := number + numbers[rowNum+1][index+1]
			if sum1 > sum2 {
				numbers[rowNum][index] = sum1
			} else {
				numbers[rowNum][index] = sum2
			}
		}
	}

	// At the end of this process, the answer should be at the top
	fmt.Printf("The answer is [%d]\n", numbers[0][0])
}

func main() {
	flag.Parse()
	fmt.Printf("Solving problem %d\n", *problemNumber)
	switch *problemNumber {
	case 18:
		solve18()
	}
}
