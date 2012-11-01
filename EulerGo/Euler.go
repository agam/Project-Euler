package main

import (
	"flag"
	"fmt"
)

var problemNumber = flag.Int(
	"Problem", 18, "A problem number from projecteuler.net")

func solve18() {
	fmt.Println("Solving problem 18")
}

func main() {
	flag.Parse()
	switch *problemNumber {
	case 18:
		solve18()
	}
}
