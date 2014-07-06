//Carlos Arévalo Jimenez
//Uso: go run main_gixparse.go [fichero_a_parsear]

package main

import (
		"gixparse"
		"os"
		"bufio"
		)

func main() {
	var text string
	var line string	
	name := os.Args[1]
    file, err := os.Open(name)
    if err != nil { panic(err) }
    reader := bufio.NewReader(file)
	for err==nil {
		line, err = reader.ReadString('\n')
		text += line
	}
	_ = gixparse.Gixparse(text)
	err = file.Close()
	if err != nil { panic(err) }
} 
