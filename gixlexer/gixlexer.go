//Carlos Arévalo Jimenez

package gixlexer

import (
	"bytes"
	"fmt"
	"io"
	"unicode"
	"strings"
	"strconv"
)

//Interfaz que avanza o retrocede runas
type Text interface {
	GetRune() (rune, error)
	UnGetRune() error
}

//Estructura que permite el uso de entrada con buffer
type Buffer struct {
	texto io.RuneScanner
}

//Avanza una runa
func (b *Buffer) GetRune() (rune, error) {
	r, _, err := b.texto.ReadRune()
	return r, err
}

//Retrocede una runa
func (b *Buffer) UnGetRune() error {
	return b.texto.UnreadRune()
}

//Nuevo buffer para texto
func NewBuffer(s string) Text {
	return &Buffer{texto: bytes.NewBufferString(s)}

}

//Función que devuelve el string asociado a cada token
func (tid TokId) String() string {
	switch tid {
	case None: return "none"
	case LitInt: return "LitInt"
	case LitFloat: return "LitFloat"
	case TypeInt: return "TypeInt"
	case TypeFloat: return "TypeFloat"
	case Nombre: return "nombre"
	case Macro: return "macro"
	case Loop: return "loop"
	case Eof: return "eof"
	case Equal: return "="
	case Comma: return ","
	case Scol: return ";"
	case Doub: return ":"
	case Lpar: return "("
	case Rpar: return ")"
	case Lbra: return "{"
	case Rbra: return "}"}
	return fmt.Sprintf("%v", tid)
}

//Id para cada token
type TokId int

//Estructura de un token: un id, un lexema y un número de línea
type Tok struct {
	Id  TokId
	ValS string
	ValI int64
	ValF float64
	Line int
}

// Id asignado a cada token
const (
	None TokId = iota
	LitInt
	LitFloat
	TypeInt
	TypeFloat
	Nombre
	Macro
	Loop
	Eof
	Equal
	Comma 
	Scol 
	Doub 
	Lpar 
	Rpar 
	Lbra 
	Rbra 
)

//Interfaz del lexer
type Lexer interface {
	Scan() (Tok, error)
	LookAhead() (Tok, error)
	GetLine() int
}

//Estructura de un lexer: un texto de entrada, un token guardado, un número de línea y un token actual
type lex struct {
	in Text
	saved Tok
	line int
	val []rune
}

//Para crear un nuevo lexer
func NewLexer(t Text) Lexer {
	return &lex{in: t, line: 1}
}

//Añade una runa al token actual
func (l *lex) got(r rune) {
	l.val = append(l.val, r)
}

//Devuelve el número de línea
func (l *lex) GetLine() int {
	return l.line
}

//Obtiene un nuevo token y lo guarda
func (l *lex) LookAhead() (Tok, error) {
	token, err := l.Scan()
	if err==io.EOF {
		return l.gotTok(Eof), nil
	}
	l.saved = token
	return token, err
}

//Salta los blancos
func (l *lex) skipBlanks() error {
	for {
		runa, err := l.in.GetRune()
		if err != nil {
			return err
		}
		if runa == '#' {
			for runa != '\n' {
				if runa, err = l.in.GetRune(); err != nil {
					return err
				}
			}
			l.line++
		}
		if !unicode.IsSpace(runa) {
			l.in.UnGetRune()
			return nil
		}
		if runa == '\n' { 
			l.line++
		}
	}
	return nil
}

//Obtiene un nuevo token
func (l *lex) Scan() (Tok, error) {
	if l.saved.Id != None {
		x := l.saved
		l.saved = Tok{}
		return x, nil
	}
	err := l.skipBlanks()
	if err == io.EOF {
		return l.gotTok(Eof), nil
	} else if err != nil {
		return Tok{}, err
	}
	return l.nextTok()
}

//Devuelve el nuevo token
func (l *lex) gotTok(id TokId) Tok {
	var t Tok
	switch id {
	case LitInt:
		if len(l.val) > 2 {
			if l.val[0]=='0' && l.val[1]=='x' {
				hexa, _ := strconv.ParseInt(string(l.val), 0, 0)
				t = Tok{
					Id: id,
					ValI: hexa,
					Line: l.line,
				}
			} else {
				i, _ := strconv.ParseInt(string(l.val), 10, 64)
				t = Tok{
					Id: id,
					ValI: i,
					Line: l.line,
				}
			}
		} else {
			i, _ := strconv.ParseInt(string(l.val), 10, 64)
			t = Tok{
				Id: id,
				ValI: i,
				Line: l.line,
			}
		}
	case LitFloat:
		flo, _ := strconv.ParseFloat(string(l.val), 0)
		t = Tok{
			Id: id,
			ValF: flo,
			Line: l.line,
		}
	default:
		t = Tok{
			Id: id,
			ValS: string(l.val),
			Line: l.line,
		}
	}
	l.val = nil
	return t
}

//Runas de puntuación que son válidas después de un número
func IsValidAfterNumber(c rune) bool {
	return !(c=='(' || c=='.' || c==':')
}

//Runas válidas en un hexadecimal
func IsHexaValid(c rune) bool {
	hexa := "abcdef"
	return unicode.IsNumber(c) || strings.ContainsRune(hexa, c) 
}

//Autómata que reconoce tokens
func (l *lex) nextTok() (Tok, error) {
	runa, err := l.in.GetRune()
	if err == io.EOF {
		return l.gotTok(Eof), nil
	} else if err != nil {
		return Tok{}, err
	}
	switch runa {
	case '=': return l.gotTok(Equal), nil
	case '(': return l.gotTok(Lpar), nil
	case ')': return l.gotTok(Rpar), nil
	case '{': return l.gotTok(Lbra), nil
	case '}': return l.gotTok(Rbra), nil
	case ';': return l.gotTok(Scol), nil
	case ':': return l.gotTok(Doub), nil
	case ',': return l.gotTok(Comma), nil
	default:
		l.got(runa)
		if unicode.IsLetter(runa) {
			//Nombres y palabras reservadas
			for {
				runa, err := l.in.GetRune()
				if err == io.EOF {
					return l.gotTok(Eof), nil
				} else if err != nil {
					return Tok{}, err
				}
				if !unicode.IsLetter(runa) && !unicode.IsNumber(runa) {
					l.in.UnGetRune()
					t := l.gotTok(Nombre)
					if id, ok := reservadas[t.ValS]; ok {
						t.Id = id
					}
					return t, nil
				}
				l.got(runa)
			}
		}
		if unicode.IsNumber(runa) || runa=='-' {
			//Números
			for {
				runa, err := l.in.GetRune()
				if err == io.EOF {
					return l.gotTok(Eof), nil
				} else if err != nil {
					return Tok{}, err
				}
				if l.val[0] == '0' && runa == 'x' {
					//Hexadecimales
					l.got(runa)
					for {
						runa, err := l.in.GetRune()
						if err == io.EOF {
							return l.gotTok(Eof), nil
						} else if err != nil {
							return Tok{}, err
						}					 
						if !IsHexaValid(runa) {
							l.in.UnGetRune()
							t := l.gotTok(LitInt)
							return t, nil
						}
						if !IsValidAfterNumber(runa) {
							return Tok{}, fmt.Errorf("syntax error %c", runa)
						}
						l.got(unicode.ToLower(runa))
					}
				}
				if runa == '.' {
					//Float
					l.got(runa)
					for {
						runa, err := l.in.GetRune()
						if err == io.EOF {
							return l.gotTok(Eof), nil
						} else if err != nil {
							return Tok{}, err
						}					 
						if !unicode.IsNumber(runa) && IsValidAfterNumber(runa) {
							l.in.UnGetRune()
							t := l.gotTok(LitFloat)
							return t, nil
						}
						if !IsValidAfterNumber(runa) {
							return Tok{}, fmt.Errorf("syntax error %c", runa)
						}
						l.got(runa)
					}
				}
				if !unicode.IsNumber(runa) && IsValidAfterNumber(runa) {
					l.in.UnGetRune()
					t := l.gotTok(LitInt)
					return t, nil
				}
				if !IsValidAfterNumber(runa) {
					return Tok{}, fmt.Errorf("syntax error %c", runa)
				}
				l.got(runa)
			}
		}
	}
	return Tok{}, fmt.Errorf("syntax error %c", runa)
}

var reservadas = map[string]TokId {
	"int": TypeInt, "float": TypeFloat, "macro": Macro, "loop": Loop,
}

func gixlexer(text string) string {
	txt := NewBuffer(text)
	l := NewLexer(txt)
	var output string 
	for {
		t, err := l.Scan()
		if err != nil {
			output += fmt.Sprintf("error: %s\n", err)
			break
		}
		if t.Id == LitInt {
			output += fmt.Sprintf("%s(%v)\n", t.Id, t.ValI)
		} else if t.Id == LitFloat {
			output += fmt.Sprintf("%s(%v)\n", t.Id, t.ValF)
		} else if t.ValS != "" {
			output += fmt.Sprintf("%s(%v)\n", t.Id, t.ValS)	
		} else {		
			output += fmt.Sprintf("%s\n", t.Id)
		}
		if t.Id==Eof { break }
	}
	return output
} 
