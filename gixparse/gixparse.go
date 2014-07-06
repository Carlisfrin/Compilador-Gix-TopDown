//Carlos Arévalo Jiménez

package gixparse

import (
	"gixlexer"
	"fmt"
	"strconv"
	"io"
	)

//Tipos para la pila
type Skind int
type Ambito map[string]*Sym
type Pila []Ambito

//Tipos de símbolos
const (
	Snone Skind=iota
	Svar
	Smacro
	Skey
	Slit
)

//El tipo símbolo
type Sym struct {
	name string
	kind Skind
	tokid gixlexer.TokId
	line int
}

//Inserta un ámbito en la pila
func pushTabla(p Pila, a Ambito) Pila{
	return append(p, a)
}

//Saca el último ámbito de la pila
func popTabla(p Pila) Pila { 
	return p[:len(p)-1]
}

//Inserta un simbolo en el ultimo ambito
func pushSim(p Pila, n string, s *Sym) Pila {
	p[len(p)-1][n]=s
	return p
}

//Devuelve un puntero a un simbolo de la tabla
func getSym(p Pila, n string) *Sym {
	for i := range p {
		for k, v := range p[i] {
			if k==n {
				return v
			}
		}
	}
	return nil
}

//Crea un símbolo nuevo
func createSym(n string, k Skind, t gixlexer.TokId, l int) *Sym {
	return &Sym{name:n, kind: k, tokid: t, line: l}
}

//Comprueba si un símbolo está declarado en algún ámbito del programa
func IsDeclared(p Pila, s *Sym) bool {
	if s.kind==Slit {
		return true
	}
	for i := range p {
		for k, _ := range p[i] {
			if k==s.name {
				return true
			}
		}
	}
	return false
}

//Comprueba si un símbolo está declarado en el ámbito actual
func IsDuplicated(p Pila, s *Sym) bool {
	for k, _ := range p[len(p)-1] {
		if k==s.name {
			return true
		}
	}
	return false
}

//Árbol
type Tkind int
type Tree struct {
	kind Tkind
	symList []*Sym
	Ramas []*Tree
}

//Tipos de arboles
const (
	Tnone Tkind=iota
	Tprograma
	Tmacro
	Tdecl
	Tloop
	Tfunc
	Tasig
)

//Crea un nuevo árbol
func NewTree(name Tkind, list []*Sym) *Tree {
	return &Tree{kind: name, symList: list}
}

//Añade una rama a un árbol existente
func (t *Tree) AddRama(name Tkind, list []*Sym) *Tree {
	rama := NewTree(name, list)
	t.Ramas = append(t.Ramas, rama)
	return rama
}

//Devuelve un string de tabuladores
func AddTabs(number int) string {
	result := ""
	for i:=0;i<number;i++ {
		result += "\t"
	}
	return result
}

//Devuelve el string asociado a una rama
func RamaString(t *Tree) string {
	switch t.kind {
	case Tmacro:
		result := "macro "+t.symList[0].name+"("
		for i:=1; i<=(len(t.symList)-1)/2; i++ {
			result += t.symList[i*2-1].name+" "
			result += t.symList[i*2].name
			if i != (len(t.symList)-1)/2 {
				result += ","
			}
		}
		return result+")"
	case Tdecl:
		return t.symList[0].name+" "+t.symList[1].name+";"
	case Tloop:
		return "loop "+t.symList[0].name+":"+t.symList[1].name+","+t.symList[2].name
	case Tfunc:
		result := t.symList[0].name+"("
		for i:=1; i<len(t.symList); i++ {
			result += t.symList[i].name
			if i != len(t.symList)-1 {
				result += ","
			}
		}
		return result+");"
	case Tasig:
		return t.symList[0].name+" = "+t.symList[1].name+";"
	}
	return ""
}

//Recorre el arbol para crear el string
func RecorreArbol(t *Tree, tabs int) string {
	result := ""
	ramas := t.Ramas
	var ramo *Tree
	for i := range ramas {
		result += AddTabs(tabs+1)+RamaString(ramas[i])+"\n"
		for k := range ramas[i].Ramas {
			ramo = ramas[i].Ramas[k]
			result += AddTabs(tabs+2)+RamaString(ramo)+"\n" + RecorreArbol(ramo, tabs+2)
		}
	}
	return result
}

//Convierte el árbol en un string imprimible
func (t *Tree) String() string {
	return "AST:\n" + RecorreArbol(t, 0)
}

//Estructura para el parser
type Parse struct {
    l gixlexer.Lexer
	tabla Pila
}

//Crea un nuevo parser
func NewParser(l gixlexer.Lexer) *Parse {
    return &Parse{l: l, tabla: nil}
}

//Comprueba si el siguiente token coincide con el id
func (p *Parse) match(id gixlexer.TokId) (error, bool, string) {
    t, err := p.l.LookAhead()
    if err == io.EOF {
        return nil, false, ""
    }
    if err != nil {
        return err, false, ""
    }
    if t.Id != id {
        return nil, false, ""
    }
    t, err = p.l.Scan()
	switch id {
	case gixlexer.LitInt, gixlexer.LitFloat:
		return err, true, ""
	}
	return err, true, t.ValS
}

//LIT =  LitInt | LitFloat
func (p *Parse) parseLit() (error, string) {
	tok, err := p.l.LookAhead()
	if err != nil {
        return err, ""
    }
	switch tok.Id {
	case gixlexer.LitInt:
		p.l.Scan()
		return nil, strconv.FormatInt(tok.ValI, 10)
	case gixlexer.LitFloat:
		p.l.Scan()
		return nil, fmt.Sprintf("%f",tok.ValF)
	}
	return fmt.Errorf("literal not found"), ""
}

//VAR = nombre
func (p *Parse) parseVar() (error, string) {
	err, found, name := p.match(gixlexer.Nombre)
	if err != nil {
		return err, ""
	}
	if !found {
        return fmt.Errorf("name not found"), ""
    }
	return nil, name
}

//VAR_LIT = VAR | LIT
func (p *Parse) parseVarLit() (error, *Sym) {
	err, varName := p.parseVar()
	if  err != nil {
		e, litName := p.parseLit()
		if litName=="" {
			return nil, nil
		}
		return e, createSym(litName, Slit, gixlexer.Nombre, p.l.GetLine())
	}
	if getSym(p.tabla, varName)==nil {
		return fmt.Errorf("symbol '"+varName+"' not declared"), nil
	}
	return nil, getSym(p.tabla, varName)
}

//TIPO = 'int' | 'float'
func (p *Parse) parseTipo() (error, string) {
	if err, found1, _ := p.match(gixlexer.TypeInt); err != nil || !found1 {
		err, found2, _ := p.match(gixlexer.TypeFloat)
		if err != nil {
	        return err, ""
		} 
		if !found1 && !found2 {
			return fmt.Errorf("type not found"), ""
		}
		return nil, "float"
    }
	return nil, "int"
}

//MACROS = MACRO MACROS	| 'eof'
func (p *Parse) parseMacros(macrostree []*Tree) (error, []*Tree) {
	tok, err := p.l.LookAhead()
	if err != nil {
        return err, macrostree
    }
	switch tok.Id {
	case gixlexer.Macro:
		err, t := p.parseMacro()
		if err != nil {	
    	    return err, macrostree
    	}
		macrostree = append(macrostree, t)
    	return p.parseMacros(macrostree)
	case gixlexer.Eof:
		p.l.Scan()
		return nil, macrostree
	}
	return fmt.Errorf("macro not found"), macrostree
}

//MACRO = 'macro' VAR '(' PARAMS ')' '{' DECLS SENTS '}'
func (p *Parse) parseMacro() (error, *Tree) {
    if err, found, _ := p.match(gixlexer.Macro); err != nil || !found {
        return fmt.Errorf("not found 'macro'"), nil
    }
	err, name := p.parseVar()
    if err != nil {
        return err, nil
    }
	simboloMacro := createSym(name, Smacro, gixlexer.Nombre, p.l.GetLine())
	p.tabla = pushSim(p.tabla, name, simboloMacro)
	p.tabla = pushTabla(p.tabla, make(Ambito))
	if err, found, _ := p.match(gixlexer.Lpar); err != nil || !found {
        return fmt.Errorf("not found '(' in macro "+name), nil
    }
	err, paramList := p.parseParams(nil)
	if err != nil {
        return err, nil
    }
	syms := []*Sym{simboloMacro}
	for i := range paramList {
		syms = append(syms, paramList[i])
	}
	macroTree := NewTree(Tmacro, syms)
	if err, found, _ := p.match(gixlexer.Rpar); err != nil || !found {
        return fmt.Errorf("not found ')' in macro "+name), nil
    }
	if err, found, _ := p.match(gixlexer.Lbra); err != nil || !found {
        return fmt.Errorf("not found '{' in macro "+name), nil
    }
	err, macroTree = p.parseDecls(macroTree)
	if err != nil {
        return err, nil
    }
	err, macroTree = p.parseSents(macroTree)
	if err != nil {
        return err, nil
    }
	if err, found, _ := p.match(gixlexer.Rbra); err != nil || !found {
        return fmt.Errorf("not found '}' at the end of macro"+name), nil
    }
	p.tabla = popTabla(p.tabla)
    return nil, macroTree
}

//PARAM = TIPO VAR | e
func (p *Parse) parseParam(result []*Sym) (error, []*Sym, bool) {
	err, tipo := p.parseTipo()
	if err != nil {
        return nil, result, false
    }
	err, varname := p.parseVar()
	if err != nil {
        return err, result, false
    }
	simboloTipo := getSym(p.tabla, tipo)
	simboloParam := createSym(varname, Svar, gixlexer.Nombre, p.l.GetLine())
	if IsDuplicated(p.tabla, simboloParam) {
		return fmt.Errorf("symbol '"+simboloParam.name+"' declared previously"), result, false
	}
	result = append(result, simboloTipo)
	result = append(result, simboloParam)
	p.tabla = pushSim(p.tabla, varname, simboloParam)
	return err, result, true
}

//NEXT_PARAM = ',' PARAM | e
func (p *Parse) parseNextParam(result []*Sym) (error, []*Sym, bool) {
	if err, found, _ := p.match(gixlexer.Comma); err != nil || !found {
        return nil, result, false
    }
	return p.parseParam(result)
}

//PARAMS = PARAM NEXT_PARAM	PARAMS | e
func (p *Parse) parseParams(result []*Sym) (error, []*Sym) {
	err, result, found1 := p.parseParam(result)
	if err != nil {
        return nil, result
    }
	err, result, found2 := p.parseNextParam(result)
	if err != nil {
        return err, result
    }
	if !found1 && !found2 { 
		return nil, result
	}
	return p.parseParams(result)
}

//DECL = PARAM ';'
func (p *Parse) parseDecl(uT *Tree) (error, *Tree, bool) {
	err, symDecl, found := p.parseParam(nil)
	if err != nil {
        return err, uT, false
    }
	if !found {
		return nil, uT, false
	}
	err, found, _ = p.match(gixlexer.Scol)
	if err != nil {
        return err, uT, false
    }
	if !found {
		return fmt.Errorf("not found semicolon at the end of declaration"), uT, false
	}
	uT.AddRama(Tdecl, symDecl)
	return nil, uT, true
}

//DECLS = DECL DECLS | e
func (p *Parse) parseDecls(upper *Tree) (error, *Tree) {
	err, t, found := p.parseDecl(upper)
	if err != nil {
        return err, t
    }
	if !found {
		return nil, t
	}
	return p.parseDecls(upper)
}

//SENTS = SENT SENTS | e
func (p *Parse) parseSents(upperTree *Tree) (error, *Tree) {
	err, t, found := p.parseSent(upperTree)
	if err != nil {
        return err, t
    }
	if !found {
		return nil, t
	}
	return p.parseSents(t)
}

//FIRST_IN = VAR_LIT | e
func (p *Parse) parseFirstIn() (error, bool, *Sym) {
	err, varSym := p.parseVarLit()
	if varSym==nil && err==nil{
		return nil, false, nil
	}
	if err != nil {
    	return err, true, nil
	}
	return nil, true, varSym
}

//IN_FUN = ',' VAR_LIT | e
func (p *Parse) parseInFun() (error, *Sym, bool) {
	if err, found, _ := p.match(gixlexer.Comma); err != nil || !found {
        return nil, nil, false
    }
	e, varSym := p.parseVarLit()
	if varSym==nil && e==nil {
		return fmt.Errorf("not found parameter at the end of function"), nil, false
	}
	if e != nil {
		return e, nil, true
	}
	return e, varSym, true
}

//IN_FUNS = FIRST_IN IN_FUN IN_FUNS | e
func (p *Parse) parseInFuns() (error, []*Sym) {
	err, found1, first := p.parseFirstIn()
	if found1 && (err!=nil) {
		return err, nil
	}
	err, second, found2 := p.parseInFun()
	if found2 && (err!=nil) {
		return err, nil
	}
	if !found1 && !found2 { 
		return nil, nil 
	}
	err, third := p.parseInFuns()
	var result []*Sym
	if found1 {
		result = append(result, first)
	}
	if found2 {
		result = append(result, second)
	}
	if third != nil {
		for i := range third {
			result = append(result, third[i])
		}
	}
	return err, result
}

//FUNC = '(' IN_FUNS ')' ';'
func (p *Parse) parseFunc(funcName string, uT *Tree) (error, *Tree) {
	if err, found, _ := p.match(gixlexer.Lpar); err != nil || !found {
        return fmt.Errorf("not found '(' in function "+funcName), uT
    }
	err, inFuns := p.parseInFuns()
	if err != nil {
    	return err, uT
	}
	if err, found, _ := p.match(gixlexer.Rpar); err != nil || !found {
	    return fmt.Errorf("not found ')' in function "+funcName), uT
	}
	if err, found, _ := p.match(gixlexer.Scol); err != nil || !found {
        return fmt.Errorf("not found ';' in function "+funcName), uT
    }
	funcSym := createSym(funcName, Svar, gixlexer.Nombre, p.l.GetLine())
	symsFunc := []*Sym{funcSym}
	for i := range inFuns {
		symsFunc = append(symsFunc, inFuns[i])
	}
	if !IsDeclared(p.tabla, funcSym) {
		return fmt.Errorf("macro '"+funcName+"' not declared"), uT
	}
	uT.AddRama(Tfunc, symsFunc)
    return nil, uT
}

//SENT = LOOP | VAR STMT | ';'	
func (p *Parse) parseSent(upT *Tree) (error, *Tree, bool) {
	tok, err := p.l.LookAhead()
	if err != nil {
        return err, nil, false
    }
	switch tok.Id {
	case gixlexer.Loop:
		return p.parseLoop(upT)
	case gixlexer.Nombre:
		err, name := p.parseVar()
		if err != nil { 
			return err, nil, false 
		}
		err, upT = p.parseStmt(upT, name)
		return err, upT, true
	case gixlexer.Scol:
		p.l.Scan()
		return nil, upT, true
	}
	return nil, upT, false
}

//STMT = FUNC | ASIG	
func (p *Parse) parseStmt(upT *Tree, n string) (error, *Tree) {
	tok, err := p.l.LookAhead()
	if err != nil {
        return err, nil
    }
	switch tok.Id {
	case gixlexer.Lpar:
		return p.parseFunc(n, upT)
	case gixlexer.Equal:
		return p.parseAsig(n, upT)
	}
	return fmt.Errorf("no sentence found"), upT
}

//ASIG = '=' VAR_LIT ';'
func (p *Parse) parseAsig(lname string, uT *Tree) (error, *Tree) {
	lSym := createSym(lname, Svar, gixlexer.Nombre, p.l.GetLine())
	if !IsDeclared(p.tabla, lSym) {
		return fmt.Errorf("symbol '"+lname+"' not declared"), uT
	}
    if err, found, _ := p.match(gixlexer.Equal); err != nil || !found {
        return fmt.Errorf("not found '=' in assignment"), uT
    }
	err, rSym := p.parseVarLit()
	if rSym==nil {
		return fmt.Errorf("not found right value in assignment"), uT
	}
    if err != nil {
        return err, uT
    }
	if err, found, _ := p.match(gixlexer.Scol); err != nil || !found {
        return fmt.Errorf("not found ';' in assignment"), uT
    }
	syms := []*Sym{lSym, rSym}
	uT.AddRama(Tasig, syms)
    return nil, uT
}

//LOOP = 'loop' VAR ':' VAR_LIT ',' VAR_LIT '{' SENTS '}'
func (p *Parse) parseLoop(uT *Tree) (error, *Tree, bool) {
    if err, found, _ := p.match(gixlexer.Loop); err != nil || !found {
        return fmt.Errorf("not found 'loop'"), uT, false
    }
	err, iter := p.parseVar()
	simboloIter := createSym(iter, Svar, gixlexer.Nombre, p.l.GetLine())
	loopAmbito := Ambito{iter: simboloIter}
	p.tabla = pushTabla(p.tabla, loopAmbito)
    if err != nil {
        return err, uT, false
    }
	if err, found, _ := p.match(gixlexer.Doub); err != nil || !found {
        return fmt.Errorf("not found ':' in loop"), uT, false
    }
	err, varSym1 := p.parseVarLit()
	if varSym1==nil {
		return fmt.Errorf("not found starting value for iterator"), uT, false
	}
	if err != nil {
        return err, uT, false
    }
	if err, found, _ := p.match(gixlexer.Comma); err != nil || !found {
        return fmt.Errorf("not found ','"), uT, false
    }
	err, varSym2 := p.parseVarLit()
	if varSym2==nil {
		return fmt.Errorf("not found ending value for iterator"), uT, false
	}
	if err != nil {
        return err, uT, false
    }
	if err, found, _ := p.match(gixlexer.Lbra); err != nil || !found {
        return fmt.Errorf("not found '{'"), uT, false
    }
	sym1 := createSym(iter, Svar, gixlexer.Nombre, p.l.GetLine())
	symsLoop := []*Sym{sym1, varSym1, varSym2}
	loopTree := uT.AddRama(Tloop, symsLoop)
	if err, uT := p.parseSents(loopTree); err != nil {
        return err, uT, false
    }
	if err, found, _ := p.match(gixlexer.Rbra); err != nil || !found {
        return fmt.Errorf("not found '}' in loop"), uT, false
    }
	p.tabla = popTabla(p.tabla)	
    return nil, uT, true
}

//Inicializa los builtins y palabras reservadas del lenguaje
func InicializaTabla() Pila {
	var p Pila
	simboloInt := createSym("int", Skey, gixlexer.TypeInt, 1)
	simboloFloat := createSym("float", Skey, gixlexer.TypeFloat, 1)
	simboloCircle := createSym("circle", Smacro, gixlexer.Nombre, 1)
	simboloRect := createSym("rect", Smacro, gixlexer.Nombre, 1)
	builtIns := Ambito{"int": simboloInt, 
					   "float": simboloFloat, 
					   "circle": simboloCircle, 
					   "rect": simboloRect}
	return pushTabla(p, builtIns)
}

func (p *Parse) Parse() error {	
	tree := NewTree(Tprograma, nil)	
	p.tabla = InicializaTabla()
	err, programTree := p.parseMacros(nil)
    if err != nil {
        return err
    }
	tree.Ramas = programTree
	fmt.Print(tree)
    return nil
}

func Gixparse(text string) bool {
    l := gixlexer.NewLexer(gixlexer.NewBuffer(text))
    p := NewParser(l)
	err := p.Parse()
	if err != nil {
		fmt.Printf("syntax error in line %v: %v\n", p.l.GetLine(), err)
	}
    return err == nil
}
