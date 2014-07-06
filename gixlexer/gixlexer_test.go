//Tests para el lexer de gix
package gixlexer

import "testing"

var test1 = `
macro main(){
	loop i:0, 3{
		rect(i, i, 0.4, 0xff);
	}
	loop j:0, 8{
		rect(j, j, 0.4, 0xff);
	}
	circle(4, 5, -2, 0x11f0011);
}`

var solucion1 = `macro(macro)
nombre(main)
(
)
{
loop(loop)
nombre(i)
:
LitInt(0)
,
LitInt(3)
{
nombre(rect)
(
nombre(i)
,
nombre(i)
,
LitFloat(0.4)
,
LitInt(255)
)
;
}
loop(loop)
nombre(j)
:
LitInt(0)
,
LitInt(8)
{
nombre(rect)
(
nombre(j)
,
nombre(j)
,
LitFloat(0.4)
,
LitInt(255)
)
;
}
nombre(circle)
(
LitInt(4)
,
LitInt(5)
,
LitInt(-2)
,
LitInt(18808849)
)
;
}
eof
`

var test2 = `
macro line(int x, int y) {
	loop i:0,x{
		circle(2, 3, y, 5);
	}
}
`

var solucion2 = `macro(macro)
nombre(line)
(
TypeInt(int)
nombre(x)
,
TypeInt(int)
nombre(y)
)
{
loop(loop)
nombre(i)
:
LitInt(0)
,
nombre(x)
{
nombre(circle)
(
LitInt(2)
,
LitInt(3)
,
nombre(y)
,
LitInt(5)
)
;
}
}
eof
`

func Test_GixLexer_1 (t *testing.T) {
	if gixlexer(test1)==solucion1 {
		t.Log("first test passed")
	} else {
		t.Error("got " + gixlexer(test1) + " expected " + solucion1)
		}
}

func Test_GixLexer_2 (t *testing.T) {
	if gixlexer(test2)==solucion2 {
		t.Log("second test passed")
	} else {
		t.Error("got " + gixlexer(test2) + " expected " + solucion2)
		}
}
