//Tests para el parser de gix
package gixparse

import "testing"

var test1 = `
macro line(int a, float b, float h){
	int e;
	float d;
	loop c:0, 4{
		a = c;
		rect(a, a, 4, 3);
	}
	loop j:1, 8{
		rect(j, j, 0.4, 4);
		loop k:2, 8{
			rect(j, j, 0.4, 5);
		}
	}
	circle(4, 5, 2, 6);
}
macro main(){
	loop i:0, 3{
		rect(i, i, 0.4, 7);
	}
	loop j:3, 8{
		rect(j, j, 0.4, 8);
		rect(j, j, 0.4, 9);
	}
}`

var test2 = `
macro line(int x, int y) {
	int k;
	k = 2;
	loop i:0,x{
		circle(2, y, 5);
	}
	
}`

func Test_GixParse_1 (t *testing.T) {
	if Gixparse(test1) {
		t.Log("first test passed")
	} else {
		t.Error("first test failed")
		}
}

func Test_GixParse_2 (t *testing.T) {
	if Gixparse(test2) {
		t.Log("second test passed")
	} else {
		t.Error("second test failed")
		}
}
