forward
global type nv_parser from nonvisualobject
end type
end forward

global type nv_parser from nonvisualobject autoinstantiate
end type

type variables

private:

any ia_tokens[]
any ia_poltok[]
boolean ib_reverse = true

string is_lasterror

constant char CC_LEFT = 'L'
constant char CC_RIGHT = 'R'

end variables

forward prototypes
public subroutine setreverse (boolean ab_reverse)
public function boolean gettokens (ref any aa_tokens[])
public function boolean polish ()
public function string getlasterror ()
public function boolean getparsed (ref any aa_expr[])
public function boolean tokenize (string as_input)
public function boolean isfunc (any aa_tok)
public function boolean isop (any aa_tok)
public function integer getprec (any aa_op)
public function character getassoc (any aa_op)
public function boolean iswordchar (character ac_char)
public function string eval ()
end prototypes

public subroutine setreverse (boolean ab_reverse);
ib_reverse = ab_reverse

end subroutine

public function boolean gettokens (ref any aa_tokens[]);
aa_tokens[] = ia_tokens[]

return true

end function

public function boolean polish ();
boolean lb_ret = true
any la_tok, empty_toks[]
nv_queue lq_out
nv_stack lst_op

is_lasterror = ""

if upperbound(ia_tokens[]) < 1 then
	is_lasterror = "no input to polish"
	lb_ret = false
	goto end_of_shunt
end if

long t = 1
do while t <= upperbound(ia_tokens[])
	la_tok = ia_tokens[t]
	
	if isnumber(la_tok) then		//number ?
		lq_out.push(la_tok)
	elseif isfunc(la_tok) then		//func ?
		lst_op.push(la_tok)
	elseif la_tok = ';' then		//arg separator ?
		do while not lst_op.isempty( ) and string(lst_op.top( ),"[general]") <> '('
			lq_out.push(lst_op.pop())
		loop
		if lst_op.isempty( ) or string(lst_op.top( ),"[general]") <> '(' then
			is_lasterror = "bad arg separator or parenthesis mismatch"
			lb_ret = false
			goto end_of_shunt
		end if
	elseif isop(la_tok) then	//O1
		do while not lst_op.isempty( ) /*O2*/ &
			and ((getassoc(la_tok) = cc_left and getprec(la_tok) <= getprec(lst_op.top())) &
					or &
					(getassoc(la_tok) = cc_right and getprec(la_tok) < getprec(lst_op.top()))) 
			lq_out.push(lst_op.pop())
		loop
		lst_op.push(la_tok)
	elseif la_tok = '(' then
		lst_op.push(la_tok)
	elseif la_tok = ')' then
		do while not lst_op.isempty( ) and lst_op.top() <> '('
			lq_out.push(lst_op.pop())
		loop
		if lst_op.top() = '(' then 
			lst_op.pop( )
		else
			is_lasterror = "parenthesis error"
			lb_ret = false
			goto end_of_shunt
		end if
		if isfunc(lst_op.top()) then lq_out.push(lst_op.pop())
	end if
	t++
loop
do while not lst_op.isempty( )
	if lst_op.top( ) = '(' or lst_op.top( ) = ')' then
		is_lasterror = "parenthesis error"
		lb_ret = false
		goto end_of_shunt
	end if
	lq_out.push(lst_op.pop( ))
loop

//final 
ia_poltok[] = empty_toks[]
if ib_reverse then
	//produce the postfixed stream
	do while not lq_out.isempty( )
		ia_poltok[upperbound(ia_poltok[]) + 1] = lq_out.pop( )
	loop
else
	//reverse the reversed => prefixed stream
	do while not lq_out.isempty( )
		lst_op.push(lq_out.pop())
	loop
	do while not lst_op.isempty( )
		ia_poltok[upperbound(ia_poltok[]) + 1] = lst_op.pop( )
	loop
end if

end_of_shunt:
return lb_ret

end function

public function string getlasterror ();
return is_lasterror

end function

public function boolean getparsed (ref any aa_expr[]);
aa_expr[] = ia_poltok[]

return true

end function

public function boolean tokenize (string as_input);
boolean lb_ret = true
long c = 1, p, size, count = 0
string ls_tok
any la_empty[]

is_lasterror = ""
ia_tokens = la_empty[]
size = len(as_input)

do while c <= size
	ls_tok = ""
	p = c
	
	choose case mid(as_input, c, 1)
		case '0' to '9' 
			do while (p <= size) and isNumber(mid(as_input, c, p - c + 1))
				p++
			loop 
			ls_tok = trim(mid(as_input, c, p - c))
		case '+', '-', '/', '^', '(', ')'
			p++
			ls_tok = mid(as_input, c, p - c)
		case '*'
			p++
			ls_tok = 'x'
		case 'A' to 'Z', 'a' to 'z', '_'
			do while (p <= size) and isWordChar(mid(as_input, p, 1))
				p++
			loop 
			ls_tok = trim(mid(as_input, c, p - c))
		case ' ' 
			c++		//ignore blanks
			continue
		case else
			is_lasterror = "possible unexpected char"
			lb_ret = false
			goto end_of_tok
	end choose
	ia_tokens[upperbound(ia_tokens[]) + 1] = ls_tok
	if p = c then 
		is_lasterror = "parsing error"
		lb_ret = false
		goto end_of_tok
	end if
	c = p
loop

end_of_tok:
return lb_ret

end function

public function boolean isfunc (any aa_tok);
boolean lb_ret = false

choose case lower(aa_tok)
	case 'test'; lb_ret = true
end choose

return lb_ret

end function

public function boolean isop (any aa_tok);
boolean lb_ret 

choose case aa_tok
	case '+', '-', '*', '/', '^'
		lb_ret = true
		
	case else
		lb_ret = false
end choose

return lb_ret

end function

public function integer getprec (any aa_op);
int li_ret

choose case string(aa_op, "[general]")
	case '^'; 		li_ret = 4
	case '*', '/';	li_ret = 3
	case '+', '-';	li_ret = 2
	case else;		li_ret = 0
end choose

return li_ret

end function

public function character getassoc (any aa_op);
char lc_ret

choose case string(aa_op, "[general]")
	case '^'
		lc_ret = CC_RIGHT
	case '*', '/'
		lc_ret = CC_LEFT
	case '+', '-'
		lc_ret = CC_LEFT
	case else
		lc_ret = CC_LEFT
end choose

return lc_ret

end function

public function boolean iswordchar (character ac_char);
choose case ac_char
	case 'a' to 'z', 'A' to 'Z', '0' to '9', '_'
		return true

	case else
		return false
end choose

end function

public function string eval ();
//

return ""

end function

on nv_parser.create
call super::create
TriggerEvent( this, "constructor" )
end on

on nv_parser.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

