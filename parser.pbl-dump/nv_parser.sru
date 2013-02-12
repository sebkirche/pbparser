forward
global type nv_parser from nonvisualobject
end type
end forward

global type nv_parser from nonvisualobject autoinstantiate
end type

type variables

private:

st_tok ist_tokens[]
st_tok ist_poltok[]
boolean ib_reverse = true

string is_lasterror

constant char CC_LEFT = 'L'
constant char CC_RIGHT = 'R'

constant string NUMERIC = "Num"
constant string STR = "Str"
constant string OPERATION = "Op"
constant string IDENT = "Id"
constant string FUNC = "Func"

end variables

forward prototypes
public subroutine setreverse (boolean ab_reverse)
public function string getlasterror ()
public function boolean tokenize (string as_input)
public function boolean isfunc (any aa_tok)
public function boolean isop (any aa_tok)
public function integer getprec (any aa_op)
public function character getassoc (any aa_op)
public function boolean iswordchar (character ac_char)
public function boolean gettokens (ref st_tok ast_tokens[])
public function boolean getparsed (ref st_tok ast_expr[])
public function boolean polish (st_tok ast_tokens[])
public function string eval (st_tok ast_toks[])
end prototypes

public subroutine setreverse (boolean ab_reverse);
ib_reverse = ab_reverse

end subroutine

public function string getlasterror ();
return is_lasterror

end function

public function boolean tokenize (string as_input);
boolean lb_ret = true
long ll_tk_start = 1, ll_tk_end, ll_in_len
st_tok lst_tok
any la_empty[]
char lc

is_lasterror = ""
ist_tokens = la_empty[]
ll_in_len= len(as_input)

do while ll_tk_start <= ll_in_len
	lst_tok.value = ""
	lst_tok.kind = ""
	ll_tk_end = ll_tk_start
	
	lc = mid(as_input, ll_tk_start, 1)
	choose case lc
		case '0' to '9' 
			do while (ll_tk_end <= ll_in_len) and isNumber(mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start + 1))
				ll_tk_end++
			loop 
			lst_tok.value = trim(mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start))
			lst_tok.kind = NUMERIC
		case '+', '-', '*', '/', '^', '(', ')'
			ll_tk_end++
			lst_tok.value = mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start)
			lst_tok.kind = OPERATION
		case 'A' to 'Z', 'a' to 'z', '_'
			do while (ll_tk_end <= ll_in_len) and isWordChar(mid(as_input, ll_tk_end, 1))
				ll_tk_end++
			loop 
			lst_tok.value = trim(mid(as_input, ll_tk_start, ll_tk_end - ll_tk_start))
			lst_tok.kind = IDENT
		case ' ' 
			ll_tk_start++		//ignore blanks
			continue
		case else
			is_lasterror = "possible unexpected char : " + lc
			lb_ret = false
			goto exit_tokenize
	end choose
	lst_tok.position = ll_tk_start
	ist_tokens[upperbound(ist_tokens[]) + 1] = lst_tok
	if ll_tk_end = ll_tk_start then 
		is_lasterror = "parsing error"
		lb_ret = false
		goto exit_tokenize
	end if
	ll_tk_start = ll_tk_end
loop

exit_tokenize:
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

public function boolean gettokens (ref st_tok ast_tokens[]);
ast_tokens[] = ist_tokens[]

return true

end function

public function boolean getparsed (ref st_tok ast_expr[]);
//bug pour les structures ? 
ast_expr[] = ist_poltok[]
//long i, n
//n = upperbound(ist_poltok[])
//for i = 1 to n
//	ast_expr[i] = ist_poltok[i]
//next

return true

end function

public function boolean polish (st_tok ast_tokens[]);
boolean lb_ret = true
st_tok lst_tok, empty_toks[]
nv_queue lq_out
nv_tokstack lst_op

is_lasterror = ""

if upperbound(ast_tokens[]) < 1 then
	is_lasterror = "no input to polish"
	lb_ret = false
	goto end_of_shunt
end if

long t = 1
do while t <= upperbound(ast_tokens[])
	lst_tok = ast_tokens[t]
	
	if isnumber(lst_tok.value) then		//number ?
		lq_out.push(lst_tok)
	elseif isfunc(lst_tok.value) then		//func ?
		lst_tok.kind = FUNC
		lst_op.push(lst_tok)
	elseif lst_tok.value = ';' then		//arg separator ?
		do while not lst_op.isempty( ) and lst_op.top().value <> '('
			lq_out.push(lst_op.pop())
		loop
		if lst_op.isempty( ) or lst_op.top().value <> '(' then
			is_lasterror = "bad arg separator or parenthesis mismatch"
			lb_ret = false
			goto end_of_shunt
		end if
	elseif isop(lst_tok.value) then	//O1
		do while not lst_op.isempty( ) /*O2*/ &
			and ((getassoc(lst_tok.value) = cc_left and getprec(lst_tok.value) <= getprec(lst_op.top().value)) &
					or &
					(getassoc(lst_tok.value) = cc_right and getprec(lst_tok) < getprec(lst_op.top().value))) 
			lq_out.push(lst_op.pop())
		loop
		lst_op.push(lst_tok)
	elseif lst_tok.value = '(' then
		lst_op.push(lst_tok)
	elseif lst_tok.value = ')' then
		do while not lst_op.isempty( ) and lst_op.top().value <> '('
			lq_out.push(lst_op.pop())
		loop
		if lst_op.top().value = '(' then 
			lst_op.pop()
		else
			is_lasterror = "parenthesis error"
			lb_ret = false
			goto end_of_shunt
		end if
		if isfunc(lst_op.top().value) then lq_out.push(lst_op.pop())
	end if
	t++
loop
do while not lst_op.isempty( )
	if lst_op.top().value = '(' or lst_op.top().value = ')' then
		is_lasterror = "parenthesis error"
		lb_ret = false
		goto end_of_shunt
	end if
	lq_out.push(lst_op.pop( ))
loop

//final 
ist_poltok[] = empty_toks[]
if ib_reverse then
	//produce the postfixed stream
	do while not lq_out.isempty( )
		ist_poltok[upperbound(ist_poltok[]) + 1] = lq_out.pop( )
	loop
else
	//reverse the reversed => prefixed stream
	do while not lq_out.isempty( )
		lst_op.push(lq_out.pop())
	loop
	do while not lst_op.isempty( )
		ist_poltok[upperbound(ist_poltok[]) + 1] = lst_op.pop( )
	loop
end if

end_of_shunt:
return lb_ret

end function

public function string eval (st_tok ast_toks[]);
nv_tokstack lst_eval
st_tok item
dec ldc_op1, ldc_op2, ldc_res
string ls_ret = "", ls_operation
long i, n

n = upperbound(ast_toks[]) 
if n = 0 then return ""

for i = 1 to n
	choose case ast_toks[i].kind
		case NUMERIC
			lst_eval.push(ast_toks[i])
		case OPERATION
			ls_operation = ast_toks[i].value
			choose case ls_operation
				case '+', '-', '*', '/', '^'
					if lst_eval.size() >= 2 then
						ldc_op2 = dec(lst_eval.pop().value)
						ldc_op1 = dec(lst_eval.pop().value)
						if ls_operation = '+' then
							ldc_res = ldc_op1 + ldc_op2
						elseif ls_operation = '-' then
							ldc_res = ldc_op1 - ldc_op2
						elseif ls_operation = '*' then
							ldc_res = ldc_op1 * ldc_op2
						elseif ls_operation = '/' then
							ldc_res = ldc_op1 / ldc_op2
						elseif ls_operation = '^' then
							ldc_res = ldc_op1 ^ ldc_op2
						end if
						item.value = ldc_res
						item.kind = NUMERIC
						lst_eval.push(item)
					else
						ls_ret = "broken expression : not enough operands for " + ls_operation + " at " + string(ast_toks[i].position)
						goto end_eval
					end if
				case else
					ls_ret = "unknown op " + ls_operation
					goto end_eval
			end choose
	end choose
next
if lst_eval.size() = 1 then
	item = lst_eval.pop()
	if item.kind = NUMERIC then
		ls_ret = string(item.value)
	elseif item.kind = STR then
		ls_ret = item.value
	else
		ls_ret = "not sure how to process " + item.kind + " `" + string(item.value) + "`"
	end if
else
	ls_ret = "evaluation failed"
end if

end_eval:
return ls_ret

end function

on nv_parser.create
call super::create
TriggerEvent( this, "constructor" )
end on

on nv_parser.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

