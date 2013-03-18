$PBExportHeader$nv_term.sru
forward
global type nv_term from nonvisualobject
end type
end forward

global type nv_term from nonvisualobject
end type
global nv_term nv_term

type variables

string text				//what the term contains
integer kind			//nature of the term (see constants below)

any value				//the result of evaluation
integer valtype			//the type of evaluated value e.g. an IDENT can contain a BOOL

long count				//number of args for a function
long position			//node position in the input string  TODO REMOVE ??
nv_term root			//parent node
nv_term childs[]		//child nodes

//types of tokens
constant integer UNDEF = 0		//undefined
constant integer INTG = 1		//integer numeric type
constant integer DECIM = 2		//decimal numeric type
constant integer BOOL = 3		//boolean type
constant integer STR = 4		//string type
constant integer ERR = 5		//error
//date?

constant integer ATOM = 10		//generic value type
constant integer UNARYOP = 11	//unary operator
constant integer BINARYOP = 12	//binary operator 
constant integer IDENT = 13		//identifier (variable)
constant integer FUNC = 14		//function
constant integer TARGSEP = 15	//argument separator
constant integer TLPAR = 16		//left parenthesis
constant integer TRPAR = 17		//right parenthesis

end variables

forward prototypes
public function string tostring ()
public function string typename ()
public function boolean tobool ()
public function decimal todec ()
public function decimal tolong ()
public function string dump ()
public function boolean iscompatiblewith (nv_term at_other)
public function string typename (readonly nv_term a_term)
end prototypes

public function string tostring ();
string ls_ret

if classname(value) = "string" then
	ls_ret = value
else
	//Coercion ?
end if

return ls_ret

end function

public function string typename ();
//return the stringified type 

return typename(this)

end function

public function boolean tobool ();
boolean lb_ret

if classname(value) = "any" then
	lb_ret = value
else
	//Coercion ?
end if

return lb_ret

end function

public function decimal todec ();
dec ldc_ret

if classname(value) = "decimal" then
	ldc_ret = value
else
	//Coercion ?
end if

return ldc_ret

end function

public function decimal tolong ();
long ll_ret

if classname(value) = "long" then
	ll_ret = value
else
	//Coercion ?
end if

return ll_ret

end function

public function string dump ();
//return a string representation of a token
string ls_ret

if kind = ATOM then
	if valtype = BOOL or valtype = DECIM or valtype = INTG then
		ls_ret = string(value)
	elseif valtype = STR then
		ls_ret = /*'"' +*/ text /*+ '"'*/
	end if
elseif kind = FUNC then
	ls_ret = text + '.' + iif(count > -1, string(count), '?')
elseif kind = UNARYOP then
	if text = '-' then 
		ls_ret = '_'
	else
		ls_ret = text
	end if
else
	ls_ret = text
end if

int i, max
max = upperbound(childs[])
if max > 0 then
	ls_ret += '('
	for i = 1 to max
		ls_ret += childs[i].dump()
		if i < max then ls_ret += ' '
	next
	ls_ret += ')'	
end if

return ls_ret

end function

public function boolean iscompatiblewith (nv_term at_other);
choose case valtype
	case INTG, DECIM
		if at_other.valtype = INTG or at_other.valtype = DECIM then
			return true
		else
			return false
		end if
	case else
		return kind = at_other.kind and valtype = at_other.valtype
end choose

end function

public function string typename (readonly nv_term a_term);
//return the stringified type 
string ls_name

choose case a_term.kind
	case ATOM;
		ls_name = "Atom"
		choose case a_term.valtype
			case INTG;		ls_name += "-Int"		//numeric type
			case DECIM;		ls_name += "-Dec"		//numeric type
			case BOOL;		ls_name += "-Bool"		//boolean type
			case STR;		ls_name += "-String"	//string type
		end choose
	case UNARYOP;	ls_name = "UnOp"				//unary operator
	case BINARYOP;	ls_name = "BinOp"				//binary operator 
	case IDENT;		ls_name = "Id"					//identifier (variable)
	case FUNC;		ls_name = "Func"				//function
	case TARGSEP, TLPAR, TRPAR;		ls_name = ""
	case ERR;		ls_name = "Err"					//error
end choose

return ls_name

end function

on nv_term.create
call super::create
TriggerEvent( this, "constructor" )
end on

on nv_term.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

event constructor;
valtype = UNDEF

end event

