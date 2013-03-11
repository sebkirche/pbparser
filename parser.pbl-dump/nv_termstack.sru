$PBExportHeader$nv_termstack.sru
forward
global type nv_termstack from nonvisualobject
end type
end forward

global type nv_termstack from nonvisualobject
end type
global nv_termstack nv_termstack

type variables

private:

nv_term it_values[]
long il_pos = 0

end variables
forward prototypes
public function boolean isempty ()
public subroutine reset ()
public function long size ()
public function nv_term peek (long al_pos)
public function nv_term pop ()
public subroutine push (nv_term at_val)
public function nv_term top ()
end prototypes

public function boolean isempty ();
return il_pos = 0

end function

public subroutine reset ();
nv_term empty[]

it_values[] = empty[]
il_pos = 0

end subroutine

public function long size ();
return il_pos

end function

public function nv_term peek (long al_pos);
// for debug : get a value

any la_val

if il_pos > 0 and abs(al_pos) <= il_pos then
	if al_pos > 0 then
		//positive => look from start
		la_val = it_values[al_pos]
	elseif al_pos < 1 then
		//negative or 0 => look from end
		la_val = it_values[il_pos + al_pos]
	end if
end if

return la_val

end function

public function nv_term pop ();
nv_term lt_ret

if il_pos = 0 then 
	lt_ret = create nv_term
	lt_ret.value = ""
	lt_ret.kind = 0
	return lt_ret	
end if

lt_ret = it_values[il_pos]
setnull(it_values[il_pos])
il_pos --

return lt_ret

end function

public subroutine push (nv_term at_val);
il_pos++
it_values[il_pos] = at_val

end subroutine

public function nv_term top ();
nv_term lt_ret

if il_pos = 0 then
	lt_ret = create nv_term
	lt_ret.value = ""
	lt_ret.kind = 0
	return lt_ret	
end if

lt_ret = it_values[il_pos]

return lt_ret

end function

on nv_termstack.create
call super::create
TriggerEvent( this, "constructor" )
end on

on nv_termstack.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

