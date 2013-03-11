$PBExportHeader$nv_termqueue.sru
forward
global type nv_termqueue from nonvisualobject
end type
end forward

global type nv_termqueue from nonvisualobject
end type
global nv_termqueue nv_termqueue

type variables

private:

nv_term ia_values[]
long il_start = 1, il_end = 0

end variables

forward prototypes
public function boolean isempty ()
public subroutine push (nv_term aa_val)
public subroutine reset ()
public function long size ()
public function nv_term peek (long al_pos)
public function nv_term shift ()
public function nv_term pop ()
end prototypes

public function boolean isempty ();
return il_end = 0

end function

public subroutine push (nv_term aa_val);
if il_end = 0 then 
	il_end = il_start
else
	il_end ++
end if

ia_values[il_end] = aa_val

end subroutine

public subroutine reset ();
nv_term empty[]

ia_values[] = empty[]
il_start = 1
il_end = 0

end subroutine

public function long size ();
if il_end = 0 then
	return 0
else
	return il_end - il_start + 1
end if

end function

public function nv_term peek (long al_pos);
// for debug : return a value from given position from start
nv_term la_val

if abs(al_pos) <= size() then
	if al_pos >= 0 then
		la_val = ia_values[il_start + al_pos - 1]
	else
		la_val = ia_values[il_end + al_pos + 1 ]
	end if
end if

return la_val

end function

public function nv_term shift ();
nv_term la_ret

if il_end = 0 then return la_ret

la_ret = ia_values[il_start]
setnull(ia_values[il_start])
il_start ++

if il_start > il_end then il_end = 0

return la_ret

end function

public function nv_term pop ();
nv_term la_ret

if il_end = 0 then return la_ret

la_ret = ia_values[il_end]
setnull(ia_values[il_end])
il_end --

if il_start > il_end then il_end = 0

return la_ret

end function

on nv_termqueue.create
call super::create
TriggerEvent( this, "constructor" )
end on

on nv_termqueue.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

