uses dos,crt;

const
  ver='0.1';
  world : array[1..16,1..16] of byte=((1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
                                      (1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1),
                                      (1,0,0,1,1,1,1,1,0,0,0,0,0,0,0,1),
                                      (1,0,0,1,0,0,0,1,0,1,0,0,0,0,0,1),
                                      (1,0,0,1,0,0,0,1,0,0,1,0,1,0,0,1),
                                      (1,0,0,1,0,0,1,1,0,0,0,0,0,0,0,1),
                                      (1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1),
                                      (1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1),
                                      (1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1),
                                      (1,0,0,1,1,0,0,1,1,1,1,1,1,0,0,1),
                                      (1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,1),
                                      (1,0,0,1,1,1,0,0,0,0,0,0,1,0,0,1),
                                      (1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,1),
                                      (1,0,0,1,1,1,1,1,1,1,1,0,1,0,0,1),
                                      (1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1),
                                      (1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1));

  texture_size=64;

  cell_x_size=64;
  cell_y_size=64;

  angle_0  = 0;
  angle_30 = 160;
  angle_90 = 480;
  angle_180= 960;
  angle_270= 1440;
  angle_360= 1920;

  world_rows=16;
  world_columns=16;

  max=1921;

type

 p_inv_tan_table=array [0..max] of real;
 p_inv_sin_table=array [0..max] of real;
 p_inv_cos_table=array [0..max] of real;
 p_tan_table=array [0..max] of real;
 p_cos_table=array [0..max] of real;
 p_y_step=array [0..max] of real;
 p_x_step=array [0..max] of real;
 p_scr=array[0..64000] of byte;

var
 inv_tan_table:^p_inv_tan_table;
 inv_sin_table:^p_inv_sin_table;
 inv_cos_table:^p_inv_cos_table;
 tan_table:^p_tan_table;
 cos_table:^p_cos_table;
 y_step:^p_y_step;
 x_step:^p_x_step;
 scr:^p_scr;

 texture:array[1..64,1..64] of byte;

var
  cell_x,cell_y,
  x_bound,y_bound,
  x_delta,y_delta,
  next_x_cell,next_y_cell,
  xb_save,yb_save,
  ray: integer;

  dist_x,dist_y,
  xi_save,yi_save,
  xi,yi: real;

  casting,xray,yray:byte;

  i  : word;                     {TU SA CIEKAWE ZMIENNE}
  lut : array[0..199] of word;
  palette: array[0..767] of byte;
  loop1:word;
  key:integer;
  old_keybord:pointer;


procedure new_keybord; interrupt;
begin
  asm
     sti
     in  al,$60
     xor ah,ah
     mov key,ax
     in  al,$61
     or  al,$82
     out $61,al
     and al,$7f
     out $61,al
     mov al,$20
     out $20,al
  end;
end;

procedure dopal(col,r,g,b:byte); assembler;
asm
   mov    dx,3c8h
   mov    al,[col]
   out    dx,al
   inc    dx
   mov    al,[r]
   out    dx,al
   mov    al,[g]
   out    dx,al
   mov    al,[b]
   out    dx,al
end;

procedure makepalette;

begin
  palette[0]:=0;
  palette[1]:=0;
  palette[2]:=0;
  for loop1:=0 to 255 do
  dopal(loop1,
  palette[loop1*3] shr 2,palette[loop1*3+1] shr 2,palette[loop1*3+2] shr 2);
end;


procedure load_file;

var         f: file of byte;
    num1,num2: word;
           ok: boolean;
        kolor: byte;
begin
 assign(f,'graph.gfx');
 {$I-} reset(f); {$I+}
       ok:=ioresult=0;
 if not ok then
  begin
    writeln('ERROR: graph.gfx file not found');
  end;
 for num1:=0 to 767 do
   begin
     read(f,kolor);
     palette[num1]:=kolor;
   end;
 for num1:=1 to 64 do
 for num2:=1 to 64 do
   begin
     read(f,kolor);
     texture[num1,num2]:=kolor;
   end;
 close(f);
end;


procedure tryb_graficzny; assembler;
asm
 mov ax,13h
 int 10h
end;

procedure tryb_textowy; assembler;
asm
 mov ax,03h
 int 10h
end;


var j:word;
    c:byte;


procedure nadaj_kolor;
begin
 c:=texture[j*texture_size div k,tix1];
end;


procedure rysuj_linie(length:real; linia:word; horacy:real);

var k    : word;
    tix1 : word;
    top,bottom: word;

begin


 length:=8000/length*cos_table^[ray];
 k:=round(length);
 tix1:=round(horacy);

    top:=100-(k div 2);
 bottom:=top+k;
 if top < 0 then top:=1;
 if bottom > 200 then bottom:=200;


asm
 sti
 mov c,100
 jmp @start
 {@nadaj_kolor:}
 cmp cx,top
 jb @podloga
 ja @sciana
 @podloga:
 mov c,200
 ret
 @sciana:
 cmp cx,bottom
 ja @sufit
 mov c,100
 ret
 @sufit:
 mov c,155
 ret
 @start:
 mov di,linia
 push 0A000h
 pop es
 mov cx,200
 @petla:
 mov j,cx
 call @nadaj_kolor
 mov al,byte ptr c
 mov es:[di],al
 add di,320
 loop @petla
 cli
end;
end;

procedure zaladuj_tablice;

var ang       :integer;
    rad_angle :real;

begin
for ang:=angle_0 to angle_360 do
  begin
    rad_angle:=(3.272e-4)+ang*2*pi/angle_360;

    tan_table^[ang]:=sin(rad_angle)/cos(rad_angle);
    inv_tan_table^[ang]:=1/tan_table^[ang];

    if (ang >= angle_0) and (ang < angle_180) then
    y_step^[ang]:= abs(tan_table^[ang]*cell_y_size)
    else y_step^[ang]:=-abs(tan_table^[ang]*cell_y_size);

    if (ang >= angle_90) and (ang < angle_270) then
    x_step^[ang]:=-abs(inv_tan_table^[ang]*cell_x_size)
    else x_step^[ang]:=abs(inv_tan_table^[ang]*cell_x_size);

    inv_cos_table^[ang]:=1/cos(rad_angle);
    inv_sin_table^[ang]:=1/sin(rad_angle);
  end;

for ang:=-angle_30 to angle_30 do
  begin
    rad_angle:=(3.272e-4)+ang*2*pi/angle_360;
    cos_table^[ang+angle_30]:=(1/cos(rad_angle))*2;
  end;
end;

procedure trace(x,y,angle:longint);
begin

 {obliczanie pierwszego przeciecia}

 angle:=angle-angle_30;
 if angle < 0 then angle:=angle+angle_360;

 for ray:=0 to 319 do
  begin
   if (angle >= angle_0) and (angle < angle_180) then
    begin
      y_bound:=cell_y_size+cell_y_size*(y shr 6);
      y_delta:=cell_y_size;
      xi:=inv_tan_table^[angle]*(y_bound-y)+x;
      next_y_cell:=0;
    end
   else
    begin
      y_bound:=cell_y_size*(y shr 6);
      y_delta:=-cell_y_size;
      xi:=inv_tan_table^[angle]*(y_bound-y)+x;
      next_y_cell:=-1;
    end;
   if (angle < angle_90) or (angle >= angle_270) then
    begin
      x_bound:=cell_x_size+cell_x_size*(x shr 6);
      x_delta:=cell_x_size;
      yi:=tan_table^[angle]*(x_bound-x)+y;
      next_x_cell:=0;
    end
   else
    begin
      x_bound:=cell_x_size*(x shr 6);
      x_delta:=-cell_x_size;
      yi:=tan_table^[angle]*(x_bound-x)+y;
      next_x_cell:=-1;
    end;
  casting:=2;
  xray:=0;
  yray:=0;

while casting <> 0 do
begin
{                        promienie poziome                                   }
  if xray <> 1 then
   begin
    if (angle=angle_90) or (angle=angle_270) then
    { if abs(y_step^[angle])=0 then }
       begin
        xray:=1;
        dec(casting);
        dist_x:=100000000;
       end;
     cell_x:=(x_bound+next_x_cell) shr 6;
     cell_y:=round(yi) shr 6;
       if world[(world_rows-1)-cell_y,cell_x] = 1 then
         begin
           dist_x:=(yi-y)*inv_sin_table^[angle];
           yi_save:=yi;
           xb_save:=x_bound;
           xray:=1;
           dec(casting);
         end
          else yi:=yi+y_step^[angle]
   end;

{                        promienie pionowe                                   }
  if yray <> 1 then
   begin
    if (angle=angle_0) or (angle=angle_180) then
    { if abs(x_step^[angle]) = 0 then }
       begin
        yray:=1;
        dec(casting);
        dist_y:=100000000;
       end;
     cell_x:=round(xi) shr 6;      {round}
     cell_y:=(y_bound+next_y_cell) shr 6 ; {div}
       if world[(world_rows-1)-cell_y,cell_x] = 1 then
         begin
           dist_y:=(xi-x)*inv_cos_table^[angle];
           xi_save:=xi;
           yb_save:=y_bound;
           yray:=1;
           dec(casting);
         end
          else xi:=xi+x_step^[angle]
   end;
    x_bound:=x_bound+x_delta;
    y_bound:=y_bound+y_delta;
  {koniec pettli while}
 end;

  if dist_x < dist_y then
        rysuj_linie(dist_x,ray,yi_save)
     else
        rysuj_linie(dist_y,ray,xi_save);
  inc(angle);
  if angle  >= angle_360 then angle:=0;
  {koniec puszczania promieni}
  end;

end;


var x_actual,y_actual,kat:word;
    world_x,world_y:integer;
    accel:byte;
    xd,yd:real;

begin
 accel:=6;
 x_actual:=64*12;
 y_actual:=64*8;
 kat:=240;
 {pierwsza wstawka}
 clrscr;
 textcolor(white);
 textbackground(blue);
 for i:=1 to 80 do write(' ');
 gotoxy(27,1); write('Wolf 3d Engine v',ver,' by Melun');
 gotoxy(1,2);
 textcolor(7);
 textbackground(black);
 {alokujemy pamiec}
 write('Alocating Memory...');

 new(inv_tan_table);new(inv_sin_table);new(inv_cos_table);
 new(tan_table);new(cos_table);new(y_step);new(x_step);
 new(scr);
 write('OK');
 writeln;

 {**************zaladowanie tablic*************}
 write('Calculating tables...');
 zaladuj_tablice;                   {ladowanie reszty tablic}
 write('OK'); writeln;

 {tu sa pierdoly nad ktorymi trzeba jeszcze popracowac}
 tryb_graficzny;
 load_file;
 makepalette;
 {draw_shit;}
 trace(x_actual,y_actual,240);
 {move16;}

 {********obsluga klawiarury oraz main loop********}

 world_x:=world_columns shl 6;
 world_y:=world_rows shl 6;

 getintvec($09,old_keybord);
 setintvec($09,@new_keybord);

while key<>1 do
 begin

 if x_actual<1 then inc(x_actual,world_x)
 else if x_actual>world_x then dec(x_actual,world_x-1);

 if y_actual<1 then inc(y_actual,world_y)
 else if y_actual>world_y then dec(y_actual,world_y-1);

 if kat>1920 then dec(kat,1921)
 else if kat<0 then inc(kat,1921);


  case key of
 72: begin
       inc(y_actual,round(sin(2*pi*(kat/1920))*accel));
       inc(x_actual,round(cos(2*pi*(kat/1920))*accel));
     end;
 77: begin
       {xd:=-cos(2*pi*((kat-480)/1920))*accel/2;
       yd:=-sin(2*pi*((kat-480)/1920))*accel/2;
       x_actual:=x_actual+round(xd);
       y_actual:=y_actual+round(yd); }
       inc(kat,6);
     end;
 75: begin
       {xd:=-cos(2*pi*((kat+480)/1920))*accel/2;
       yd:=-sin(2*pi*((kat+480)/1920))*accel/2;
       x_actual:=x_actual+round(xd);
       y_actual:=y_actual+round(yd); }
       dec(kat,6);
     end;
 80: begin
       inc(y_actual,round(-sin(2*pi*(kat/1920))*accel));
       inc(x_actual,round(-cos(2*pi*(kat/1920))*accel));
     end
  end;
 {draw_shit;}
 trace(x_actual,y_actual,kat);
 {move16;}
end;

 setintvec($09,old_keybord);

 {**********koniec**********}
 {powrot do trybu textowego}
 tryb_textowy;
 {zwalniamy pamiec}
 dispose(inv_tan_table);dispose(inv_sin_table);dispose(inv_cos_table);
 dispose(tan_table);dispose(cos_table);dispose(y_step);dispose(x_step);
 dispose(scr);
end.
