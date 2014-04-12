input common;
def draw_arr(expr a) =
  begingroup
    save m;
    m=(1+ahlength/(arclength a))/2;
    drawarrow subpath (0,m) of a;
    draw subpath (m,1) of a;
  endgroup;
enddef;

w=20*xu;
h=15*yu;

def feyn =
  pair l[];
  pair r[];
  pair v[];
  path p[];
  l1=(0,0);
  l2=(0,h);
  r1=(w,0);
  r2=(w,h/3);
  r3=(w,2*h/3);
  r4=(w,h);
  v1=(w/3,0);
  v2=(2*w/3,h/2);
  v3=(w/3,h);
  p1=fullcircle scaled (3*unit) shifted v1;
  p2=fullcircle scaled (3*unit) shifted v2;
  p3=fullcircle scaled (3*unit) shifted v3;
  draw_arr ((l1 -- v1) cutafter p1);
  draw_arr ((v1 -- r1) cutbefore p1);
  draw_arr ((l2 -- v3) cutafter p3);
  draw_arr ((v3 -- r4) cutbefore p3);
  draw_arr ((v3 -- v2) cutbefore p3 cutafter p2);
  draw_arr ((v1 -- v2) cutbefore p1 cutafter p2);
  draw_arr ((v2 -- r2) cutbefore p2);
  draw_arr ((v2 -- r3) cutbefore p2);
  draw p1;
  draw p2;
  draw p3;
enddef;

beginfig(1)
  feyn;
  label(TEX("$P_1^{(k)}$"), (l1+v1)/2+(-xu,-yu));
  label(TEX("$P_2^{(k)}$"), (l2+v3)/2+(-xu,yu));
  label(TEX("$P_1^{(k+1)}$"), (v1+r1)/2+(xu,-yu));
  label(TEX("$P_2^{(k+1)}$"), (v3+r4)/2+(xu,yu));
  label(TEX("$\hat{p}_1^{(k)}$"), (v1+v2)/2+(-xu,yu));
  label(TEX("$\hat{p}_2^{(k)}$"), (v3+v2)/2+(-xu,-yu));
  label(TEX("$a^{(k)}$"), (v2+(-2*xu,-1*yu)));
  label(TEX("$b^{(k)}$"), (v2+(-2*xu,1*yu)));
  label(TEX("$c^{(k)}$"), (v2+(2*xu,-1.5*yu)));
  label(TEX("$d^{(k)}$"), (v2+(2*xu,1.5*yu)));
  label(TEX("$f_a^{(k)}$"), v1);
  label(TEX("$\hat\sigma$"), v2);
  label(TEX("$f_b^{(k)}$"), v3);
  pickup pencircle scaled 0pt;
  drawdot(-xu,-yu);
  drawdot(w+xu,h+yu);
endfig;

beginfig(2)
  feyn;
  label(TEX("$X_1$"), (l1+v1)/2+(-xu,-yu));
  label(TEX("$X_2$"), (l2+v3)/2+(-xu,yu));
  label(TEX("$\xi_1 X_1$"), (v1+v2)/2+(-xu,yu));
  label(TEX("$\xi_2 X_2$"), (v3+v2)/2+(-xu,-yu));
  label(TEX("$1$"), (v2+(-2*xu,-1*yu)));
  label(TEX("$2$"), (v2+(-2*xu,1*yu)));
  label(TEX("$3$"), (v2+(2*xu,-1.5*yu)));
  label(TEX("$4$"), (v2+(2*xu,1.5*yu)));
  label(TEX("$f_1$"), v1);
  label(TEX("$\hat\sigma$"), v2);
  label(TEX("$f_2$"), v3);
  pickup pencircle scaled 0pt;
  drawdot(-xu,-yu);
  drawdot(w+xu,h+yu);
endfig;

beginfig(3)
  draw (0,unit) -- (4,1)*unit;
  draw (0,0)--((1,0)*unit){dir 0} .. {dir 90}((2,1.5)*unit) ..{dir 0}((3,3)*unit)--(4,3)*unit;
  draw (0,4*unit) -- (4*unit,4*unit);
  drawdot (0,3)*unit;
  drawdot (4,1)*unit;
  label(TEX("$0$"), ((-1,3)*unit));
  label(TEX("$3$"), ((5,1)*unit));
  label(TEX("$3$"), ((-1,1)*unit));
  label(TEX("$4$"), ((5,3)*unit));
  label(TEX("$4$"), ((-1,0)*unit));
  label(TEX("$0$"), ((5,0)*unit));
  label(TEX("$5$"), ((-1,4)*unit));
  label(TEX("$5$"), ((5,4)*unit));
  
  label(TEX("$1$"), ((-2,0.5)*unit));
  draw fullcircle scaled (1.1*unit) shifted ((-2,0.5)*unit);
  label(TEX("$2$"), ((-2,3.5)*unit));
  draw fullcircle scaled (1.1*unit) shifted ((-2,3.5)*unit);
  label(TEX("$3$"), ((6,0.5)*unit));
  draw fullcircle scaled (1.1*unit) shifted ((6,0.5)*unit);
  label(TEX("$4$"), ((6,3.5)*unit));
  draw fullcircle scaled (1.1*unit) shifted ((6,3.5)*unit);
endfig;

def draw_splitting=
  numeric w,h;
  w=6*xu;
  h=6*yu;
  path p[];
  pair v[];
  v1=(unit,h-unit);
  v2=(w-unit,unit);
  p1=fullcircle scaled (2*unit) shifted v1;
  p2=fullcircle scaled (2*unit) shifted v2;
  v3=(v1--v2) intersectionpoint p1;
  v4=(v1--v2) intersectionpoint p2;
  v5=(v3+v4)/2;
  v6=(w,ypart(v5));
  draw p1;
  draw p2;
  draw v3 -- v4;
  label(TEX("$f$"), v1);
  label(TEX("$\widehat{\sigma}$"), v2);
enddef;

beginfig(4)
  draw_splitting;
  label(TEX("$1$"), (v5-(0.5,0.5)*unit));
endfig;

beginfig(5)
  v7=(w/4,h/2);
  v8=(3*w/4,h/2);
  draw v7 -- v8;
  draw (v8+dir(135)*unit) -- v8 --(v8+dir(225)*unit);
  label(TEX("ISR"), (v7+v8+(0,unit))/2);
  pickup pencircle scaled 0pt;
  drawdot (0,0);
  drawdot (w,h);
endfig;

beginfig(6)
  draw_splitting;
  draw v5--v6;
  label(TEX("$1$"), (v4+v5-(unit,unit))/2);
  label(TEX("$2$"), (v3+v5-(unit,unit))/2);
  label(TEX("$3$"), (v5+v6+(0,unit))/2);
endfig;
end;
