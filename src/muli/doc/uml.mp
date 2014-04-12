outputtemplate := "%j-%c.mps";
input TEX;
TEXPRE("%&latex" & char(10) & "\documentclass{article}\begin{document}");
TEXPOST("\end{document}");
unit=10;
xu=unit;
yu=unit;

path item[][];

def item_path(expr a, b, c)= 
  ((subpath (0,(length fullcircle)/2)  of fullcircle) scaled (2*unit) rotated 90 shifted (-b*unit,0) --
      (subpath (0,(length fullcircle)/2)  of fullcircle) scaled (2*unit) rotated 270 shifted (b*unit,0) -- cycle) shifted (a*unit);
  label(TEX(c), (a*unit));
enddef;

def decision_path(expr a, b)=  
  ((2*unit,0) -- (0,2*unit) -- (-2*unit,0) -- (0,-2*unit) -- cycle) shifted (a*unit);
  label(TEX(b), (a*unit));
enddef;

def set_begin(expr a)=
  item[xpart(a)][ypart(a)]=fullcircle scaled unit shifted (a*unit);
enddef;

def set_end(expr a)=
  item[xpart(a)][ypart(a)]=fullcircle scaled unit shifted (a*unit);
enddef;

def draw_arrow(expr a,b)=
  drawarrow (a*unit -- b*unit);
enddef;

beginfig(1)
  fill fullcircle scaled unit shifted ((11,36.5)*unit);
  draw_arrow((11,36),(11,35));
  draw item_path((11,34), 5,"initialize");
  draw_arrow((11,33),(11,32));
  draw item_path((11,31), 5,"apply\_initial\_interaction");
  draw_arrow((11,30),(11,29));
  draw item_path((11,28), 5,"generate\_gev2\_pt2");
  draw_arrow((11,27),(11,26));  
  draw decision_path((11,24), "$p_{\perp}\!\!<\!t$");
  label(TEX("yes"), ((14,24.5)*unit));
  label(TEX("no"), ((8,24.5)*unit));
  draw((9,24)*unit--(5,24)*unit);
  draw_arrow((5,24),(5,21));
  draw item_path((5,20), 4,"generate\_partons");
  draw_arrow((5,19),(5,18));
  draw item_path((5,17), 4,"get\_correlations");
  draw((13,24)*unit--(17,24)*unit);
  draw_arrow((17,24),(17,19.5));
  draw item_path((17,18.5), 4,"replace\_partons");
  draw (5,16)*unit -- (5,15)*unit;
  draw (17,17.5)*unit -- (17,15)*unit;
  draw (5,15)*unit -- (17,15)*unit;
  draw_arrow((11,15),(11,14));
  draw decision_path((11,12), "\small $p_{\perp}\!\!<\!p_{\perp}^{min}$");
  label(TEX("yes"), ((12,9.5)*unit));
  label(TEX("no"), ((13.5,13)*unit));
  draw_arrow((11,10),(11,9));
  draw decision_path((11,7), "\small $m\!>\!m_E$");
  label(TEX("yes"), ((12,4.5)*unit));
  label(TEX("no"), ((13.5,8)*unit));
  draw_arrow((11,5),(11,4));
  draw item_path((11,3), 4, "finalize");
  draw_arrow((11,2),((11,1)));
  draw fullcircle scaled unit shifted ((11,0.5)*unit);
  fill fullcircle scaled (unit*0.5) shifted ((11,0.5)*unit);

  draw_arrow((13,12),(14,12));
  draw item_path((18,12), 3,"replace\_partons");
  draw_arrow((22,12),(23,12));
  draw decision_path((25,12), "\small $t\!\!<\!t^{min}$");
  label(TEX("yes"), ((26,14.5)*unit));
  label(TEX("no"), ((26,9.5)*unit));
  draw ((25,10)*unit -- (25,9)*unit -- (18,9)*unit);
  draw_arrow ((18,9),(18,11));
  draw ((25,14)*unit -- (25,28)*unit);
  draw_arrow ((25,28),(17,28));
  
  
  draw_arrow((13,7),(14,7));
  draw item_path((18,7), 3,"restart");
  draw (22,7)*unit -- (28,7)*unit;
  draw (28,7)*unit -- (28,31)*unit;
  draw_arrow((28,31),(17,31));

endfig;
end;
