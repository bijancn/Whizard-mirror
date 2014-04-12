input common;
color extends_color, comp_color;
extends_color=(1,0,0);
comp_color=(0,0,1);
path last_extends_path, mae_ser_path;
pair last_extends_endpoint, last_extends_beginning;
numeric last_extends_eangle, last_extends_bangle;

def draw_label(expr px,py,lab,offset)=
  label(TEX(lab),((px*xu,py*yu)+offset));
enddef;

def draw_line(expr s,ds,e,de,c,offset)=
  draw ((xpart(s)*xu,ypart(s)*yu)+offset){dir ds} .. {dir de}((xpart(e)*xu,ypart(e)*yu)+offset) withcolor c;
enddef;

def draw_extends_endpoint(expr parent, a, offset)=
  save d;
  save v;
  pair d;
  pair v[];
  pair last_extends_endpoint;
  numeric last_extends_eangle;
  last_extends_eangle=a;
  d=dir(a);
  v1=(xpart(parent)*xu,ypart(parent)*yu)+offset;
  v2=v1+(d rotated 150)*unit;
  v3=v1+(d rotated 210)*unit;
  last_extends_endpoint=(v2+v3)/2;
  draw v2 -- v1 -- v3 -- cycle withcolor extends_color;
enddef;

def draw_extends_line(expr child, ca, ten, offset)=
  path last_extends_path;
  pair last_extends_beginning;
  numeric last_extends_bangle;
  last_extends_bangle=ca;
  last_extends_beginning=(xpart(child)*xu,ypart(child)*yu)+offset;
  last_extends_path=last_extends_beginning{dir ca} .. tension ten .. {dir last_extends_eangle}last_extends_endpoint;
  draw last_extends_path withcolor extends_color;  
enddef;

def fork_extends_line(expr child, ca, ten, time, offset)=
  save b_v,p;
  pair b_v;
  path p;
  b_v=(xpart(child)*xu,ypart(child)*yu)+offset;
  p=b_v{dir ca} .. tension ten .. {dir (angle (direction time of last_extends_path))}point time of last_extends_path;
  draw p withcolor extends_color;
enddef;

def fork_push_extends_line(expr child, ca, ten, time, offset)=
  save b_v,p;
  pair b_v;
  path p;
  b_v=(xpart(child)*xu,ypart(child)*yu)+offset;
  p=b_v{dir ca} .. tension ten .. {dir (angle (direction time of last_extends_path))}point time of last_extends_path;
  draw p withcolor extends_color;
  path last_extends_path;
  last_extends_path=p;
  pair last_extends_beginning;
  last_extends_beginning=b_v;
  numeric last_extends_bangle;
  last_extends_bangle = ca;
enddef;

def push_extends_line(expr child, ca, ten, offset)=
  save b_v,p;
  pair b_v;
  path p[];
  b_v=(xpart(child)*xu,ypart(child)*yu)+offset;
  p2=b_v{dir ca} .. tension ten .. {dir last_extends_bangle}last_extends_beginning;
  draw p2 withcolor extends_color;
  p1=last_extends_path;
  path last_extends_path;
  last_extends_path=p2;
  pair last_extends_beginning;
  last_extends_beginning=b_v;
  numeric last_extends_bangle;
  last_extends_bangle = ca;
enddef;

def draw_extends(expr child,ca,parent,pa,ten,offset)=
  draw_extends_endpoint(parent,pa,offset);
  draw_extends_line(child,ca,ten,offset);
enddef;

def draw_comp_endpoint(expr type, a, offset)=
  save d;
  save v;
  pair v[];
  pair d;
  path last_comp_path;
  pair last_comp_endpoint;
  numeric last_comp_eangle;
  v2=(xpart(type)*xu,ypart(type)*yu)+offset;
  last_comp_endpoint = v2 - d*unit;
  last_comp_eangle = a;
  d=dir(a);
  last_comp_path=(v2 -- (v2 + (d rotated 135)*unit/diag) -- (v2 - d*unit) -- (v2+(d rotated 225)*unit/diag) -- cycle);
  draw last_comp_path withcolor comp_color;
  fill last_comp_path withcolor comp_color;
enddef;

def draw_comp_beginning(expr comp_v, comp_a, ten, comp_n, l_width, offset)=
  save beg_v;
  pair beg_v;
  beg_v=(xpart(comp_v)*xu,ypart(comp_v)*yu)+offset;
  draw beg_v{dir comp_a} .. tension ten .. {dir last_comp_eangle}last_comp_endpoint withcolor comp_color;
  label(TEX(comp_n),beg_v-(dir comp_a)*l_width*unit/2);
enddef;

def draw_comp_line(expr child, ca, ten, offset)=
  draw ((xpart(child)*xu,ypart(child)*yu)+offset){dir ca} .. tension ten .. {dir last_comp_eangle}last_comp_endpoint withcolor comp_color;
enddef;

def push_comp_line(expr child, ca, ten, offset)=
  save b_v,p;
  pair b_v;
  path p[];
  b_v=(xpart(child)*xu,ypart(child)*yu)+offset;
  p2=b_v{dir ca} .. tension ten .. {dir last_comp_eangle}last_comp_endpoint;
  draw p2 withcolor comp_color;
  p1=last_comp_path;
  path last_comp_path;
  last_comp_path=p2;
  pair last_comp_endpoint;
  last_comp_endpoint=b_v;
  numeric last_comp_eangle;
  last_comp_eangle = ca;
enddef;

def draw_comp(expr comp,ca,type,ta,ten,lab,lab_width,offset)=
  draw_comp_endpoint(type,ta,offset);
  draw_comp_beginning(comp,ca,ten,lab,lab_width,offset);
enddef;

def draw_module(expr lu,ro,lab,offset)=
  save v;
  save lx;
  pair v[];
  lx=8*xu;
  v1=(xpart(lu)*xu,ypart(lu)*yu)+offset;
  v2=(xpart(ro)*xu,ypart(ro)*yu)+offset;
  v3=(xpart(v1),ypart(v2));
  draw v1 -- (xpart(v2),ypart(v1)) -- v2 -- v3 -- cycle;
  draw v3 -- v3+(0,2*yu) -- v3+(lx,2*yu) -- v3+(lx,0);
  label(TEX(lab),v3+(lx/2,yu));
enddef;

def draw_box(expr lu,ro,lab,offset)=
  draw ((xpart(lu)*xu,ypart(lu)*yu)+offset)
    -- ((xpart(ro)*xu,ypart(lu)*yu)+offset)
    -- ((xpart(ro)*xu,ypart(ro)*yu)+offset)
    -- ((xpart(lu)*xu,ypart(ro)*yu)+offset)
    -- cycle;
  label(TEX(lab),
      ((xpart(lu)+xpart(ro))*xu/2,(ypart(lu)+ypart(ro))*yu/2)+offset);
enddef;

def basic_module(expr offset)=
  draw_module((-2,-2),(47,12),"muli\_basic",offset);
  draw_box((0,4),(10,6),"measurable\_type",offset);
  draw_box((6,9),(16,11),"serializable\_class",offset);
  draw_box((7,-1),(15,1),"unique\_type",offset);
  draw_box((12,4),(22,6),"identified\_type",offset);
  draw_box((24,0),(34,3),"",offset);
  draw_label(29,1,"next",offset);
  draw_label(29,2,"ref",offset);
  draw_box((24,3),(34,5),"serializable\_ref\_type",offset);
  draw_box((24,7),(34,9),"",offset);
  draw_label(29,8,"next",offset);
  draw_box((24,9),(34,11),"position\_stack\_type",offset);
  draw_box((36,0),(46,3),"",offset);
  draw_label(41,1,"references",offset);
  draw_label(41,2,"heap",offset);
  draw_box((36,3),(46,5),"marker\_type",offset);
  draw_box((36,7),(46,9),"position\_stack",offset);
  draw_box((36,9),(46,11),"page\_ring\_type",offset);
  
  draw_extends((5,6),90,(11,9),90,1.5,offset);
  path mae_ser_path;
  mae_ser_path=last_extends_path;
  draw_extends_line((17,6),90,1.5,offset);
  path id_ser_path;
  id_ser_path=last_extends_path;
  
  draw_extends((11,1),90,(17,4),90,1.5,offset);
  draw_extends((41,5),90,(41,7),90,1,offset);
  
  draw_comp((36,8),180,(34,10),180,1.5,"1",1,offset);
  draw_comp_beginning((34,8),0,1.5,"1",1,offset);

  draw_comp((34,1),0,(34,4),180,1.5,"1",1,offset);
  draw_comp_beginning((36,1),180,1.5,"1",1,offset);
  draw_comp_beginning((36,2),180,1.5,"1",1,offset);

  draw_comp_endpoint((16,10),180,offset);
  push_comp_line((23,3),90,1,offset);
  draw_comp_beginning((24,2),180,1,"1",1,offset);
enddef;

def momentum_module(expr offset)=
  draw_module((0,-1),(16,8),"muli\_momentum",offset);
  draw_box((1,1),(15,3),"qcd\_2\_2\_class",offset);
  draw_extends((8,3),90,(8,5),90,1,offset);
  draw_box((1,5),(15,7),"transversal\_momentum\_type",offset);
enddef;

def aq_module(expr offset)=
  draw_module((0,0),(14,8),"muli\_aq",offset);
  draw_box((1,2),(13,5),"",offset);
  draw_label(7,3,"int\_list",offset);
  draw_label(7,4,"err\_tree",offset);
  draw_box((1,5),(13,7),"aq\_class",offset);
enddef;

def dsigma_module(expr offset)=
  draw_module((0,0),(14,7),"dsigma",offset);
  draw_box((1,1),(13,4),"",offset);
  draw_label(6,2,"pt",offset);
  draw_label(6,3,"cuba\_int",offset);
  draw_box((1,4),(13,6),"muli\_dsigma\_type",offset);
enddef;

def cuba_module(expr offset)=
  draw_module((0,0),(32,9),"cuba",offset);
  draw_box((1,1),(7,3),"cuhre\_type",offset);
  draw_box((9,1),(15,3),"suave\_type",offset);
  draw_box((16.8,1),(23.2,3),"divonne\_type",offset);
  draw_box((25,1),(31,3),"vegas\_type",offset);
  draw_box((13,6),(19,8),"cuba\_type",offset);
  draw_extends((4,3),90,(16,6),90,5,offset);
  draw_extends_line((12,3),90,2,offset);
  draw_extends_line((20,3),90,2,offset);
  draw_extends_line((29,3),90,5,offset);
enddef;

def fibonacci_module(expr offset)=
  draw_module((0,0),(26,21),"fibonacci\_tree",offset);
  draw_box((1,4),(11,6),"fibonacci\_list\_type",offset);
  draw_box((15,1),(25,3),"fibonacci\_stub\_type",offset);
  draw_box((1,8),(11,10),"fibonacci\_leave\_type",offset);
  draw_box((15,8),(25,10),"fibonacci\_root\_type",offset);
  draw_box((8,18),(18,20),"fibonacci\_node\_type",offset);
  
  draw_box((8,13),(18,18),"",offset);
  draw_label(13,14,"down",offset);
  draw_label(13,15,"up",offset);
  draw_label(13,16,"right",offset);
  draw_label(13,17,"left",offset);
  draw_extends((6,10),90,(13,13),90,1.5,offset);
  draw_extends_line((20,10),90,1.5,offset);
  draw_extends((20,3),90,(20,5),90,1,offset);

  draw_box((15,5),(25,8),"",offset);
  draw_label(20,6,"rightmost",offset);
  draw_label(20,7,"leftmost",offset);

  draw_box((1,1),(11,4),"",offset);
  draw_label(6,2,"leave",offset);
  draw_label(6,3,"next",offset);
  draw_comp((11,3),0,(11,5),180,1.5,"1",1,offset);
  
  draw_comp((11,2),0,(11,9),180,2,"1",1,offset);
  draw_comp_beginning((15,6),180,1,"1",1,offset);
  draw_comp_beginning((15,7),180,1,"1",1,offset);

  draw_comp_endpoint((18,19),180,offset);
  draw_comp_beginning((18,17),0,1.5,"1",1,offset);
  draw_comp_beginning((18,16),0,1.5,"1",1,offset);
  draw_comp_beginning((18,15),0,1.5,"1",1,offset);
enddef;

def muli_module(expr offset)=
  draw_module((0,0),(14,13),"muli",offset);
  draw_box((5,1),(11,6),"",offset);
  draw_label(8,5,"node",offset);
  draw_label(8,4,"beam",offset);
  draw_label(8,3,"samples",offset);
  draw_label(8,2,"dsigma",offset);
  draw_box((5,6),(11,8),"muli\_type",offset);
  draw_box((4,10),(12,12),"qcd\_2\_2\_type",offset);
  draw_extends((8,8),90,(8,10),90,1,offset);
enddef;

def mcint_module(expr offset)=
  draw_module((0,0),(16,27),"muli\_mcint",offset);
  draw_box((3,1),(13,3),"",offset);
  draw_label(8,2,"int\_kinds",offset);
  draw_box((3,3),(13,5),"sample\_inclusive\_type",offset);
  
  draw_box((3,7),(13,9),"sample\_int\_kind\_type",offset);

  draw_box((3,11),(13,13),"",offset);
  draw_label(8,12,"slices",offset);
  draw_box((3,13),(13,15),"sample\_3d\_type",offset);

  draw_box((3,17),(13,19),"",offset);
  draw_label(8,18,"regions",offset);
  draw_box((3,19),(13,21),"sample\_2d\_type",offset);

  draw_box((3,23),(13,25),"sample\_region\_type",offset);

  draw_comp((13,2),0,(13,8),180,2,"1..n",2,offset);
  draw_comp((13,12),0,(13,20),180,2,"1..n",2,offset);
  draw_comp((3,18),180,(3,24),0,2,"1..n",2,offset);
  
  draw_extends((8,9),90,(8,11),90,1,offset);
enddef;

def trapezium_module(expr offset)=
  draw_module((0,0),(30,18),"muli\_trapezium",offset);
  draw_box((1,3),(13,5),"muli\_trapezium\_list\_type",offset);
  draw_box((15,1),(27,3),"",offset);
  draw_label(21,2,"down",offset);
  draw_box((15,3),(27,5),"muli\_trapezium\_tree\_type",offset);
  draw_box((8,8),(20,11),"",offset);
  draw_label(14,9,"right",offset);
  draw_label(14,10,"left",offset);

  draw_box((8,11),(20,13),"muli\_trapezium\_node\_class",offset);
  draw_box((8,15),(20,17),"muli\_trapezium\_type",offset);

  draw_extends_endpoint((14,15),90,offset);
  draw_extends_line((14,13),90,1,offset);
  draw_extends_endpoint((14,8),90,offset);
  draw_extends_line ((7,5),90,1.5,offset);
  draw_extends_line ((21,5),90,1.5,offset);

  draw_comp_endpoint((20,12),180,offset);
  draw_comp_beginning ((27,2),0,1.5,"1",1,offset);
  draw_comp_beginning ((20,9),0,1.5,"1",1,offset);
  draw_comp_beginning ((20,10),0,1.5,"1",1,offset);
enddef;

def remnant_module(expr offset)=
  draw_module((0,0),(22,28),"muli\_remnant",offset);
  draw_box((4,1),(16,4),"",offset);
  draw_label(10,2,"pdf\_norm",offset);
  draw_label(10,3,"proton",offset);
  draw_box((4,4),(16,6),"muli\_pp\_remnant\_type",offset);
  draw_box((4,8),(16,13),"",offset);
  draw_label(10,9,"pdf\_norm",offset);
  draw_label(10,10,"twin\_partons",offset);
  draw_label(10,11,"fs\_partons",offset);
  draw_label(10,12,"is\_partons",offset);
  draw_box((4,13),(16,15),"muli\_proton\_remnant\_type",offset);
  draw_box((4,17),(16,20),"",offset);
  draw_label(10,18,"twin",offset);
  draw_label(10,19,"next",offset);
  draw_box((4,20),(16,22),"muli\_parton\_type",offset);
  draw_box((4,24),(16,26),"pdfnorm\_type",offset);

  draw_comp((4,3),180,(4,14),0,2,"2",1,offset);
  draw_comp((16,2),0,(16,25),180,2.5,"1",1,offset);
  draw_comp_beginning((16,9),0,2,"1",1,offset);

  draw_comp((16,19),0,(16,21),180,1,"0,1",2,offset);
  draw_comp_beginning((16,18),0,1,"0,1",2,offset);
  draw_comp_beginning((16,10),0,2,"1",1,offset);
  draw_comp_beginning((16,11),0,2,"1",1,offset);
  draw_comp_beginning((16,12),0,2,"1",1,offset);
enddef;

def interactions_module(expr offset)=
  draw_box((0,0),(12,2),"",offset);
  draw_box((0,2),(10,4),"interactions\_module",offset);
enddef;

beginfig(1)
  pair basic_offset,%
    cuba_offset,%
    remnant_offset,%
    mcint_offset,%
    momentum_offset,%
    aq_offset,%
    dsigma_offset,%
    trapezium_offset,%
    fibonacci_offset,%
    interactions_offset,%
    muli_offset;
  basic_offset=(20*xu,48*yu);
  cuba_offset=(0,30*yu);
  remnant_offset=(72*xu,31*yu);
  mcint_offset=(78*xu,0);
  momentum_offset=(53*xu,27*yu);
  aq_offset=(36*xu,34*yu);
  dsigma_offset=(36*xu,23*yu);
  trapezium_offset=(29*xu,0*yu);
  fibonacci_offset=(0,0);
  interactions_offset=(0,58*yu);
  muli_offset=(61*xu,0);

  pickup pencircle scaled 1.5;
  
  basic_module(basic_offset);

  interactions_module(interactions_offset);
  cuba_module(cuba_offset);
  fibonacci_module(fibonacci_offset);
  
  aq_module(aq_offset);
  dsigma_module(dsigma_offset);
  trapezium_module(trapezium_offset);

  momentum_module(momentum_offset);
  muli_module(muli_offset);

  remnant_module(remnant_offset);
  mcint_module(mcint_offset);

  path last_extends_path;
  last_extends_path=id_ser_path;
  fork_push_extends_line((20,10),-150,1,0.67,basic_offset);
  push_extends_line((-2,29),90,1,remnant_offset);
  push_extends_line((-2,0),90,1,remnant_offset);
  fork_extends_line((10,26),90,2,1,remnant_offset);
  fork_extends_line((10,22),90,2,25/29,remnant_offset);
  fork_extends_line((10,15),90,2,18/29,remnant_offset);
  fork_extends_line((10,7),90,1,13.5/29,momentum_offset);
  fork_extends_line((10,6),90,2,9/29,remnant_offset);
  push_extends_line((-2,17),110,1,mcint_offset);
  fork_extends_line((8,21),90,4,0.45,mcint_offset);
  fork_extends_line((8,15),90,3,0,mcint_offset);
  push_extends_line((3,6),180,1,mcint_offset);
  fork_extends_line((8,5),90,2,0,mcint_offset);

  path last_extends_path;
  last_extends_path=mae_ser_path;
  fork_push_extends_line((-4,4),90,1,0.5,basic_offset);
  push_extends_line((16,8),90,1,cuba_offset);

  draw_extends_endpoint((8,1),90,momentum_offset);
  draw_extends_line((10,12),90,1,muli_offset);

  draw_extends_endpoint((17,4),90,basic_offset);
  draw_extends_line((7,7),90,1,aq_offset);

  draw_extends_endpoint((7,2),90,aq_offset);
  draw_extends_line((10,6),90,1,dsigma_offset);

  draw_extends_endpoint((5,4),90,basic_offset);
  draw_extends_line((13,-11),90,1,basic_offset);
  push_extends_line((13,-23),90,1,basic_offset);
  fork_extends_line((13,20),90,1,0.5,fibonacci_offset);
  fork_extends_line((14,17),90,1,0,trapezium_offset);
  
  draw_comp_endpoint((6,10),0,basic_offset);
  draw_comp_beginning((8,14),180,1.2,"1",1,fibonacci_offset);

  draw_comp_endpoint((1,6),0,momentum_offset);
  draw_comp_beginning((13,2),0,1,"1",1,dsigma_offset);

  draw_comp_endpoint((23.7,2.5),90,cuba_offset);
  draw_comp_beginning((1,3),180,1.2,"1",1,dsigma_offset);

  draw_comp_endpoint((27,4),180,trapezium_offset);
  draw_comp_beginning((5,2),180,1,"1",1,muli_offset);

  draw_comp_endpoint((3,4),0,mcint_offset);
  draw_comp_beginning((11,3),0,1,"1",1,muli_offset);

  draw_comp_endpoint((20,12),180,trapezium_offset);
  draw_comp_beginning((5,4),180,1,"1",1,muli_offset);
  
  draw_comp_endpoint((4,5),0,remnant_offset);
  draw_comp_beginning((11,5),0,1.5,"1",1,muli_offset);

  draw_comp_endpoint((25,9),180,fibonacci_offset);
  push_comp_line((27,10),-90,1,fibonacci_offset);
  push_comp_line((27,20),-90,1,fibonacci_offset);
  push_comp_line((-2,-5),-90,1,aq_offset);
  push_comp_line((-2,1),-90,1,aq_offset);
  draw_comp_beginning((1,4),180,1,"1",1,aq_offset);

  draw_comp_endpoint((1,4),0,trapezium_offset);
  push_comp_line((-1,5),-90,1,trapezium_offset);
  push_comp_line((-1,19),-90,1,trapezium_offset);
  push_comp_line((-1,-6),-90,1,aq_offset);
  push_comp_line((-1,1),-90,1,aq_offset);
  draw_comp_beginning((1,3),180,1,"1",1,aq_offset);
endfig;
beginfig(2)
  basic_module((0,0));
endfig;
beginfig(3)
  aq_module((0,0));
endfig;
beginfig(4)
  dsigma_module((0,0));
endfig;
beginfig(5)
  trapezium_module((0,0));
endfig;
beginfig(6)
  momentum_module((0,0));
endfig;
beginfig(7)
  muli_module((0,0));
endfig;
beginfig(8)
  remnant_module((0,0));
endfig;
beginfig(9)
  mcint_module((0,0));
endfig;
beginfig(10)
  cuba_module((0,0));
endfig;
beginfig(11)
  fibonacci_module((0,0));
endfig;
beginfig(12)
  interactions_module((0,0));
endfig;
end;
