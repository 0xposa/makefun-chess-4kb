import os,json as J,urllib.request as U
V=[100,320,330,500,900,0]
W=['0000::::2246112500041/.0122,0000','&(**(,00*023*134*034*123(,01&(**',',....000.012.112.022.222.100,...','*((&*((&*((&*((&,**(.,,,44004620']
Q={}
for k,p in enumerate('PNBRQK'):
 for j,c in enumerate(W[k if k<3 else 2 if k<5 else 3]):
  v=5*ord(c)-240
  for i in(j//4*8+j%4,j//4*8+7-j%4):Q[p,i]=V[k]+v;Q[p.lower(),i^56]=-V[k]-v
KN=[(1,2),(2,1),(2,-1),(1,-2),(-1,-2),(-2,-1),(-2,1),(-1,2)]
D8=[(i//3-1,i%3-1)for i in range(9)if i!=4]
CM={60:12,63:14,56:13,4:3,7:11,0:7}
nd=X=0
def P(f):
 global E
 p=f.split();s=p[0].replace('/','')
 for d in'12345678':s=s.replace(d,'.'*int(d))
 b=list(s);ca=sum(1<<'KQkq'.index(c)for c in p[2]if c in'KQkq')
 E=sum(Q[x,i]for i,x in enumerate(b)if x!='.')
 return[b,p[1]=='w',ca,p[3]=='-'and-1 or(8-int(p[3][1]))<<3+ord(p[3][0])-97]
def A(b,s,w):
 f=s%8
 for k in(7,9):
  j=s+k if w else s-k
  if 0<=j<64 and abs(j%8-f)==1 and b[j]=='Pp'[not w]:return 1
 for df,dr in KN:
  ff,rr=f+df,s//8+dr
  if 0<=ff<8>rr>=0 and b[rr*8+ff]=='Nn'[not w]:return 1
 for df,dr in D8:
  ff,rr=f+df,s//8+dr;n=0
  while 0<=ff<8>rr>=0:
   pc=b[rr*8+ff]
   if pc!='.':
    if pc.isupper()==w and pc.upper()in(('K'if n==0 else'')+('BQ'if df and dr else'RQ')):return 1
    break
   ff+=df;rr+=dr;n+=1
def C(S):
 return A(S[0],S[0].index('Kk'[S[1]]),S[1])
def G(S):
 b,tm,ca,ep=S;M=[]
 for i,pc in enumerate(b):
  if pc=='.'or pc.isupper()!=tm:continue
  f,r=i%8,i//8;u=pc.upper()
  if u=='P':
   d=8-16*tm;la=r==6-5*tm
   if b[i+d]=='.':
    M.append((i,i+d,la))
    if r==1+5*tm and b[i+2*d]=='.':M.append((i,i+2*d,''))
   for df in(-1,1):
    if 0<=f+df<8:
     j=i+d+df;tg=b[j]
     if tg!='.'and tg.isupper()!=tm:M.append((i,j,la))
     elif j==ep:M.append((i,j,''))
  else:
   for df,dr in(KN if u=='N'else D8):
    if(u=='B')!=bool(df and dr)and u in'BR':continue
    ff,rr=f+df,r+dr
    while 0<=ff<8>rr>=0:
     tg=b[rr*8+ff]
     if tg=='.'or tg.isupper()!=tm:M.append((i,rr*8+ff,''))
     if tg!='.'or u in'NK':break
     ff+=df;rr+=dr
   if u=='K'and i==(60 if tm else 4):
    for dd,em,rk in((1,(1,2),3),(-1,(-1,-2,-3),-4)):
     if ca>>(2*(not tm)+(dd<0))&1 and b[i+rk]=='Rr'[not tm]and all(b[i+x]=='.'for x in em)and not(A(b,i,not tm)or A(b,i+dd,not tm)):M.append((i,i+2*dd,''))
 return[(i,j,q)for i,j,p in M for q in('QRBN'if p else[''])]
def K(S,m):
 global E
 b,tm,ca,ep=S;fr,to,pr=m;pc=b[fr];u=pc.upper();cq=to;cap=b[to]
 if u=='P'and to==ep and cap=='.':cq=to-8+16*tm;cap=b[cq]
 ud=(b[:],ca,ep,E)
 if cap!='.':E-=Q[cap,cq]
 if cq!=to:b[cq]='.'
 np=pr and(pr if tm else pr.lower())or pc;b[to]=np;b[fr]='.';E+=Q[np,to]-Q[pc,fr]
 if u=='K'and abs(to-fr)==2:
  rf,rt=(fr+3,fr+1)if to>fr else(fr-4,fr-1);rp=b[rf];b[rt]=rp;b[rf]='.';E+=Q[rp,rt]-Q[rp,rf]
 S[1]=not tm;S[2]&=CM.get(fr,15)&CM.get(to,15);S[3]=fr+8-16*tm if u=='P'and abs(to-fr)==16 else-1
 return ud
def N(S,ud):
 global E
 S[0][:],S[2],S[3],E=ud;S[1]=not S[1]
def O(S,M):
 b=S[0]
 return sorted(M,key=lambda m:-abs(Q.get((b[m[1]],m[1]),0)))
def R(S,d,a,bb):
 global nd,X
 nd+=1
 if nd>100000:X=1;return 0
 b,tm,_,ep=S
 if d>0 or A(b,b.index('Kk'[not tm]),not tm):ms=O(S,G(S));n=0
 else:
  st=E*(2*tm-1)
  if st>=bb:return bb
  if st>a:a=st
  ms=O(S,[m for m in G(S)if b[m[1]]!='.'or m[1]==ep or m[2]]);n=1
 for m in ms:
  ud=K(S,m)
  if C(S):N(S,ud);continue
  s=-R(S,d-1,-bb,-a);N(S,ud)
  if X:return 0
  n+=1
  if s>a:a=s
  if a>=bb:break
 if n==0:return-1e5 if A(S[0],S[0].index('Kk'[not S[1]]),not S[1])else 0
 return a
def B(S):
 global nd,X
 M=[]
 for m in G(S):
  ud=K(S,m)
  if not C(S):M.append(m)
  N(S,ud)
 nd=X=0;bm=M[0]
 for d in range(1,9):
  a=-1e7;cm=bm
  for m in O(S,M):
   ud=K(S,m);s=-R(S,d-1,-1e7,-a);N(S,ud)
   if X:break
   if s>a:a=s;cm=m
  else:bm=cm;continue
  break
 return bm
H=os.getenv('MAKEFUN_ENV_URL')
if H:
 v=lambda m:''.join(chr(97+x%8)+str(8-x//8)for x in m[:2])+m[2].lower()
 rq=lambda p,dd:J.loads(U.urlopen(H+p,J.dumps(dd).encode()).read())
 while 1:
  g=rq('/game',{});t=g['status'][0]
  if t=='d':break
  if t=='y':g=rq('/move',{'game':g['game'],'move':v(B(P(g['fen'])))})
