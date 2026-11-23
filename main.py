import os,json,urllib.request as U
V=[100,320,330,500,1000,0]
W="0000::::2246112500041/.0122,0000|*((&*((&*((&*((&,**(.,,,44004620".split('|')
W[1:1]=['0'*32]*4
T=[[(ord(s[(i//8)*4+min(i%8,7-i%8)])-48)*5 for i in range(64)]for s in W]
Q={}
for k,p in enumerate('PNBRQK'):
 for i in range(64):Q[p,i]=V[k]+T[k][i];Q[p.lower(),i]=-V[k]-T[k][i^56]
KN=[(1,2),(2,1),(2,-1),(1,-2),(-1,-2),(-2,-1),(-2,1),(-1,2)]
D8=[(a,b)for a in(-1,0,1)for b in(-1,0,1)if a or b]
ND=8000;nd=STP=0
def P(f):
 p=f.split();s=p[0].replace('/','')
 for d in'12345678':s=s.replace(d,'.'*int(d))
 return[list(s),p[1]=='w',p[2],(8-int(p[3][1]))*8+ord(p[3][0])-97 if p[3]!='-'else-1]
def atk(b,s,w):
 f=s%8
 for j in((s+7,s+9)if w else(s-9,s-7)):
  if 0<=j<64 and abs(j%8-f)==1 and b[j]==('P'if w else'p'):return 1
 for ds,pc in((KN,'N'),(D8,'K')):
  for df,dr in ds:
   ff,rr=f+df,s//8+dr
   if 0<=ff<8 and 0<=rr<8 and b[rr*8+ff]==(pc if w else pc.lower()):return 1
 for df,dr in D8:
  ff,rr=f+df,s//8+dr
  while 0<=ff<8 and 0<=rr<8:
   pc=b[rr*8+ff]
   if pc!='.':
    if pc.isupper()==w and pc.upper()in('BQ'if df and dr else'RQ'):return 1
    break
   ff+=df;rr+=dr
def gen(S):
 b,tm,ca,ep=S;M=[]
 for i,pc in enumerate(b):
  if pc=='.'or pc.isupper()!=tm:continue
  f,r=i%8,i//8;u=pc.upper()
  if u=='P':
   d=-8 if tm else 8;la=r==(1 if tm else 6)
   if b[i+d]=='.':
    M.append((i,i+d,la))
    if r==(6 if tm else 1)and b[i+2*d]=='.':M.append((i,i+2*d,''))
   for df in(-1,1):
    if 0<=f+df<8:
     j=i+d+df;tg=b[j]
     if tg!='.'and tg.isupper()!=tm:M.append((i,j,la))
     elif j==ep:M.append((i,j,''))
  else:
   for df,dr in(KN if u=='N'else D8):
    if(u=='B')!=bool(df and dr)and u in'BR':continue
    ff,rr=f+df,r+dr
    while 0<=ff<8 and 0<=rr<8:
     tg=b[rr*8+ff]
     if tg=='.'or tg.isupper()!=tm:M.append((i,rr*8+ff,''))
     if tg!='.'or u in'NK':break
     ff+=df;rr+=dr
   if u=='K'and i==(60 if tm else 4):
    R='R'if tm else'r'
    for dd,rt,em,rk in((1,'K'if tm else'k',(1,2),3),(-1,'Q'if tm else'q',(-1,-2,-3),-4)):
     if rt in ca and b[i+rk]==R and all(b[i+x]=='.'for x in em)and not atk(b,i,not tm)and not atk(b,i+dd,not tm):M.append((i,i+2*dd,''))
 return[(i,j,q)for i,j,p in M for q in('QRBN'if p else[''])]
def mk(S,m):
 b,tm,ca,ep=S;fr,to,pr=m;pc=b[fr];u=pc.upper();un=(b[:],ca,ep)
 if u=='P'and to==ep and b[to]=='.':b[to+8 if tm else to-8]='.'
 b[to]=pc if not pr else(pr if tm else pr.lower());b[fr]='.'
 if u=='K'and abs(to-fr)==2:
  rf,rt=(fr+3,fr+1)if to>fr else(fr-4,fr-1);b[rt]=b[rf];b[rf]='.'
 rm=''.join(ch for sq,ch in((60,'KQ'),(63,'K'),(56,'Q'),(4,'kq'),(7,'k'),(0,'q'))if fr==sq or to==sq)
 S[1]=not tm;S[2]=''.join(c for c in ca if c not in rm);S[3]=(fr+to)//2 if u=='P'and abs(to-fr)==16 else-1
 return un
def un(S,ud):
 S[0][:],S[2],S[3]=ud;S[1]=not S[1]
def O(S,M):
 b=S[0]
 def sc(m):
  return 10*abs(Q[b[m[1]],m[1]])if b[m[1]]!='.'else 0
 return sorted(M,key=sc,reverse=True)
def sr(S,d,a,bb):
 global nd,STP
 if d<=0:
  s=sum(Q[pc,i]for i,pc in enumerate(S[0])if pc!='.')
  return s if S[1]else-s
 nd+=1
 if STP or nd>ND:STP=1;return 0
 bst=-10**7
 for m in O(S,gen(S)):
  ud=mk(S,m)
  if atk(S[0],S[0].index('K'if not S[1]else'k'),S[1]):un(S,ud);continue
  s=-sr(S,d-1,-bb,-a);un(S,ud)
  if STP:return 0
  if s>bst:bst=s
  if s>a:a=s
  if a>=bb:break
 if bst==-10**7:return-100000 if atk(S[0],S[0].index('K'if S[1]else'k'),not S[1])else 0
 return bst
def best(S):
 global nd,STP
 M=[]
 for m in gen(S):
  ud=mk(S,m)
  if not atk(S[0],S[0].index('K'if not S[1]else'k'),S[1]):M.append(m)
  un(S,ud)
 if not M:return None
 nd=STP=0;bm=M[0]
 for d in range(1,7):
  a=-10**7;cm=bm
  for m in O(S,M):
   ud=mk(S,m);s=-sr(S,d-1,-10**7,-a);un(S,ud)
   if STP:break
   if s>a:a=s;cm=m
  bm=cm
 return bm
E=os.environ.get('MAKEFUN_ENV_URL')
if E:
 rq=lambda p,dd:json.loads(U.urlopen(E+p,json.dumps(dd).encode()).read())
 g=rq('/game',{})
 while g['status']!='done':
  if g['status']=='your_move':
   m=best(P(g['fen']));g=rq('/move',{'game':g['game'],'move':chr(97+m[0]%8)+str(8-m[0]//8)+chr(97+m[1]%8)+str(8-m[1]//8)+(m[2].lower()if m[2]else'')})
  else:g=rq('/game',{})
