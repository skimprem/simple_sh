module sh_expand_ls
contains
subroutine sf_ab(f,pu,nst,ab)
  !real*8 f(*),pu(*),ab(*)
  real(kind=8), dimension(:), intent(in) ::  f(:), pu(:)
  real(kind=8), dimension(:), intent(out) :: ab(:)
  integer(kind=4), intent(in) :: nst
  integer(kind=4) :: i,j,kb,kl,lst,kbl,n_2,kl_2,kt,la,ma,mb
  real(kind=8) :: a(:),cs(:),ss(:),fb(:),cb(:),sb(:),pnn(:),pln(:),v,bt,c1,c2,fi
  !logical*4 chet
  !integer*4 nom_pp,indo
  allocatable a,cs,ss,fb,cb,sb,pnn,pln
! f() - функция для разложения (сетка)
! запись в ленточной форме от сев.-зап. угла
! по широтным поясам
! pu(1) - северная граница
! pu(2) - Южная граница
! pu(3) - шаг по широте
! pu(4) - западная граница
! pu(5) - восточная граница
! pu(6) - шаг по долготе
! nst - степень разложения
! ab() - упакованная матрица коэффициентов
! kb - число делений по широте
! kl - число делений по долготе
  n_2 = nst/2
  v = dsqrt(3.d0)
  call puch(pu,kb,kl,0)
  kbl = kb*kl
  lst = (nst + 1)*(nst + 2) - nst - 1
  la  = (nst + 1)*(nst + 2)/2
  mst = lst + nst + 1
  if((2*nst+1).gt.kl .or. (nst+1).gt.kb) then
   do i = 1,mst
    ab(i) = 9999.
   enddo
   return
  endif
  kl_2 = kl/2
  do i = 1,mst
   ab(i) = 0.d0
  enddo
  allocate(a(kbl),cs(kl),fb(kl))
  fi = pu(6)/2.d0
  do i = 1,kb
   do j = 1,kl
    fb(j) = f((i-1)*kl+j)
   enddo
   call dft_for(fb,kl,cs)
   call sd_kof(cs,kl,fi,fb)
   do j = 1,kl
    a((i-1)*kl+j) = fb(j)
   enddo
  enddo
  deallocate(cs,fb)
  allocate(cb(kb),sb(kb),pnn(kb),pln(kb*nst+kb),cs(kb),ss(kb))
  do i = 1,kb
   bt = pu(1)-(i-1)*pu(3)
   cb(i) = dcos(bt)
   sb(i) = dsin(bt)
  enddo
  c2 = 0.d0
  do i = 0,nst
   kt = nst + 1 - i
   if(i.gt.1) v = dsqrt((2*i+1.d0)/(2.d0*i))
   do j = 1,kb
    call ft_nkoef1( i,kl,ma,mb )
    cs(j) = a((j-1)*kl+ma)
    if(mb.gt.0) ss(j) = a((j-1)*kl+mb)
    if(i.eq.0) then
     pnn(j) = 1.d0
    else
     pnn(j) = pnn(j)*v*cb(j)
    endif
    pln(indo(j,1,kb)) = pnn(j)
   enddo
   do j = i+1,nst
    c1 = dsqrt((4.d0*j*j - 1.d0)/(j*j - i*i + 0.d0))
    if(j.gt.1) then
     c2 = (2*j+1.d0)/(2.d0*j-3.d0)*((j-1.)*(j-1.)-i*i)/(j*j-i*i+0.d0)
     c2 = dsqrt(c2)
    endif
    do ij = 1,kb
     l = indo(ij,(j-i+1),kb)
     if(j-i.gt.1) then
      pln(l) = c1*pln(indo(ij,(j-i),kb))*sb(ij) - c2*pln(indo(ij,(j-i-1),kb))
     else
      pln(l) = c1*pln(indo(ij,(j-i),kb))*sb(ij)
     endif
    enddo
   enddo
   if(i.eq.0 .or. (i.eq.kl_2 .and. chet(kl))) then
    call o_sdv1(pln,cs,kb,kt,iErrO)
    do j = i,kt - 1
     ab(nom_pp(j,i,nst)) = cs(j-i+1)
    enddo
   else
    call o_sdv2(pln,cs,ss,kb,kt,iErrO)
    do j = i,nst
     l = nom_pp(j,i,nst)
     ab(l) = cs(j-i+1)
     ab(l+la) = ss(j-i+1)
    enddo
   endif
  enddo
  deallocate(a,cs,ss,cb,sb,pnn,pln)
  return
end subroutine sf_ab
subroutine psvd(u,v,w,n,k,a,ierr)
  real*8    u(*),w(*),v(*),a(*)
  integer*4 n,k,ierr
  real*8    wmax,eps
  integer*4 i,j,ipt
  integer*1 i0,i1
  !integer indo
  i0 = 0
  i1 = 1
  ierr = 0
  eps  = k*1.e-12
  iPt   = 0
  wmax = 0.d0
  do i = 1,k
   if( w(i).gt.wmax ) wmax = w(i)
  enddo
  wmax = wmax * eps
  do i = 1,k
   if( w(i).gt.wmax .and. wmax.gt.0 ) then
    w(i) = 1.d0/w(i)
   else
    if(iPt.eq.0) iPt  = i
    w(i) = 0.d0
   endif
  enddo
  do i = 1,k
   do j = 1,k
    v(indo(i,j,k)) = v(indo(i,j,k))*w(j)
   enddo
  enddo
  call um2( v,i0,i0,u,i1,i0,a,i0,i0,k,k,n,ierr )
  if( iPt.gt.0 ) ierr = -iPt
  return
end subroutine psvd
integer function indo( i,j,N )
  integer i,j,N
  indo = ( j - 1 ) * N + i
  return
end function indo
function indx(i,j)
  if(i-j) 1,2,2  
1  indx=j*(j-1)/2+i  
   return            
2 indx=i*(i-1)/2+j  
  return
end function indx
subroutine um2(A,itA,isA,B,itB,isB,C,izC,isC,N,K,M,iErrO)
	real*8    A(1),B(1),C(1)
	integer*1 itA,itB,isA,isB,isC,izC
	integer*4 N,K,M
    real*8      vM(:)
 	allocatable vM
    iErrO=0
    if(isA.eq.1.and.N.ne.K) then
	isA=0
	iErrO=302
	endif
    if(isB.eq.1.and.K.ne.M) then
	isB=0
	iErrO=302
	endif
    if(isC.eq.1.and.N.ne.M) then
	isC=0
	iErrO=302
	endif
    if(izC.eq.1.and.K.lt.M) then
	iErrO=205
	return
	endif
    if(izC.eq.2.and.K*M.lt.N*M) then
	iErrO=205 
	return
	endif
    if(izC.ne.0) then
     if(isC.eq.0) then
      allocate(vM(N*M),stat=ier)
     elseif(isC.eq.1) then
      allocate(vM(N*(N+1)/2),stat=ier)
     else
      goto 205
     endif
     if(ier.ne.0) then
      iErrO=202
      return
     endif
    endif
    do 1 ir=1,N
     if(isC.eq.0) then
      jPrd=M
     elseif(isC.eq.1) then
      jPrd=ir
     else
      goto 205
     endif
	do 2 jr=1,jPrd
     if(isC.eq.0) then
      lC=(jr-1)*N+ir
     else
      lC=indx(ir,jr)
     endif
     if(izC.eq.0) then
      C(lC)=0.d0
     else
      vM(lC)=0.d0
     endif
     do 3 ii=1,K
      if(isA.eq.0) then
       if(itA.eq.0) then
        lA=(ii-1)*N+ir
       elseif(itA.eq.1) then
        lA=(ir-1)*K+ii
       else
        goto 205
       endif
      elseif(isA.eq.1) then
       lA=indx(ir,ii)
      else
       goto 205
      endif
      if(isB.eq.0) then
       if(itB.eq.0) then
        lB=(jr-1)*K+ii
       elseif(itB.eq.1) then
        lB=(ii-1)*M+jr
       else
        goto 205
       endif
      elseif(isB.eq.1) then
       lB=indx(jr,ii)
      else
       goto 205
      endif
      if(izC.ne.0) then
       vM(lC)=vM(lC)+A(lA)*B(lB)
      else
       C(lC)=C(lC)+A(lA)*B(lB)
      endif
3     continue
2     continue
1     continue
      if(izC.ne.0) then
       jPrd=N*M
       if(isC.eq.1) jPrd=N*(N+1)/2
       do 4 i=1,jPrd
       if(izC.eq.1) then
       A(i)=vM(i)
       elseif(izC.eq.2) then
       B(i)=vM(i)
       else
       goto 205
       endif
4	 continue
     deallocate(vM)
     endif
     return
205   iErrO=205
   if(izC.ne.0) deallocate(vM)
   return
end subroutine um2
subroutine ft_nkoef1( n,k,ma,mb )
   integer*4 n,k,ma,mb
   mn = k/2
   if( n.gt.mn ) then
    ma = -1
    mb = -1
    return
   endif
   ma = n + 1
   mb = -1
   if( n.gt.0 .and. n+mn+1.le.k ) mb = n + mn + 1
   return
end subroutine ft_nkoef1

subroutine cos_sin_nx( cnx,snx,cx,sx )
   real*8    cnx,snx,cx,sx,t
   t   = cnx
   cnx = cnx*cx - snx*sx
   snx = snx*cx + t  *sx
   return
end subroutine cos_sin_nx

subroutine sd_kof(a,k,fi,b)
   real*8 a(*),fi,b(*),cf,sf,ckf,skf
   integer*4 k,i,n,ma,mb
   n = k/2
   cf = dcos(fi)
   sf = dsin(fi)
   ckf = cf
   skf = sf
   do i = 1,k
    b(i) = 0.d0
   enddo
   b(1) = a(1)
   do i = 1,n
    call ft_nkoef1( i,k,ma,mb )
    if(ma.lt.0 .and. mb.lt.0) then
     b(ma) = 0.d0
     b(mb) = 0.d0
    elseif(ma.gt.0 .and. mb.gt.0) then
     b(ma) = a(ma)*ckf - a(mb)*skf
     b(mb) = a(ma)*skf + a(mb)*ckf
    elseif(ma.gt.0 .and. mb.lt.0) then
     b(ma) = a(ma)
    endif
    call cos_sin_nx( ckf,skf,cf,sf )
   enddo
   return
end subroutine sd_kof

subroutine o_sdv2(A,B,C,N,K,iErrO)
   real*8    A(*),B(*),C(*)
   integer*4 N,K
   real*8    U(:),W(:),V(:)
   integer*1 i0,i1
   allocatable U,W,V
   iErrO = 0
   i0 = 0
   i1 = 2
   allocate( U(N*K),V(K*K),W(K) )
   call svd_i( A,N,K,U,1,V,1,W,iErrO )
   if(iErrO.ne.0) then
    deallocate( U,V,W )
    return
   endif
   call psvd(u,v,w,n,k,a,ierrO)
   deallocate( U,V,W )
   if( iErrO.gt.0 ) return
   call um2( A,i0,i0,B,i0,i0,B,i1,i0,K,N,1,iErrO)
   call um2( A,i0,i0,C,i0,i0,C,i1,i0,K,N,1,iErrO)
   return
end subroutine o_sdv2

subroutine dft_for(f,n,a)
   real*8    f(*),a(*)
   integer*4 n,np
   real*8 pi/3.1415926535897932384626433832795d0/
   real*8 v,ct,st,t,tc,ts,tt,vm
   np = n/2
   v  = 2*pi/(n+0.d0)
   do i = 0,np
    t = i*v
    ct = dcos(t)
    st = dsin(t)
    a(i+1) = 0.d0
    if( i.gt.0 .and. np+i+1.le.n) a(np+i+1) = 0.d0
    do j = 1,n
     if( j.eq.1 ) then
      tc = 1.d0
      ts = 0.d0
     else
      tt = tc
      tc = tc*ct - ts*st
      ts = ts*ct + tt*st
     endif
     a(i+1) = a(i+1) + f(j)*tc
     if( i.gt.0 .and. np+i+1.le.n ) a(np+i+1) = a(np+i+1) + f(j)*ts
    enddo
    if( i.eq.0 ) then
     a(1) = a(1)/(n+0.d0)
    else
     vm = 1.d0
     if( np+i+1.le.n ) vm = 2.d0
     a(i+1) = vm*a(i+1)/(n+0.d0)
     if( vm.gt.1 ) a(np+i+1) = 2*a(np+i+1)/(n+0.d0)
    endif
   enddo
   return
end subroutine dft_for

subroutine svd_i( A,M,N,U,iU,V,iV,W,iErrO )
   real*8  A(*),U(*),V(*),W(*)
   integer M,N,iU,iV,iErrO
   real*8  vR(:),S,C,F,G,H,X,Y,Z,Anr,Sc
   real*8  eps
   integer i,j,l,k,i1,k1,l1,ii,kk,its,MN,il,jl
   !integer indo
   allocatable vR
   allocate( vR(N) )
   iErrO = 0
   eps   = 1.d-12
   G     = 0.d0
   Sc    = 0.d0
   Anr   = 0.d0
   do i = 1,M
    do j = 1,N
     U(indo(i,j,M)) = A(indo(i,j,M))
    enddo
   enddo
   do i = 1,N
    l     = i + 1
    vR(i) = Sc * G
    G     = 0.d0
    S     = 0.d0
    Sc    = 0.d0
    if( i.le.M ) then
     do k = i,M
      Sc = Sc + dabs( U(indo(k,i,M)) )
     enddo
     if( Sc.ne.0.d0 ) then
      do k = i,M
       il = indo(k,i,M)
       U(il) = U(il) / Sc
       S      = S + U(il)*U(il)
      enddo
      F = U(indo(i,i,M))
      G = -dsign( dsqrt(S),F )
      H = F*G - S
      U(indo(i,i,M)) = F - G
      if( i.ne.N ) then
       do j = l,N
        S = 0.d0
        do k = i,M
         S = S + U(indo(k,i,M))*U(indo(k,j,M))
        enddo
        F = S / H
        do k = i,M
         il = indo(k,j,M)
         U(il) = U(il) + F*U(indo(k,i,M))
        enddo
       enddo
      endif
      do k = i,M
       il = indo(k,i,M)
       U(il) = Sc * U(il)
      enddo
     endif
    endif
    W(i) = Sc * G
    G    = 0.d0
    S    = 0.d0
    Sc   = 0.d0
    if( i.le.M .and. i.ne.N ) then
     do k = l,N
      Sc = Sc + dabs( U(indo(i,k,M)) )
     enddo
     if( Sc.ne.0.d0 ) then	
      do k = l,N
       il = indo(i,k,M)
       U(il) = U(il) / Sc
       S      = S + U(il)*U(il)
      enddo
      F = U(indo(i,l,M))
      G = -dsign( dsqrt(S),F )
      H = F*G - S
      U(indo(i,l,M)) = F - G
      do k = l,N
       vR(k) = U(indo(i,k,M)) / H
      enddo
      if( i.ne.M ) then
       do j = l,M
        S = 0.d0
        do k = l,N
         S = S + U(indo(j,k,M))*U(indo(i,k,M))
        enddo
        do k = l,N
         il = indo(j,k,M)
         U(il) = U(il) + S*vR(k)
        enddo
       enddo
      endif
      do k = l,N
       il = indo(i,k,M)
       U(il) = Sc * U(il)
      enddo
     endif
    endif
    Anr = dmax1( Anr, (dabs(W(i))+dabs(vR(i))) )
   enddo
   if( iV.eq.1 ) then
    do ii = 1,N
     i = N + 1 - ii
     if( i.ne.N ) then
      if( G.ne.0.d0 ) then
       do j = l,N
        V(indo(j,i,N)) = ( U(indo(i,j,M))/U(indo(i,l,M)) )/G
       enddo
       do j = l,N
        S = 0.d0
        do k = l,N
         S = S + U(indo(i,k,M))*V(indo(k,j,N))
        enddo
        do k = l,N
         il     = indo(k,j,N)
         V(il) = V(il) + S*V(indo(k,i,N))
        enddo
       enddo
      endif
      do j = l,N
       V(indo(i,j,N)) = 0.d0
       V(indo(j,i,N)) = 0.d0
      enddo
     endif
     V(indo(i,i,N)) = 1.d0
     G = vR(i)
     l = i
    enddo
   endif
   if( iU.eq.1 ) then
    MN = N
    if( M.lt.N ) MN = M
    do ii = 1,MN
     i = MN + 1 - ii
     l = i + 1
     G = W(i)
     if( i.ne.N ) then
      do j = l,N
       U(indo(i,j,M)) = 0.d0
      enddo
     endif
     if( G.ne.0.d0 ) then
      if( i.ne.MN ) then
 	     do j = l,N
        S = 0.d0
        do k = l,M
         S = S + U(indo(k,i,M))*U(indo(k,j,M))
        enddo
        F = ( S/U(indo(i,i,M)) )/G
        do k = i,M
         il     = indo(k,j,M)
         U(il) = U(il) + F*U(indo(k,i,M))
        enddo
       enddo
      endif
      do j = i,M
       il = indo(j,i,M)
       U(il) = U(il) / G
      enddo
     elseif( G.eq.0.d0 ) then
      do j = i,M
       U(indo(j,i,M)) = 0.d0
      enddo
     endif
     il = indo(i,i,M)
     U(il) = U(il) + 1.d0
    enddo
   endif
   do kk = 1,N
    k1 = N - kk
    k  = k1 + 1
    its = 0
1   do ll = 1,k
     l1 = k - ll
     l  = l1 + 1
     if( (dabs(vR(l))+Anr)-Anr.lt.eps ) goto 3
     if( (dabs(W(l1))+Anr)-Anr.lt.eps ) goto 2
    enddo
2   C = 0.d0
    S = 1.d0
    do i = l,k
     F     = S * vR(i)
     vR(i) = C * vR(i)
     if( (dabs(F)+Anr)-Anr.lt.eps ) goto 3
     G = W(i)
     H = dsqrt( F*F + G*G )
     W(i) = H
     C =  G / H
     S = -F / H
     if( iU.eq.1 ) then
      do j = 1,M
       il = indo(j,l1,M)
       jl = indo(j,i,M)
       Y = U(il)
       Z = U(jl)
       U(il)  = Y*C + Z*S
       U(jl)  = Z*C	- Y*S
      enddo
     endif
    enddo
3   Z = W(k)
    if( l.ne.k ) then
     if( its.eq.100 ) then
      iErrO = k
      deallocate( vR )
      return
     endif
     its = its + 1
     X = W(l)
     Y = W(k1)
     G = vR(k1)
     H = vR(k)
     F = ( (Y-Z)*(Y+Z) + (G-H)*(G+H) )/(2.d0*H*Y)
     G = dsqrt( F*F + 1.d0 )
     F = ( (X-Z)*(X+Z) + H*( Y/(F+dsign(G,F))-H )) / X
     C = 1.d0
     S = 1.d0
     do i1 = l,k1
      i = i1 + 1
      G = vR(i)
      Y = W(i)
      H = S * G
      G = C * G
      Z = dsqrt( F*F + H*H )
      vR(i1) = Z
      C = F / Z
      S = H / Z
      F = X*C + G*S
      G = G*C - X*S 
      H = Y * S
      Y = Y * C
      if( iV.eq.1 ) then
       do j = 1,N
        il = indo(j,i1,N)
        jl = indo(j,i,N)
        X = V(il)
        Z = V(jl)
        V(il)  = X*C + Z*S
        V(jl)  = Z*C - X*S 
       enddo
      endif
      Z = dsqrt( F*F + H*H )
      W(i1) = Z
      if( Z.ne.0.d0 ) then
       C = F / Z
       S = H / Z
      endif
      F = C*G + S*Y
      X = C*Y - S*G 
      if( iU.eq.1 ) then
       do j = 1,M
        il = indo(j,i1,M)
        jl = indo(j,i,M)
        Y = U(il)
        Z = U(jl)
        U(il)  = Y*C + Z*S
        U(jl)  = Z*C - Y*S 
       enddo
      endif
     enddo
     vR(l) = 0.d0
     vR(k) = F
     W(k) = X
     goto 1
    endif
    if( Z.lt.0.d0 ) then
     W(k) = -Z
     if( iV.eq.1 ) then
      do j = 1,N
       il = indo(j,k,N)
       V(il) = -V(il)
      enddo
     endif
    endif
   enddo
   deallocate( vR )
   return
end subroutine svd_i

subroutine o_sdv1(A,B,N,K,iErrO)
   real*8    A(*),B(*)
   integer*4 N,K
   integer*1 i0,i1
   real*8    U(:),W(:),V(:)
   allocatable U,W,V
   iErrO = 0
   i0 = 0
   i1 = 2
   allocate( U(N*K),V(K*K),W(K) )
   call svd_i( A,N,K,U,1,V,1,W,iErrO )
   if(iErrO.ne.0) then
    deallocate( U,V,W )
    return
   endif
   call psvd(u,v,w,n,k,a,ierrO)
   deallocate( U,V,W )
   if( iErrO.gt.0 ) return
   call um2( a,i0,i0,B,i0,i0,B,i1,i0,K,N,1,iErrO)
   return
end subroutine o_sdv1

subroutine puch(Pu,kB,kL,iPr)
   real*8 Pu(*),vPu(6)
   !real*8 drad
   integer*4 kB,kL,iPr
   do i = 1,6
    if( iPr.eq.0 ) then
     vPu(i) = Pu(i)
    elseif( iPr.eq.1 .or. iPr.eq.2 ) then
     vPu(i) = drad( Pu(i) )
    endif
   enddo
! изменено Сермягиным 29.09.2009
   !kB = ( vPu(1)-vPu(2) ) / vPu(3) + 0.5
   !kL = ( vPu(5)-vPu(4) ) / vPu(6) + 0.5
! было у Майорова
   kB = ( vPu(1)-vPu(2) ) / vPu(3) + 1.5
   kL = ( vPu(5)-vPu(4) ) / vPu(6) + 1.5
   if( iPr.eq.1 ) then
    do i = 1,6
     Pu(i) = vPu(i)
    enddo
   endif
   return
end subroutine puch
integer*4 function nom_pp(i,j,Nraz)
  integer*4 i,j,Nraz
  if(i.ge.j) then
    nom_pp = (2*Nraz-j+3)*j/2+i-j+1
  elseif(i.lt.j) then
    nom_pp = (2*Nraz-i+3)*i/2+j-i+1
  endif
  return
end function nom_pp
real*8 function drad(x)
  real*8 x,y,pi
  pi=3.1415926535897932384626433832795d0
  if(x.ne.0) then
  y=dabs(x)
  kg=y/10000
  km=(y-kg*10000)/100
  y=y-kg*10000-km*100
  drad=(kg+km/60.d0+y/3600.d0)*pi/180.d0
  if(x.lt.0) drad=-drad
  else
  drad=0.d0
  end if
  return
end function drad
logical*4 function chet(k)
   integer k,n
   n=k/2
   chet=.false.
   if(n*2.eq.k) chet=.true.
   return
end function chet
end module sh_expand_ls
