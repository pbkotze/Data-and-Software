      program lomb
c
c  bereken periodes uit data met onreelmatige monster tempos
c
      character *80,opsk1
      dimension x(400),y(400),px(400),py(400),per(400),pyn(400)
c       dimension x(5000),y(5000),px(5000),py(5000),per(5000),
c     @pyn(3000)
c       dimension x(30000),y(30000),px(30000),py(30000),per(30000),
c     @pyn(30000)
c      open(10,file='c:\fieldsurveys\obsdata\her\2015\her_17mrt15_xyz.dat
c     @')
c      open(10,file='c:\data\indices\aa\aa1939dm.dat')
c      open(10,file='c:\data\imf\imf_crmod_16dm.dat') 
c       open(10,file='c:\data\indekse\f107\ge_f10.7_1840-2015.dat')
c        open(10,file='c:\models\calsk\pafuri_f_1600-1990.dat')
      open(10,file='c:\satellites\soho\soho_hep_57mev_2000-2019dm.dat')
c      open(10,file='c:\data\treering\c14\c14_1750-1850_hp.dat')
c      open(10,file='c:\data\treering\c13_1750-1850.dat')
c      open(10,file='c:\data\atmos\champ\ch_dens_2010dm.dat')
c        open(10,file='c:\data\sunmf\suv\sorce\muv_180-310_2019dm.dat')
c      open(10,file='c:\data\sunmf\suv\sorce\fuv_115-180_2004dm.dat')
c      open(10,file='c:\data\sunmf\lya\lya_2020dm.dat')
c      open(10,file='c:\data\neutron\jungfr\jfj_2019dm.dat')
c      open(10,file='c:\data\sunmf\tsi\sorce\tsi_sorce_2004dm.dat')
c      open(10,file='c:\data\sunmf\fexiv\fexiv_2005dm.dat')
c      open(10,file='c:\data\sunmf\lya\lya_1947-2020dm.dat')
c      open(10,file='c:\data\sunmf\mgii\mgii_1978-2020dm.dat')
c      open(10,file='c:\data\sunmf\ss\sn_hem_1945-2019dm.dat')
c       open(10,file='c:\data\vstar\dat\v339del_2014-2017dm.dat')
c      open(10,file='c:\data\imf\imf_bza_amnz_1995-2019dm.dat')
c      open(10,file='c:\data\imf\imf_balf_2009dm.dat')
c      open(10,file='c:\models\wavelets\nr\nino3sst.dat')
c     open(11,file='c:\models\wavelets\nr\nino3sst_95pera.dat')
c      open(11,file='c:\models\lomb\out\ae\ae_1971-1975pera.dat')
c        open(10,file='c:\data\indices\ler\lerk_60-10dm.dat')
c       open(11,file='c:\models\lomb\out\ler\lerk95_60-10pera.dat')
c       open(11,file='c:\models\lomb\out\sunmf\lya\lya_47-20_95pa.dat')
c      open(11,file='c:\models\lomb\out\sunmf\fexiv\fexiv_2005a_95p.dat') 
c      open(11,file='c:\models\lomb\out\vstar\v339del_15-17_95pa.dat')
c      open(11,file='c:\models\lomb\out\sunmf\tsi\tsi1_sorce04_95pa.dat')
c      open(11,file='c:\models\lomb\out\sunmf\mgii\mgii_78-20_95pa.dat')
c       open(11,file='c:\models\lomb\out\sunmf\suv\muv_2019_95pa.dat')
c       open(11,file='c:\models\lomb\out\sunmf\suv\fuv_2004_95pa.dat')
c       open(11,file='c:\models\lomb\out\sunmf\lya\lya_2020_95pa.dat')
c        open(11,file='c:\models\lomb\out\nm\jungfr\jfj_nm2019_95pa.dat')
c      open(11,file='c:\models\lomb\out\atmos\ch_dens10_95pa.dat')
c      open(11,file='c:\models\lomb\out\indices\aa\aa1939_95pa.dat')
c      open(12,file='c:\models\lomb\out\her\herk_2004-2009logpera.dat')
c      open(11,file='c:\models\lomb\out\tr\c14\c14_1750-1850hp_95.dat')
c      open(11,file='c:\models\lomb\out\tr\c13_1750-1850_95(b).dat')
c      open(11,file='c:\models\lomb\out\ace_sis\ace_O8_2020_95pa.dat')
        open(11,file='c:\models\lomb\out\soho\He_57_2019_95pa.dat')
c      open(11,file='c:\models\lomb\out\imf\imf_16crm_95pa.dat')
c      write(11,'(1x,a,3x,a,5x,a)')'Per','Pwr','99%'
c      write(6,'(1x,a,3x,a,5x,a)')'Per','Pwr','95%'
      write(11,'(a)')'    Per      Pwr     95%  NPwr  StError'
      
c      write(12,'(7x,a,3x,a,5x,a)')'Per','Pwr','90%'
      n=1
       READ(10,'(a)')opsk1
c1     read(10,*,end=100)idoy,fyr,e1,e2,e3,e4,e5,e6,e7,e8!ACE He format
c1      read(10,*,end=100)fyear,idoy,tsi1,tsi2 !flya !cr! ,snt,snn,sns
c1       read(10,*,end=100)fyr,idoy,fexiv !dns !f1,f2,f3,f107
c       iyr=fyr
c         idy=0
c1       read(10,*,end=100)fyr,idoy,nm
c1       read(10,*,end=100)icr,fyear,idoy,tav,tn,ts 
c      idy=idy+1
c1       read(10,*,end=100)fyr,idoy,bx,by,bz,entr,hep,vsw,am !omni2 
1       read(10,*,end=100)fyr,idoy,p,he,hep !b,vsw,crm !balf!flya
         iyr=fyr
c1     read(10,*,end=100)fyr,idoy,iaa
c      iyr=fyr
c      if(iyr.ge.1940.and.iyr.le.1941)then
c      e=e8
c      if(e.ge.1)goto 1
c      if(e.eq.-999.9) goto 1
      if(iyr.eq.2019)then
      continue
      else
      goto 1
      endif
c1      read(10,*,end=100)fday,ihr,hr,bx,by,bz,vx,vy,vz,fn,vsw,pdyn,temp,
c     @ae ! mn,hr,fx,fy,fz 
       x(n)=idoy !yr
c       f=f4
c       if(isnt.eq.0)goto 1
       y(n)=he !crm !fexiv !c14 !flya !crm !balf !am !iaa !flya 
c        y(n)=e
c2     format(f9.3,1x,i2,1x,i3,i4)
5     format(f8.2,1x,f8.1,1x,f6.2,1x,f4.1,1x,f5.1)
6     format(f12.6,1x,f12.6)
      n=n+1
      
      goto 1
100   n=n-1
      print * ,n
c      ofac=0.95 ! c14 !0.8 ! dens !1.5 ! 1.5 !standard
c      hifac=0.95 ! c14!1.3 !dens!1.3 !1.0 ! standard
c       ofac=0.75
c       hifac=1.51
c        ofac=0.415 !0.415 !0.515 !c14
c        hifac=1.88 !1.88 !1.98 !c14
       ofac=0.85 !c14! ACE GCR full range
       hifac=0.85 !c14!1.0 !0.95 ! ACE GCR full range
c       ofac=1.5 ! standard
c       hifac=.5 ! standard
c        ofac=0.85 !c14! 155 day lya 1yr interval
c        hifac=1.65 ! c14!155 day 1 yr interval
c       ofac=0.975  ! LyA
c       hifac=1.55  !155 lya LyA
c      ofac=0.95  ! elemenal abundance !suv!tsi
c      hifac=1.8 ! elemental abundance ! suv!tsi
c      ofac=1.72 ! 12 min SW data ! SS data! Fe XIV full range
c      hifac=0.85 ! 12 min SW data ! SS data! Fe XIV full range
c      ofac=1.95 ! Fe XIV full range
c      hifac=0.85
      np=n
     
      call period(x,y,n,ofac,hifac,px,py,np,nout,jmax,prob)
      call avevar(py,nout,aver,vari)
      s=sqrt(vari)
     
      conf99=2.58*s
      print*,'Confidence 99% = ',conf99
c      print*,prob,jmax
c      do i=1,nout
c      py1=aver-conf99
c      py2=aver+conf99
c      if(py(i).ge.conf99)then
c      per(i)=1/px(i)
c      print*,i,per(i)/60,py(i)
c      write(11,*)per(i)/60,py(i),conf99
c      endif
c      enddo
      conf95=1.96*s
      conf90=1.645*s
      print*,'Confidence 95% = ',conf95
      print*,'Confidence 90% = ',conf90
      print*,'Significance = ',prob
      write(6,'(a)')' Per(d)      Pwr     95%  NPwr  StError'
      do i=1,nout
      py1=aver-conf95
      py2=aver+conf95
c      if(py(i).ge.conf95)then
      per(i)=1/px(i)
      pyn(i)=py(i)/vari
      flpx=log10(px(i))
      flpy=log10(py(i))
c      per(i)=per(i)*12       ! skakel om na min periodes
      if(per(i).gt.1.)then
      write(6,5)per(i),py(i),conf95,pyn(i),s
c      print*,i,per(i),py(i),conf95,conf90,pyn(i)
      endif
c       if(per(i).gt.6.0)then
      write(11,5)per(i),py(i),conf95,pyn(i),s
c      endif
c      if(per(i).lt.60.and.per(i).gt.6)then
c      write(12,6)flpx,flpy
c      endif
c      if(py(i).ge.conf90)then
c      per(i)=1/px(i)
c       print*,i,per(i),py(i),conf90
c      write(12,*)per(i),py(i),conf90
c      endif
      enddo
c      print*,prob,jmax,aver,vari,s,conf99
     
      stop
      end
