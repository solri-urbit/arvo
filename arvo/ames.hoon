!:  ::  ames (4a), networking  
::
  |=  pit/vase
  =>  =~
::  structures
|%
++  move  {p/duct q/(wind note gift-ames)}              ::  local move
++  note                                                ::  out request $->
          $?  $:  $d                                    ::  to %dill
          $%  {$flog p/flog}                            ::
          ==  ==                                        ::
              $:  $a                                    ::  to %ames
          $%  {$kick p/@da}                             ::
          ==  ==                                        ::
              $:  $g                                    ::  to %gall
          $%  {$deal p/sock q/cush}                     ::
          ==  ==                                        ::
              $:  @tas                                  ::  to any
          $%  {$init p/@p}                              ::
              {$want p/sock q/path r/*}                 ::
              {$wart p/sock q/@tas r/path s/*}          ::
              {$west p/sack q/path r/*}                 ::
          ==  ==  ==                                    ::
++  sign                                                ::  in result _<-
          $?  $:  $a                                    ::  from %ames
          $%  {$went p/ship q/cape}                     ::
          ==  ==                                        ::
              $:  $g                                    ::  from %gall
          $%  {$unto p/cuft}                            ::
              {$mean p/ares}                            ::  XX old clean up
              {$nice $~}                                ::
          ==  ==                                        ::
              $:  @tas                                  ::
          $%  {$crud p/@tas q/(list tank)}              ::  by any
              {$mack p/(unit tang)}                     ::  message ack
          ==  ==  ==                                    ::
--
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::              section 4aA, identity logic           ::
  ::
  |%
  ::
  ++  grip                                              ::  extend will
    |=  {wet/will law/will}
    ^-  will
    ?~  wet  law
    ?:  =(wet law)  law
    ?^  t.wet
      ?>((meld i.wet i.t.wet) [i.wet $(wet t.wet)])
    ?~  law
      ?>((pier i.wet) [i.wet ~])
    ?~  q.p.q.i.wet
      ?>((meld i.wet i.law) [i.wet law])
    =+  rul=(sein r.p.q.i.wet)
    |-  ^-  will
    ?:  ?&  =(rul r.p.q.i.law)
            =(p.p.q.i.law u.q.p.q.i.wet)
        ==
      ?>((meld i.wet i.law) [i.wet law])
    ?>(?=(^ t.law) $(law t.law))
  ::
  ++  meld                                              ::  verify connect
    |=  {new/deed old/deed}
    ^-  $&
    ?>  (melt new old)
    ?>  =((shaf %meld (sham q.new)) (need (sure:as:(com:nu:crub r.q.old) *code p.new)))
    %&
  ::
  ++  melt                                              ::  proper connect
    |=  {new/deed old/deed}
    ^-  ?
    =+  rac=(clan r.p.q.new)
    ?&  =(r.new r.old)                                  ::  match fake
        ?~  q.p.q.new
          ?&  =(r.p.q.old r.p.q.new)
              &(!=(%earl rac) =(p.p.q.old (dec p.p.q.new)))
          ==
        ?&  &(!=(%pawn rac) !=(%czar rac))
            |(=(0 p.p.q.new) =(%earl rac))
            =(r.p.q.old (sein r.p.q.new))
            =(p.p.q.old u.q.p.q.new)
        ==
    ==
  ::
  ++  pare                                              ::  shorten against
    |=  {fou/will law/will}
    ::  ~&  [%pare-fou fou]
    ::  ~&  [%pare-law law]
    ^-  will
    =+  [ouf=(flop fou) wal=(flop law)]
    %-  flop
    |-  ^-  will
    ?~  ouf  wal
    ?~  wal  !!
    ?.  =(i.wal i.ouf)  ouf
    $(wal t.wal, ouf t.ouf)
  ::
  ++  pier  !:                                          ::  initial deed
    |=  wed/deed
    ^-  $&
    ?>  =+  rac=(clan r.p.q.wed)
        =+  loy=(com:nu:crub r.q.wed)
        ?:  &(r.wed =(rac %czar))  %&
        ?>  =(0 p.p.q.wed)
        ?>  =(fig:ex:loy ?+(rac !! $czar (zeno r.p.q.wed), $pawn r.p.q.wed))
        ?>  =((shaf %self (sham q.wed)) (need (sure:as:loy *code p.wed)))
        %&
    %&
  ::
  ++  real                                              ::  validate
    |=  {mac/mace law/will}
    ?>  ?&  |-  ^-  ?
            ?~  mac  &
            ?>  ?&  ?=(^ law)
                    (lth p.p.q.i.law 9)                 ::  9-lives rule
                    =(p.p.q.i.law p.i.mac)
                    =(r.q.i.law pub:ex:(nol:nu:crub q.i.mac))
                ==
            $(mac t.mac, law t.law)
        ==
    %&
  ::
  ++  rice                                              ::  mace at life
    |=  {mar/life mac/mace}
    ^-  (unit mace)
    ?~  mac  ~
    ?:  =(mar p.i.mac)  [~ mac]
    ?:  (gth mar p.i.mac)  ~
    $(mac t.mac)
  ::
  ++  rick                                              ::  will at life
    |=  {mar/life lag/ship law/will}
    ^-  (unit will)
    ?~  law  ~
    ?:  =(mar p.p.q.i.law)  [~ law]
    ?:  (gth mar p.p.q.i.law)  ~
    ?:  |(?=($~ q.p.q.i.law) !=(lag r.p.q.i.law))  ~
    $(law t.law)
  ::
  ++  zeno                                              ::  imperial keyprint
    |=  zar/@pD
    ^-  @uvH  ^-  @
    %+  snag  zar
    ^-  (list @uw)
    :~  0w32.fDSZy.0ZCHX.49suL.i4eOC  ::    0, ~zod
        0w0                           ::    1, ~nec
        0w0                           ::    2, ~bud
        0w0                           ::    3, ~wes
        0w0                           ::    4, ~sev
        0w0                           ::    5, ~per
        0w0                           ::    6, ~sut
        0w0                           ::    7, ~let
        0w0                           ::    8, ~ful
        0w0                           ::    9, ~pen
        0w0                           ::   10, ~syt
        0w0                           ::   11, ~dur
        0w0                           ::   12, ~wep
        0w0                           ::   13, ~ser
        0w0                           ::   14, ~wyl
        0w2g.T8lmF.OeCBc.t1dOZ.x7RnI  ::   15, ~sun
        0w0                           ::   16, ~ryp
        0w0                           ::   17, ~syx
        0w0                           ::   18, ~dyr
        0w0                           ::   19, ~nup
        0w0                           ::   20, ~heb
        0w0                           ::   21, ~peg
        0w0                           ::   22, ~lup
        0w0                           ::   23, ~dep
        0w0                           ::   24, ~dys
        0w0                           ::   25, ~put
        0w0                           ::   26, ~lug
        0w0                           ::   27, ~hec
        0w0                           ::   28, ~ryt
        0w0                           ::   29, ~tyv
        0w0                           ::   30, ~syd
        0w0                           ::   31, ~nex
        0w0                           ::   32, ~lun
        0wp.SLJ-w.6Os8J.mA7-m.BWwqe   ::   33, ~mep
        0w0                           ::   34, ~lut
        0w0                           ::   35, ~sep
        0w0                           ::   36, ~pes
        0w0                           ::   37, ~del
        0w0                           ::   38, ~sul
        0w0                           ::   39, ~ped
        0w0                           ::   40, ~tem
        0w0                           ::   41, ~led
        0w0                           ::   42, ~tul
        0w0                           ::   43, ~met
        0w0                           ::   44, ~wen
        0w0                           ::   45, ~byn
        0w0                           ::   46, ~hex
        0w0                           ::   47, ~feb
        0w0                           ::   48, ~pyl
        0w0                           ::   49, ~dul
        0w0                           ::   50, ~het
        0w0                           ::   51, ~mev
        0w0                           ::   52, ~rut
        0w0                           ::   53, ~tyl
        0w0                           ::   54, ~wyd
        0w0                           ::   55, ~tep
        0w0                           ::   56, ~bes
        0w0                           ::   57, ~dex
        0w0                           ::   58, ~sef
        0w0                           ::   59, ~wyc
        0w0                           ::   60, ~bur
        0w0                           ::   61, ~der
        0w0                           ::   62, ~nep
        0w0                           ::   63, ~pur
        0w0                           ::   64, ~rys
        0w0                           ::   65, ~reb
        0w0                           ::   66, ~den
        0w0                           ::   67, ~nut
        0w0                           ::   68, ~sub
        0w0                           ::   69, ~pet
        0w0                           ::   70, ~rul
        0w0                           ::   71, ~syn
        0w0                           ::   72, ~reg
        0w0                           ::   73, ~tyd
        0w0                           ::   74, ~sup
        0w0                           ::   75, ~sem
        0w0                           ::   76, ~wyn
        0w0                           ::   77, ~rec
        0w0                           ::   78, ~meg
        0w0                           ::   79, ~net
        0w0                           ::   80, ~sec
        0w0                           ::   81, ~mul
        0w0                           ::   82, ~nym
        0w0                           ::   83, ~tev
        0w0                           ::   84, ~web
        0w0                           ::   85, ~sum
        0w0                           ::   86, ~mut
        0w0                           ::   87, ~nyx
        0w0                           ::   88, ~rex
        0w0                           ::   89, ~teb
        0w0                           ::   90, ~fus
        0w0                           ::   91, ~hep
        0w0                           ::   92, ~ben
        0w0                           ::   93, ~mus
        0w0                           ::   94, ~wyx
        0w0                           ::   95, ~sym
        0w0                           ::   96, ~sel
        0w0                           ::   97, ~ruc
        0w0                           ::   98, ~dec
        0w0                           ::   99, ~wex
        0w0                           ::  100, ~syr
        0w0                           ::  101, ~wet
        0w0                           ::  102, ~dyl
        0w0                           ::  103, ~myn
        0w0                           ::  104, ~mes
        0w0                           ::  105, ~det
        0w0                           ::  106, ~bet
        0w0                           ::  107, ~bel
        0w0                           ::  108, ~tux
        0w0                           ::  109, ~tug
        0w0                           ::  110, ~myr
        0w0                           ::  111, ~pel
        0w0                           ::  112, ~syp
        0w0                           ::  113, ~ter
        0w0                           ::  114, ~meb
        0w0                           ::  115, ~set
        0w0                           ::  116, ~dut
        0w0                           ::  117, ~deg
        0w0                           ::  118, ~tex
        0w0                           ::  119, ~sur
        0w0                           ::  120, ~fel
        0w0                           ::  121, ~tud
        0w0                           ::  122, ~nux
        0w0                           ::  123, ~rux
        0w0                           ::  124, ~ren
        0w0                           ::  125, ~wyt
        0w0                           ::  126, ~nub
        0w0                           ::  127, ~med
        0w0                           ::  128, ~lyt
        0w0                           ::  129, ~dus
        0w0                           ::  130, ~neb
        0w0                           ::  131, ~rum
        0w0                           ::  132, ~tyn
        0w0                           ::  133, ~seg
        0w0                           ::  134, ~lyx
        0w0                           ::  135, ~pun
        0w0                           ::  136, ~res
        0w0                           ::  137, ~red
        0w0                           ::  138, ~fun
        0w0                           ::  139, ~rev
        0w0                           ::  140, ~ref
        0w0                           ::  141, ~mec
        0w0                           ::  142, ~ted
        0w0                           ::  143, ~rus
        0w0                           ::  144, ~bex
        0w0                           ::  145, ~leb
        0w0                           ::  146, ~dux
        0w0                           ::  147, ~ryn
        0w0                           ::  148, ~num
        0w0                           ::  149, ~pyx
        0w0                           ::  150, ~ryg
        0w0                           ::  151, ~ryx
        0w0                           ::  152, ~fep
        0w0                           ::  153, ~tyr
        0w0                           ::  154, ~tus
        0w0                           ::  155, ~tyc
        0w0                           ::  156, ~leg
        0w0                           ::  157, ~nem
        0w0                           ::  158, ~fer
        0w0                           ::  159, ~mer
        0w0                           ::  160, ~ten
        0w0                           ::  161, ~lus
        0w0                           ::  162, ~nus
        0w0                           ::  163, ~syl
        0w0                           ::  164, ~tec
        0w0                           ::  165, ~mex
        0w0                           ::  166, ~pub
        0w0                           ::  167, ~rym
        0w0                           ::  168, ~tuc
        0w0                           ::  169, ~fyl
        0w0                           ::  170, ~lep
        0w0                           ::  171, ~deb
        0w0                           ::  172, ~ber
        0w0                           ::  173, ~mug
        0w0                           ::  174, ~hut
        0w0                           ::  175, ~tun
        0w0                           ::  176, ~byl
        0w0                           ::  177, ~sud
        0w0                           ::  178, ~pem
        0w0                           ::  179, ~dev
        0w0                           ::  180, ~lur
        0w0                           ::  181, ~def
        0w0                           ::  182, ~bus
        0w0                           ::  183, ~bep
        0w0                           ::  184, ~run
        0w0                           ::  185, ~mel
        0w0                           ::  186, ~pex
        0w0                           ::  187, ~dyt
        0w0                           ::  188, ~byt
        0w0                           ::  189, ~typ
        0w0                           ::  190, ~lev
        0w0                           ::  191, ~myl
        0w0                           ::  192, ~wed
        0w0                           ::  193, ~duc
        0w0                           ::  194, ~fur
        0w0                           ::  195, ~fex
        0w0                           ::  196, ~nul
        0w0                           ::  197, ~luc
        0w0                           ::  198, ~len
        0w0                           ::  199, ~ner
        0w0                           ::  200, ~lex
        0w0                           ::  201, ~rup
        0w0                           ::  202, ~ned
        0w0                           ::  203, ~lec
        0w0                           ::  204, ~ryd
        0w0                           ::  205, ~lyd
        0w0                           ::  206, ~fen
        0w0                           ::  207, ~wel
        0w0                           ::  208, ~nyd
        0w0                           ::  209, ~hus
        0w0                           ::  210, ~rel
        0w0                           ::  211, ~rud
        0w0                           ::  212, ~nes
        0w0                           ::  213, ~hes
        0w0                           ::  214, ~fet
        0w0                           ::  215, ~des
        0w0                           ::  216, ~ret
        0w0                           ::  217, ~dun
        0w0                           ::  218, ~ler
        0w0                           ::  219, ~nyr
        0w0                           ::  220, ~seb
        0w0                           ::  221, ~hul
        0w0                           ::  222, ~ryl
        0w0                           ::  223, ~lud
        0w0                           ::  224, ~rem
        0w0                           ::  225, ~lys
        0w0                           ::  226, ~fyn
        0w0                           ::  227, ~wer
        0w0                           ::  228, ~ryc
        0w0                           ::  229, ~sug
        0w0                           ::  230, ~nys
        0w0                           ::  231, ~nyl
        0w0                           ::  232, ~lyn
        0w0                           ::  233, ~dyn
        0w0                           ::  234, ~dem
        0w0                           ::  235, ~lux
        0w0                           ::  236, ~fed
        0w0                           ::  237, ~sed
        0w0                           ::  238, ~bec
        0w0                           ::  239, ~mun
        0w0                           ::  240, ~lyr
        0w0                           ::  241, ~tes
        0w0                           ::  242, ~mud
        0w0                           ::  243, ~nyt
        0w0                           ::  244, ~byr
        0w0                           ::  245, ~sen
        0w0                           ::  246, ~weg
        0w0                           ::  247, ~fyr
        0w0                           ::  248, ~mur
        0w0                           ::  249, ~tel
        0w0                           ::  250, ~rep
        0w0                           ::  251, ~teg
        0w0                           ::  252, ~pec
        0w0                           ::  253, ~nel
        0w0                           ::  254, ~nev
        0w0                           ::  255, ~fes
    ==
  --
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::              section 4aB, packet format            ::
  ::
  |%
  ++  bite                                              ::  packet to cake
    |=  pac/rock  ^-  cake
    =+  [mag=(end 5 1 pac) bod=(rsh 5 1 pac)]
    =+  :*  vez=(end 0 3 mag)                           ::  protocol version
            chk=(cut 0 [3 20] mag)                      ::  checksum
            wix=(bex +((cut 0 [23 2] mag)))             ::  width of receiver
            vix=(bex +((cut 0 [25 2] mag)))             ::  width of sender
            tay=(cut 0 [27 5] mag)                      ::  message type
        ==
    ?>  =(6 vez)
    ?>  =(chk (end 0 20 (mug bod)))
    :+  [(end 3 wix bod) (cut 3 [wix vix] bod)]
      (kins tay)
    (rsh 3 (add wix vix) bod)
  ::
  ++  kins  |=(tay/@ (snag tay `(list skin)`[%none %open %fast %full ~]))
  ++  ksin  |=(sin/skin `@`?-(sin $none 0, $open 1, $fast 2, $full 3))
  ++  spit                                              ::  cake to packet
    |=  kec/cake  ^-  @
    =+  wim=(met 3 p.p.kec)
    =+  dum=(met 3 q.p.kec)
    =+  yax=?:((lte wim 2) 0 ?:((lte wim 4) 1 ?:((lte wim 8) 2 3)))
    =+  qax=?:((lte dum 2) 0 ?:((lte dum 4) 1 ?:((lte dum 8) 2 3)))
    =+  wix=(bex +(yax))
    =+  vix=(bex +(qax))
    =+  bod=:(mix p.p.kec (lsh 3 wix q.p.kec) (lsh 3 (add wix vix) r.kec))
    =+  tay=(ksin q.kec)
    %+  mix
      %+  can  0
      :~  [3 6]
          [20 (mug bod)]
          [2 yax]
          [2 qax]
          [5 tay]
      ==
    (lsh 5 1 bod)
  --
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::              section 4aC, PKI engine               ::
  ::
  |%
  ++  go                                                ::    go
    |_  ton/town                                        ::  ames state
    ++  as                                              ::    as:go
      |_  {our/ship saf/sufi}                           ::  per server
      ++  born                                          ::    born:as:go
        |=  {now/@da her/@p tic/@pG ges/gens pub/pass}  ::  register user
        ^-  {(unit will) _+>}
        ?.  =(our (sein her))  [~ +>.$]
        =+  nes=sen
        =+  ryt=(end 6 1 (shaf %tick (mix her (shax sec:ex:q.nes))))
        ?.  =(tic ryt)
          ~&  [%ames-wrong-ticket `@p`ryt]
          [~ +>.$]
        =+  rad=(~(get by hoc.saf) her)
        ?^  rad
          ?.  ?=(^ lew.wod.u.rad)
            $(hoc.saf (~(del by hoc.saf) her))          :: XX how can this be?
          ?.  =(pub r.q.i.lew.wod.u.rad)  [~ +>.$]
          [[~ lew.wod.u.rad] +>.$]
        =+  syp=[[0 [~ p.nes] her now] ges pub]
        =+  ded=[(sign:as:q.nes *code (shaf %meld (sham syp))) syp fak.ton]
        =+  wil=[ded law.saf]
        ?>  =(wil (grip wil ~))
        :-  [~ wil]
        +>.$(hoc.saf (~(put by hoc.saf) her [[~31337.1.1 ~ wil] ~ *clot]))
      ::
      ++  lax                                           ::    lax:as:go
        |_  {her/ship dur/dore}                         ::  per client
        ++  cluy                                        ::    cluy:lax:as:go
          ^-  {p/life q/gens r/acru}                    ::  client crypto
          ?~  lew.wod.dur  !!
          ?.  =(fak.ton r.i.lew.wod.dur)  ~|([%client-wrong-fake her] !!)
          :+  p.p.q.i.lew.wod.dur
            q.q.i.lew.wod.dur
          (com:nu:crub r.q.i.lew.wod.dur)
        ::
        ++  clon
          ^-  life
          ?~(lew.wod.dur 0 p.p.q.i.lew.wod.dur)
        ::
        ++  deng
          |=  law/will
          %_(+> lew.wod.dur (grip law lew.wod.dur))
        ::
        ++  griz                                        ::    griz:lax:as:go
          |=  now/@da                                   ::  generate key for
          ^-  {p/code q/_+>}
          =+  key=(shas %enty (mix now any.ton))
          :-  key
          %=  +>.$
            any.ton      (shax (mix now any.ton))
            heg.caq.dur  (~(put by heg.caq.dur) (shaf %hand key) key)
          ==
        ::
        ++  pode                                        ::    pode:lax:as:go
          |=  now/@da                                   ::  timeout route
          ^+  +>
          ?:  (lth her 256)  +>(lun.wod.dur [~ %if ~2000.1.1 0 (mix her .0.0.1.0)])
          +>(lun.wod.dur ~)
        ::
        ++  kuch                                        ::    kuch:lax:as:go
          |=  had/hand                                  ::  hear key tag
          ^-  (unit {code _+>})
          =+  wey=(~(get by heg.caq.dur) had)
          ?^  wey
            =+  key=u.wey
            :+  ~  key
            %=    ..kuch
                yed.caq.dur  [~ had u.wey]
                heg.caq.dur  (~(del by heg.caq.dur) had)
                qim.caq.dur  (~(put by qim.caq.dur) had key)
            ==
          =+  dyv=(~(get by qim.caq.dur) had)
          ?~  dyv  ~
          [~ u.dyv ..kuch]
        ::
        ++  wasc                                        ::    wasc:lax:as:go
          |=  key/code                                  ::  hear foreign code
          ^+  +>
          =+  had=(shaf %hand key)
          %_  ..wasc
            yed.caq.dur  [~ had key]
            qim.caq.dur  (~(put by qim.caq.dur) had key)
          ==
        ::
        ++  wast                                        ::    wast:lax:as:go
          |=  ryn/lane                                  ::  set route
          ^+  +>
          %=    +>
              lun.wod.dur
            ?:  ?=({$ix *} ryn)
              ?:  ?|  ?=($~ lun.wod.dur)
                      ?=({$ix *} u.lun.wod.dur)
                      ?&  ?=({$if *} u.lun.wod.dur)
                          (gth p.ryn (add ~s10 p.u.lun.wod.dur))
                      ==
                  ==
                [~ ryn]
              lun.wod.dur
            [~ ryn]
          ==
        ::
        ++  wist                                        ::    wist:lax:as:go
          |=  $:  now/@da                                ::  route via
                  waz/(list @p)
                  ryn/(unit lane)
                  pac/rock
              ==
          ^-  (list boon)
          ?:  =(our her)  [[%ouzo *lane pac] ~]
          ?~  waz  ~
          =+  dyr=?:(=(her i.waz) dur (gur i.waz))
          ?.  ?&  !=(our i.waz)
                  ?=(^ lun.wod.dyr)
              ==
            $(waz t.waz)
          :_  ?:  ?=($ix -.u.lun.wod.dyr)
                $(waz t.waz)
              ~
          :+  %ouzo  u.lun.wod.dyr
          ?:  &(=(i.waz her) =(~ ryn))  pac
          =+  mal=(jam `meal`[%fore her ryn pac])
          %-  spit
          ^-  cake
          :*  [our i.waz]
              ?~  yed.caq.dyr  [%none mal]
              :-  %fast
              %^  cat  7
                p.u.yed.caq.dyr
              (en:crub q.u.yed.caq.dyr mal)
          ==
        ::
        ++  xeno                                        ::    xeno:lax:as:go
          ^-  (list ship)                               ::  foreign canon
          (saxo her)
        ::
        ++  xong                                        ::    xong:lax:as:go
          ^-  (list ship)                               ::  route unto
          =+  [fro=xen too=xeno]
          =+  ^=  oot  ^-  (list ship)
              =|  oot/(list ship)
              |-  ^+  oot
              ?~  too  ~
              ?:  (lien fro |=(a/ship =(a i.too)))  ~
              [i.too $(too t.too)]
          ::  ~&  [%xong-to [our her] (weld oot ?>(?=(^ fro) t.fro))]
          (weld oot ?>(?=(^ fro) t.fro))
        ::
        ++  zuul                                        ::    zuul:lax:as:go
          |=  {now/@da ham/meal}                        ::  encode message
          ^-  {p/(list rock) q/_+>}
          =<  weft
          |%
          ++  wasp                                      ::  null security
            ^-({p/skin q/@} [%none (jam ham)])
          ::
          ++  weft                                      ::  fragment message
            ^-  {p/(list rock) q/_+>.$}
            =^  gim  ..weft  wisp
            :_  +>.$
            ^-  (list rock)
            =+  wit=(met ?:(fak.ton 16 13) q.gim)
            ?<  =(0 wit)
            ?:  =(1 wit)
              =+  yup=(spit [our her] p.gim q.gim)
              [yup ~]
            =+  ruv=(rip ?:(fak.ton 16 13) q.gim)
            =+  gom=(shaf %thug q.gim)
            =+  inx=0
            |-  ^-  (list rock)
            ?~  ruv  ~
            =+  ^=  vie
                %+  spit
                  [our her]
                wasp(ham [%carp (ksin p.gim) inx wit gom i.ruv])
            :-  vie
            $(ruv t.ruv, inx +(inx))
          ::
          ++  wisp                                      ::  generate message
            ^-  {{p/skin q/@} q/_..wisp}
            ?:  =(%carp -.ham)
              [wasp ..wisp]
            ?:  !=(~ yed.caq.dur)
              ?>  ?=(^ yed.caq.dur)
              :_  ..wisp
              :-  %fast
              %^  cat  7
                p.u.yed.caq.dur
              (en:r:cluy q.u.yed.caq.dur (jam ham))
            ?:  &(=(~ lew.wod.dur) |(=(%back -.ham) =(%buck -.ham)))
              [wasp ..wisp]
            =^  tuy  +>.$
              ?:(=(~ lew.wod.dur) [*code +>.$] (griz now))
            :_  ..wisp
            =+  yig=sen
            ::  =+  bil=`will`(pare wyl.dur law.saf)    ::  XX not set
            =+  bil=law.saf                             ::  XX send whole will
            =+  hom=(jam ham)
            ?:  =(~ lew.wod.dur)
              :-  %open
              %^    jam
                  [~ `life`p.yig]
                bil
              (sign:as:q.yig tuy hom)
            :-  %full
              =+  cay=cluy
              %^    jam
                  [`life`p.cay `life`p.yig]
                bil
              (seal:as:q.yig pub:ex:r.cay tuy hom)
          --                                            ::  --zuul:lax:as:go
        --                                              ::  --lax:as:go
      ::
      ++  gur                                           ::  default dore
        |=  her/ship
        ^-  dore
        =+  def=?.((lth her 256) ~ [~ %if ~2000.1.1 0 (mix her .0.0.1.0)])
        [[~2100.1.1 def ~] ~ *clot]
      ::
      ++  myx                                           ::  dore by ship
        |=  her/ship
        ^+  lax
        =+  fod=(~(get by hoc.saf) her)
        ~(. lax [her ?~(fod (gur her) u.fod)])
      ::
      ++  nux                                           ::  install dore
        |=  new/_lax
        ^+  +>
        +>(hoc.saf (~(put by hoc.saf) her.new dur.new))
      ::
      ++  sen                                           ::  current crypto
        ^-  {p/life q/acru}
        ?~(val.saf !! [p.i.val.saf r.i.val.saf])
      ::
      ++  sev                                           ::  crypto by life
        |=  mar/life
        ^-  {p/? q/acru}
        ?~  val.saf  !!
        ?:  =(mar p.i.val.saf)
          [& r.i.val.saf]
        ?>  (lth mar p.i.val.saf)
        :-  |
        |-  ^-  acru
        ?>  ?=(^ t.val.saf)
        ?:  =(mar p.i.t.val.saf)
          r.i.t.val.saf
        $(t.val.saf t.t.val.saf)
      ::
      ++  sex                                           ::  export secrets
        |-  ^-  mace
        ?~  val.saf  ~
        :-  [p.i.val.saf sec:ex:r.i.val.saf]
        $(val.saf t.val.saf)
      ::
      ++  xen                                           ::  canon
        |-  ^-  (list ship)
        (saxo our)
      ::
      ++  yew                                           ::  best will for
        |=  her/ship
        ^-  will
        =+  gel=(~(get by hoc.saf) her)
        ?^  gel
          lew.wod.u.gel
        ?:((lth her 256) ~ $(her (sein her)))
      --                                                ::  --as:go
    ::
    ++  ha  !:                                          ::  adopt new license
      |=  {our/ship mac/mace wil/will}
      ^-  town
      ?>  !=(~ mac)
      ?>  ?=(^ wil)
      ::  ?>  =(our r.p.q.i.wil)
      ?>  =(wil (grip wil ~))
      ?>  (real mac wil)
      %_    ton
          fak  r.i.wil
          urb
        %+  ~(put by urb.ton)
          our
        :*  %-  flop
            |-  ^-  (list ship)
            ?:((lth our 256) ~ =+(seg=(sein our) [seg $(our seg)]))
        ::
            (turn mac |=({p/life q/ring} [p q (nol:nu:crub q)]))
            wil
            ~
            ~
        ==
      ==
    ::
    ++  su                                              ::  install safe
      |=  new/_as
      ^-  town
      ton(urb (~(put by urb.ton) our.new saf.new))
    ::
    ++  ti                                              ::  expire by time
      |=  now/@da
      ^-  town
      !!
    ::
    ++  us                                              ::  produce safe
      |=  our/ship
      ^-  (unit _as)
      =+  goh=(~(get by urb.ton) our)
      ?~  goh  ~
      [~ ~(. as [our u.goh])]
    --                                                ::  --go
  --
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::              section 4aF, packet pump             ::
  |%
  ++  pu                                                ::  packet pump
    |_  shed
    ++  abet  +<
    ++  ahoy                                            ::    ahoy:pu
      ^+  .                                             ::  initialize
      %_    .
          rtt  ~s1
          rto  ~s4
          rtn  ~
          rue  ~
          nus  0
          nif  0
          nep  0
          caw  2
          cag  64
          diq  ~
          pyz  ~
          puq  ~
      ==
    ::
    ++  bick                                            ::    bick:pu
      |=  {now/@da fap/flap}                            ::  ack by hash
      ^-  {{p/(unit soup) q/(list rock)} _+>}
      =+  sun=(~(get by diq) fap)
      ?~  sun
        ::  ~&  [%bick-none `@p`(mug fap)]              ::  not a real error
        [[~ ~] +>.$]
      ::  ~&  [%bick-good `@p`(mug fap) u.sun]
      =.  diq  (~(del by diq) fap)
      =^  gub  +>.$  (bock now u.sun)
      =^  yop  +>.$  (harv now)
      [[gub yop] +>.$]
    ::
    ++  bilk                                            ::    bilk:pu
      |=  now/@da                                       ::  inbound packet
      ^+  +>
      =+  trt=(mul 2 rtt)
      %=  +>.$
        rue  [~ now]
        rto  trt
        rtn  ?~(puq ~ [~ (add now trt)])
      ==
    ::
    ++  boom                                            ::    boom:pu
      |=  now/@da  ^-  ?                                ::  address timeout
      |(?=($~ rue) (gte (sub now u.rue) ~m1))
    ::
    ++  bust                                            ::    bust:pu
      ^-  ?                                             ::  not responding
      &(?=(^ rtn) (gte rto ~s16))
    ::
    ++  bike                                            ::    bike:pu
      ^+  .                                             ::  check stats
      ?>  .=  nif
          |-  ^-  @
          ?~  puq  0
          :(add !liv.q.n.puq $(puq l.puq) $(puq r.puq))
      .
    ::
    ++  beet                                            ::    beet:pu
      ^+  .                                             ::  advance unacked
      =-  +(nep ?~(foh nus u.foh))
      ^=  foh
      |-  ^-  (unit @ud)
      ?~  puq  ~
      ?:  (lte p.n.puq nep)  $(puq l.puq)
      =+  rig=$(puq r.puq)
      ?^(rig rig [~ p.n.puq])
    ::
    ++  bine                                            ::    bine:pu
      |=  {now/@da num/@ud}                             ::  apply ack
      ^-  {(unit soup) _+>}
      ?~  puq  !!
      ?.  =(num p.n.puq)
        ?:  (gth num p.n.puq)
          =+  lef=$(puq l.puq)
          [-.lef +.lef(puq [n.puq puq.lef r.puq])]
        =+  rig=$(puq r.puq)
        [-.rig +.rig(puq [n.puq l.puq puq.rig])]
      =:  rtt  ?.  &(liv.q.n.puq =(1 nux.q.n.puq))  rtt
               =+  gap=(sub now lys.q.n.puq)
               ::  ~&  [%bock-trip num (div gap (div ~s1 1.000))]
               (div (add (mul 2 rtt) gap) 3)
          nif  (sub nif !liv.q.n.puq)
        ==
      =+  lez=(dec (need (~(get by pyz) gom.q.n.puq)))
      =^  gub  pyz
          ?:  =(0 lez)
            [[~ gom.q.n.puq] (~(del by pyz) gom.q.n.puq)]
          [~ (~(put by pyz) gom.q.n.puq lez)]
      :-  gub
      +>.$(puq ~(nap to puq))
    ::
    ++  bock                                            ::    bock:pu
      |=  {now/@da num/@ud}                             ::  ack by sequence
      ^-  {(unit soup) _+>}
      =^  gym  +>  (bine now num)
      :-  gym
      ?:  (gth num nep)
        =+  cam=(max 2 (div caw 2))
        ::  ~&  [%bock-hole num nep cam]
        beet:(wept(nep num, cag cam, caw cam) nep num)
      =.  caw  ?:  (lth caw cag)  +(caw)
               (add caw !=(0 (mod (mug now) caw)))
      ?:  =(num nep)
        ::  ~&  [%bock-fine num nif caw cag]
        beet
      ::  ~&  [%bock-fill num nif caw cag]
      +>.$
    ::
    ++  harv                                            ::    harv:pu
      |=  now/@da                                       ::  harvest queue
      ^-  {(list rock) _+>}
      ?:  =(~ puq)  [~ +>(rtn ~)]
      ?.  (gth caw nif)  [~ +>]
      =+  wid=(sub caw nif)
      =|  rub/(list rock)
      =<  abet  =<  apse
      |%
      ++  abet
        ?~  rub  [~ +>.$]
        [(flop rub) +>.$(rtn [~ (add rto now)])]
      ::
      ++  apse
        ^+  .
        ?~  puq  .
        ?:  =(0 wid)  .
        =>  rigt  =<  left
        ?>  ?=(^ puq)
        ?:  =(0 wid)  .
        ?.  =(| liv.q.n.puq)  .
        ::  ~&  [%harv nux.q.n.puq p.n.puq]
        %_    .
          wid          (dec wid)
          rub          [pac.q.n.puq rub]
          nif          +(nif)
          liv.q.n.puq  &
          nux.q.n.puq  +(nux.q.n.puq)
          lys.q.n.puq  now
        ==
      ::
      ++  left
        ?>  ?=(^ puq)
        ^+(. =+(lef=apse(puq l.puq) lef(puq [n.puq puq.lef r.puq])))
      ++  rigt
        ?>  ?=(^ puq)
        ^+(. =+(rig=apse(puq r.puq) rig(puq [n.puq l.puq puq.rig])))
      --
    ::
    ++  wack                                            ::    wack:pu
      |=  now/@da                                       ::  wakeup (timeout)
      ^-  {(list rock) _+>}
      ?.  &(!=(~ rtn) ?>(?=(^ rtn) (gte now u.rtn)))  [~ +>]
      ::  ~&  [%slow (div rto (div ~s1 1.000))]
      =.  +>  (wept 0 nus)
      ?>  =(0 nif)
      =+  oub=(gte rto ~s16)
      =:  caw  2
          rto  ;:  min
                 (mul 2 rto)
                 ~m15
                 (mul ~m1 ?~(rue 1 +((div (sub now u.rue) ~d1))))
               ==
        ==
      (harv now)
    ::
    ++  wept                                            ::    wept:pu
      |=  {fip/@ud lap/@ud}                             ::  fip thru lap-1
      =<  abet  =<  apse
      |%
      ++  abet  +>.$
      ++  apse
        ^+  .
        ?~  puq  .
        ?:  (lth p.n.puq fip)  ?~(l.puq . left)
        ?:  (gte p.n.puq lap)  ?~(r.puq . rigt)
        =>  rigt  =<  left
        ?>  ?=(^ puq)
        ?.(liv.q.n.puq . .(nif (dec nif), liv.q.n.puq |))
      ::
      ++  left
        ?>  ?=(^ puq)
        ^+(. =+(lef=apse(puq l.puq) lef(puq [n.puq puq.lef r.puq])))
      ++  rigt
        ?>  ?=(^ puq)
        ^+(. =+(rig=apse(puq r.puq) rig(puq [n.puq l.puq puq.rig])))
      --
    ::
    ++  whap                                            ::    whap:pu
      |=  {now/@da gom/soup wyv/(list rock)}            ::  send a message
      ^-  {(list rock) _+>}
      =.  pyz  (~(put by pyz) gom (lent wyv))
      =.  +>
        |-  ^+  +>.^$
        ?~  wyv  +>.^$
        %=  $
          wyv  t.wyv
          nus  +(nus)
          diq  (~(put by diq) (shaf %flap i.wyv) nus)
          puq  (~(put to puq) [nus `soul`[gom 0 | ~2000.1.1 i.wyv]])
        ==
      (harv now)
    --
  --
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::              section 4aG, protocol engine          ::
  ::
  |%
  ++  am                                                ::    am
    |_  {now/@da fox/fort}                              ::  protocol engine
    ++  boot                                            ::    boot:am
      ^-  fort                                          ::  restore from noun
      %=    fox
          urb.ton
        %-  ~(gas by *(map ship sufi))
        %+  turn
          (~(tap by urb.ton.fox) ~)
        |=  {p/ship q/sufi}  ^-  {p/ship q/sufi}
        :-  p
        %=    q
            val
          (turn val.q |=({p/life q/ring r/acru} [p q (nol:nu:crub q)]))
        ==
      ==
    ++  come                                            ::    come:am
      |=  {ges/(unit @t) wid/@ bur/@ fak/?}            ::  instantiate pawn
      ^-  {p/{p/ship q/@uvG} q/fort}
      =+  loy=(pit:nu:crub wid bur)
      =+  rig=sec:ex:loy
      =+  our=`@p`fig:ex:loy
      =+  syp=[[0 ~ our now] [%en %pawn ges] pub:ex:loy]
      :-  [our pac:ex:loy]
      %_    fox
          ton
        %^    ~(ha go ton.fox)
            our
          `mace`[[0 rig] ~]
        `will`[[(sign:as:loy *@ (shaf %self (sham syp))) syp fak] ~]
          fak.ton
        fak
      ==
    ::
    ++  czar  !:                                        ::    czar:am
      |=  {our/ship ger/@uw fak/?}                      ::  instantiate emperor
      ^-  {p/(list boon) q/fort}
      =+  loy=?:(fak (pit:nu:crub 512 our) (pit:nu:crub 512 ger)) ::  fake uses carrier /
      =+  fim==(fig:ex:loy (zeno our))
      ?:  &(!fak !fim)  !!                              ::  not fake & bad fig
      =+  mac=`mace`[[0 sec:ex:loy] ~]
      =+  syp=`step`[`bray`[0 ~ our now] [%en %czar ~] pub:ex:loy]
      =+  ded=`deed`[(sign:as:loy *@ (shaf %self (sham syp))) syp fak]
      =+  buq=`buck`[mac [ded ~]]
      =:  ton.fox  (~(ha go ton.fox) our buq)
          zac.fox  (~(put by zac.fox) our *corn)
          fak.ton.fox  fak
        ==
      [[[%beer our pac:ex:loy] ~] fox]
    ::
    ++  gnaw                                            ::    gnaw:am
      |=  {kay/cape ryn/lane pac/rock}                  ::  process packet
      ^-  {p/(list boon) q/fort}
      ?.  =(6 (end 0 3 pac))  [~ fox]
      =+  kec=(bite pac)
      ?:  (goop p.p.kec)  [~ fox]
      ?.  (~(has by urb.ton.fox) q.p.kec)
        [~ fox]
      =<  zork
      =<  zank
      ::  ~&  [%hear p.p.kec ryn `@p`(mug (shaf %flap pac))]
      %-  ~(chew la:(ho:(um q.p.kec) p.p.kec) kay ryn %none (shaf %flap pac))
      [q.kec r.kec]
    ::
    ++  goop                                            ::  blacklist
      |=  him/ship
      |
    ::
    ++  hall                                            ::    hall:am
      ^-  (list sock)                                   ::  all sockets
      =|  sox/(list sock)                               ::  XX hideous
      |-  ^+  sox
      ?~  zac.fox  sox
      =.  sox  $(zac.fox l.zac.fox)
      =.  sox  $(zac.fox r.zac.fox)
      |-  ^+  sox
      ?~  wab.q.n.zac.fox  sox
      =.  sox  $(wab.q.n.zac.fox l.wab.q.n.zac.fox)
      =.  sox  $(wab.q.n.zac.fox r.wab.q.n.zac.fox)
      [[p.n.zac.fox p.n.wab.q.n.zac.fox] sox]
    ::
    ++  have                                            ::    have:am
      |=  {our/ship buq/buck}                           ::  acquire license
      ^-  {p/(list boon) q/fort}
      =:  ton.fox  (~(ha go ton.fox) our buq)
          zac.fox  (~(put by zac.fox) our *corn)
        ==
      [[[%beer our pac:ex:q:sen:(need (~(us go ton.fox) our))] ~] fox]
    ::
    ++  kick                                            ::    kick:am
      |=  hen/duct                                      ::  refresh net
      =+  aks=(turn (~(tap by urb.ton.fox) ~) |=({p/ship q/sufi} p))
      |-  ^-  {p/(list boon) q/fort}
      ?~  aks  [~ fox]
      =^  buz  fox  zork:(kick:(um i.aks) hen)
      =^  biz  fox  $(aks t.aks)
      [(weld p.buz p.biz) fox]
    ::
    ++  rack                                            ::    rack:am
      |=  {soq/sock cha/path cop/coop}                  ::  e2e ack
      =+  oh=(ho:(um p.soq) q.soq)
      =^  gud  oh  (cook:oh cop cha ~)
      ?.  gud  oh
      (cans:oh cha)
    ::
    ++  wake                                            ::    wake:am
      |=  hen/duct                                      ::  harvest packets
      ^-  {p/(list boon) q/fort}
      =+  sox=hall
      =|  bin/(list boon)
      |-  ^-  {p/(list boon) q/fort}
      ?~  sox
        =^  ban  fox  (kick hen)
        [(weld bin p.ban) fox]
      =^  bun  fox  zork:zank:thaw:(ho:(um p.i.sox) q.i.sox)
      $(sox t.sox, bin (weld p.bun bin))
    ::
    ++  wise                                            ::    wise:am
      |=  {soq/sock hen/duct cha/path val/* ete/?}      ::  send a statement
      ^-  {p/(list boon) q/fort}
      zork:zank:(wool:(ho:(um p.soq) q.soq) hen cha val ete)
    ::
    ++  um                                              ::  per server
      |=  our/ship
      =+  gus=(need (~(us go ton.fox) our))
      =+  ^=  weg  ^-  corn
          =+  weg=(~(get by zac.fox) our)
          ?^(weg u.weg *corn)
      =|  bin/(list boon)
      |%
      ++  ho                                            ::    ho:um:am
        |=  her/ship                                    ::  per friend
        =+  diz=(myx:gus her)
        =+  bah=(~(get by wab.weg) her)
        =+  puz=?~(bah ahoy:pu %*(. pu +< sop.u.bah))
        =>  .(bah `bath`?~(bah [abet:puz ~ ~] u.bah))
        |%
        ++  busk                                        ::    busk:ho:um:am
          |=  {waz/(list ship) pax/(list rock)}         ::  send packets
          %_    +>
              bin
            |-  ^+  bin
            ?~  pax  bin
            $(pax t.pax, bin (weld (flop (wist:diz now waz ~ i.pax)) bin))
          ==
        ::
        ++  cans                                        ::    cans:ho:um:am
          |=  cha/path
          =+  rum=(need (~(get by raz.bah) cha))
          =.  rum
            %=  rum
              did  +(did.rum)
              mis  (~(del by mis.rum) did.rum)
            ==
          (coat cha rum)
        ::
        ++  coat                                        ::    coat:ho:um:am
          |=  {cha/path rum/race}                       ::  update input race
          ^+  +>
          =+  cun=(~(get by mis.rum) did.rum)
          ?~  cun
            +>.$(raz.bah (~(put by raz.bah) cha rum))
          ?.  =(%good p.u.cun)  +>.$
          ?>  ?=(^ s.u.cun)
          %=    +>.$
              raz.bah  (~(put by raz.bah) cha rum(dod |))
              bin
            :_  bin
            :^    %mulk
                [our her]
              `soap`[[p:sen:gus clon:diz] cha did.rum]
            u.s.u.cun
          ==
        ::
        ++  cook                                        ::    cook:ho:um:am
          |=  {cop/coop cha/path ram/(unit {ryn/lane dam/flap})}
          ^-  {gud/? con/_+>}                        ::  acknowledgment
          ::  ~&  [%cook cop num cha ram]
          =+  rum=(need (~(get by raz.bah) cha))
          =+  lat=(~(get by mis.rum) did.rum)
          ?:  &(?=($~ lat) ?=($~ ram))
            ~&  %ack-late-or-redundant
            [%| +>.$]
          :-  %&
          =+  ^-  {ryn/lane dam/flap}
              ?^  ram  [ryn.u.ram dam.u.ram]
              ?<  ?=($~ lat)
              [q r]:u.lat
          =.  raz.bah
            ?^  ram  raz.bah
            %+  ~(put by raz.bah)  cha
            rum(dod &, bum ?~(cop bum.rum (~(put by bum.rum) did.rum u.cop)))
          =^  roc  diz  (zuul:diz now [%buck cop dam ~s0])
          (busk(diz (wast:diz ryn)) xong:diz roc)
        ::
        ++  done                                        ::    done:ho:um:am
          |=  {cha/path num/@ud}                        ::  complete outgoing
          ^-  {(unit duct) _+>}
          =+  rol=(need (~(get by ryl.bah) cha))
          =+  rix=(~(get by san.rol) num)
          ?~  rix  [~ +>.$]
          :-  rix
          %_    +>.$
              ryl.bah
            (~(put by ryl.bah) cha rol(san (~(del by san.rol) num)))
          ==
        ::
        ++  la                                          ::    la:ho:um:am
          |_  {kay/cape ryn/lane aut/skin dam/flap}     ::  per packet
          ::
          ++  chew                                      ::    chew:la:ho:um:am
            |=  {sin/skin msg/@}                        ::  receive
            ^+  +>
            =<  apse
            |%
            ++  apse
              ^+  +>.$
              =+  oub=bust:puz
              =+  neg==(~ yed.caq.dur.diz)
              =.  +>.$  east
              =+  eng==(~ yed.caq.dur.diz)
              =+  bou=bust:puz
              =.  bin
                ?.  &(oub !bou)  bin
                :_(bin [%wine [our her] " is ok"])
              =.  bin
                ?.  &(neg !eng)  bin
                :_(bin [%wine [our her] " is your neighbor"])
              +>.$
            ::
            ++  east
              ^+  +>.$
              ?-    sin
                  $none
                ::  ~&  %chew-none
                =.  puz  (bilk:puz now)
                (chow ((hard meal) (cue msg)))
              ::
                  $fast
                ::  ~&  %chew-fast
                =+  [mag=`hand`(end 7 1 msg) bod=(rsh 7 1 msg)]
                =+  dey=(kuch:diz mag)
                ?~  dey
                  ::  ~&  [%bad-key her mag]
                  +>.$                           ::  ignore unknown key
                =.  puz  (bilk:puz now)
                =^  key  diz  u.dey
                (chow(aut sin) ((hard meal) (cue (dy:q:sen:gus key bod))))
              ::
                  $full
                ::  ~&  %chew-full
                =+  mex=((hard {p/{p/life q/life} q/will r/@}) (cue msg))
                =.  diz  (deng:diz q.mex)
                =+  wug=cluy:diz
                ?>  =(q.p.mex p.wug)
                =+  gey=(sev:gus p.p.mex)
                =+  mes=(need (tear:as:q.gey pub:ex:r.wug r.mex))
                =.  diz  (wasc:diz p.mes)
                =.  puz  (bilk:puz now)
                (west(msg q.mes))
              ::
                  $open
                ::  ~&  %chew-open
                =+  mex=((hard {p/{$~ q/life} q/will r/@}) (cue msg))
                =.  diz  (deng:diz q.mex)
                =+  wug=cluy:diz
                ?>  =(q.p.mex p.wug)
                =+  mes=(need (sure:as:r.wug *code r.mex))
                =.  puz  (bilk:puz now)
                (west(msg mes))
              ==
            ++  west
              |=  $~
              =+  vib=(cue msg)
              =+  mal=(meal vib)
              ?.  =(mal vib)
                ~&  [%bad-meal her]
                +>.^$
              (chow(aut sin) mal)
            --
          ::
          ++  chow                                      ::    chow:la:ho:um:am
            |=  fud/meal                                ::  interpret meal
            ^+  +>
            =.  diz  ?:(=(%none aut) diz (wast:diz ryn))
            (dine fud)
          ::
          ++  cock                                      ::    cock:la:ho:um:am
            ^+  .                                       ::  acknowledgment
            ::  ~&  [%back kay dam]
            =^  pax  diz  (zuul:diz now [%back kay dam ~s0])
            +(+> (busk(diz (wast:diz ryn)) xong:diz pax))
          ::
          ++  coot                                      ::    coot:la:ho:um:am
            |=  {cha/path rum/race}                     ::  update input race
            ^+  +>
            =+  cun=(~(get by mis.rum) did.rum)
            ?~  cun
              +>.$(raz.bah (~(put by raz.bah) cha rum))
            =.  +>.$  cock(kay p.u.cun, dam r.u.cun)
            =.  +>.$  ?.  =(%good p.u.cun)  +>.$
                      ?>  ?=(^ s.u.cun)
                      %-  emit
                      ^-  boon
                      :^    %milk
                          [our her]
                        `soap`[[p:sen:gus clon:diz] cha did.rum]
                      u.s.u.cun
            %=  $
              mis.rum  (~(del by mis.rum) did.rum)
              did.rum  +(did.rum)
            ==
          ::
          ++  dear                                      ::    dear:la:ho:um:am
            |=  {cha/path num/@ud dut/(unit)}           ::  interpret message
            ^+  +>
            =+  ^=  rum  ^-  race
                =+  rum=(~(get by raz.bah) cha)
                ?~(rum *race u.rum)
            ?.  (gte num did.rum)
              cock                                      ::  always ack a dup
            (coot cha rum(mis (~(put by mis.rum) num [kay ryn dam dut])))
          ::
          ++  deer                                      ::    deer:la:ho:um:am
            |=  {cha/path num/@ud dut/(unit)}           ::  interpret message
            ^+  +>
            =+  rum=(fall (~(get by raz.bah) cha) *race)
            %=    +>.$
                +>
              ?.  (gte num did.rum)                     ::  always ack a dup
                ::  ~&  [%deer-1 num]
                con:(cook (~(get by bum.rum) num) cha ~ ryn dam)
              ?:  dod.rum
                ::  ~&  [%deer-2 num]
                (coat cha rum(mis (~(put by mis.rum) num [kay ryn dam dut])))
              ::  ~&  [%deer-3 num]
              %=    +>.+>.$
                  raz.bah
                %+  ~(put by raz.bah)  cha
                rum(mis (~(put by mis.rum) num [kay ryn dam dut]))
              ==
            ==
          ::
          ++  dine                                      ::    dine:la:ho:um:am
            |=  fud/meal                                ::  interpret meal
            ^+  +>
            ?-    -.fud
                $back
              =.  +>  ?.(=(%full aut) +> cock)          ::  finish key exch
              +>(..la (tuck p.fud q.fud r.fud))
            ::
                $buck
              =.  +>  ?.(=(%full aut) +> cock)          ::  finish key exch
              +>(..la (tock p.fud q.fud r.fud))
            ::
                $bond
              ::  ~&  [%bond q.fud r.fud]
              ?>  =(p:sen:gus p.fud)
              (dear q.fud r.fud ?-(kay $dead ~, $good [~ s.fud]))
            ::
                $bund
              ::  ~&  [%bund q.fud r.fud]
              ?>  =(p:sen:gus p.fud)
              (deer q.fud r.fud ?-(kay $dead ~, $good [~ s.fud]))
            ::
                $carp
              ::  =+  zol=(~(get by olz.weg) s.fud)
              ::  ?^  zol  cock(kay u.zol)
              =^  neb  nys.weg
                  =+  neb=(~(get by nys.weg) s.fud)
                  ?^  neb  [u.neb nys.weg]
                  =+  neb=`bait`[(kins p.fud) 0 r.fud ~]
                  [neb (~(put by nys.weg) s.fud neb)]
              ?>  (lth q.fud p.r.neb)
              ?>  =((kins p.fud) p.neb)
              ?>  =(r.fud p.r.neb)
              =+  doy=`(unit @)`(~(get by q.r.neb) q.fud)
              ?^  doy  cock
              =>  ^+  .   %=  .
                    q.r.neb  (~(put by q.r.neb) q.fud t.fud)
                    q.neb    +(q.neb)
                  ==
              ::  ~&  [%carp q.fud s.fud q.neb p.r.neb]
              ?:  =(q.neb p.r.neb)
                =:  nys.weg  (~(del by nys.weg) s.fud)
                ::  olz.weg  (~(put by olz.weg) s.fud kay)
                  ==
                (golf p.neb r.neb)
              =.  +>.$  cock
              +>.$(nys.weg (~(put by nys.weg) s.fud neb))
            ::
                $fore
              =+  ^=  lyn  ^-  lane
                  ?~  q.fud  ryn
                  ?.  ?=($if -.u.q.fud)  u.q.fud
                  [%ix +.u.q.fud]
                  ::  u.q.fud
              ?:  =(our p.fud)
                (emit %mead lyn r.fud)
              =+  zid=(myx:gus p.fud)
              (emir (wist:zid now xong:zid [~ lyn] r.fud))
            ==
          ::
          ++  emir                                      ::    emir:la:ho:um:am
            |=  ben/(list boon)                         ::  emit boons
            ^+  +>
            ?~(ben +> $(ben t.ben, bin [i.ben bin]))
          ::
          ++  emit                                      ::    emit:la:ho:um:am
            |=  bun/boon                                ::  emit a boon
            +>(bin [bun bin])
          ::
          ++  golf                                      ::    golf:la:ho:um:am
            |=  {sin/skin duv/dove}                     ::  assemble fragments
            ^+  +>
            %+  chew  sin
            =+  [nix=0 rax=*(list @)]
            |-  ^-  @
            ?:  =(p.duv nix)
              (can ?:(fak.ton.fox 16 13) (turn (flop rax) |=(a/@ [1 a])))
            $(nix +(nix), rax [(need (~(get by q.duv) nix)) rax])
          --                                            ::  --la:ho:um:am
        ::
        ++  pong                                        ::    pong:ho:um:am
          |=  hen/duct                                  ::  test connection
          ^+  +>
          ?.  ?&  =(~ puq.puz)
                  ?|  bust:puz
                      ?=($~ rue.puz)
                      (gth now (add ~s32 u.rue.puz))
                      (lth u.rue.puz hop.fox)
                  ==
              ==
            +>.$
          (wool [/a/ping hen] /q/pi ~ |)
        ::
        ++  thaw                                        ::    thaw:ho:um:am
          ^+  .                                         ::  wakeup bomb
          =+  oub=bust:puz
          =^  yem  puz  (wack:puz now)
          =+  bou=bust:puz
          =.  bin
              ?.  &(bou !oub)  bin
              :_(bin [%wine [our her] " not responding still trying"])
          =.  diz  ?:((boom:puz now) (pode:diz now) diz)
          (busk xong:diz yem)
        ::
        ++  tock                                        ::    tock:ho:um:am
          |=  {cop/coop fap/flap cot/@dr}               ::  e2e ack by hash
          ^+  +>
          =^  yoh  puz  (bick:puz now fap)
          =.  +>.$
            ?~  p.yoh  +>.$
            =^  hud  +>.$
              (done p.u.p.yoh q.u.p.yoh)
            ?~  hud  +>.$
            %=    +>.$
                bin
              :_  bin
              `boon`[%cake [our her] [[p:sen:gus clon:diz] u.p.yoh] cop u.hud]
            ==
          (busk xong:diz q.yoh)
        ::
        ++  tuck                                        ::    tuck:ho:um:am
          |=  {kay/cape fap/flap cot/@dr}               ::  ack by hash
          ^+  +>
          =^  yoh  puz  (bick:puz now fap)
          =.  +>.$
            ?~  p.yoh  +>.$
            =^  hud  +>.$
              (done p.u.p.yoh q.u.p.yoh)
            ?~  hud  +>.$
            %=    +>.$
                bin
              :_  bin
              `boon`[%coke [our her] [[p:sen:gus clon:diz] u.p.yoh] kay u.hud]
            ==
          (busk xong:diz q.yoh)
        ::
        ++  wind                                        ::    wind:ho:um:am
          |=  {gom/soup ham/meal}
          ::  ~&  [%wind her gom]
          ^+  +>
          =^  wyv  diz  (zuul:diz now ham)
          =^  feh  puz  (whap:puz now gom wyv)
          (busk xong:diz feh)
        ::
        ++  wool                                        ::    wool:ho:um:am
          |=  {hen/duct cha/path val/* ete/?}           ::  send a statement
          ^+  +>
          =+  ^=  rol  ^-  rill
              =+  rol=(~(get by ryl.bah) cha)
              ?~(rol *rill u.rol)
          =+  sex=sed.rol
          ::  ~&  [%tx [our her] cha sex]
          =.  ryl.bah
              %+  ~(put by ryl.bah)  cha
              rol(sed +(sed.rol), san (~(put by san.rol) sex hen))
          =+  cov=[p=p:sen:gus q=clon:diz]
          %+  wind  [cha sex]
          ?:  ete
            [%bund q.cov cha sex val]
          [%bond q.cov cha sex val]
        ::
        ++  zest                                        ::    zest:ho:um:am
          :~  :~  :*  [%rtt rtt.sop.bah]
                      [%rto rto.sop.bah]
                      [%rtn rtn.sop.bah]
                      [%rue rue.sop.bah]
                  ==
                  :*  [%nus nus.sop.bah]
                      [%nif nif.sop.bah]
                      [%nep nep.sop.bah]
                      [%caw caw.sop.bah]
                      [%cag cag.sop.bah]
                  ==
                  =+  qup=(~(tap to puq.sop.bah) ~)
                  :-  %qup
                  %+  turn  qup
                  |=  {a/@ud b/soul}
                  :*  a
                      nux.b
                      liv.b
                      lys.b
                      `@p`(mug (shaf %flap pac.b))
                      gom.b
                  ==
              ==
          ::
              :-  %raz
              =+  zar=(~(tap by raz.bah) ~)
              %+  turn  zar
              |=  {a/path b/race}
              :+  a
                did.b
              =+  ciy=(~(tap by mis.b) ~)
              %+  turn  ciy
              |=  {c/@ud d/{p/cape q/lane r/flap s/(unit)}}
              [c p.d r.d]
          ::
              [%ryl (~(tap to ryl.bah) ~)]
              [%lun lun.wod.dur.diz]
              [%caq caq.dur.diz]
              [%lew lew.wod.dur.diz]
          ==
        ::
        ++  zank                                        ::    zank:ho:um:am
          %=  +>.$                                      ::  resolve
            gus      (nux:gus diz)
            wab.weg  (~(put by wab.weg) her bah(sop abet:puz))
          ==
        --                                              ::  --ho:um:am
      ::
      ++  kick                                          ::    kick:um:am
        |=  hen/duct                                    ::  test connection
        ^+  +>
        =+  hoy=hoy.saf.gus
        |-  ^+  +>.^$
        ?~  hoy
          +>.^$
        $(hoy t.hoy, +>.^$ (pong i.hoy hen))
      ::
      ++  pals                                          ::    pals:um:am
        ^-  (list @p)                                   ::  active neighbors
        %+  turn
          %+  skim  (~(tap by wab.weg) ~)
          |=  {a/ship b/bath}
          !(~(boom pu sop.b) now)
        |=({a/ship b/bath} a)
      ::
      ++  pong                                          ::    pong:um:am
        |=  {her/ship hen/duct}                         ::  test neighbor
        ^+  +>
        zank:(pong:(ho her) hen)
      ::
      ++  zork                                          ::    zork:um:am
        ^-  {p/(list boon) q/fort}                      ::  resolve
        :-  (flop bin)
        %_  fox
          ton  (~(su go ton.fox) gus)
          zac  (~(put by zac.fox) our.gus weg)
        ==
      --                                                ::  --um:am
    --                                                  ::  --am
  --
  .  ==
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::              section 4aH, protocol vane            ::
  ::
  =|  $:  fox/fort                                      ::  kernel state
      ==                                                ::
  |=  {now/@da eny/@ ski/sley}                          ::  current invocation
  ^?                                                    ::  opaque core
  =<
    |%                                                  ::  vane interface
    ++  call                                            ::  handle request
      |=  $:  hen/duct
              hic/(hypo (hobo kiss-ames))
          ==
      =>  %=    .                                       ::  XX temporary
              q.hic
            ^-  kiss-ames
            ?:  ?=($soft -.q.hic)
              ((hard kiss-ames) p.q.hic)
            ?:  (~(nest ut -:!>(*kiss-ames)) | p.hic)  q.hic
            ~&  [%ames-call-flub (@tas `*`-.q.hic)]
            ((hard kiss-ames) q.hic)
          ==
      ^-  {p/(list move) q/_..^$}
      =^  duy  ..knob
        (knob hen q.hic)
      [duy ..^$]
    ::
    ++  doze
      |=  {now/@da hen/duct}
      =+  doz=`(unit @da)`[~ (add now ~s32)]
      |-  ^+  doz
      ?~  zac.fox  doz
      =.  doz  $(zac.fox l.zac.fox)
      =.  doz  $(zac.fox r.zac.fox)
      =+  yem=q.n.zac.fox
      |-  ^+  doz
      ?~  wab.yem  doz
      =.  doz  $(wab.yem l.wab.yem)
      =.  doz  $(wab.yem r.wab.yem)
      =+  bah=q.n.wab.yem
      (hunt doz rtn.sop.bah)
    ::
    ++  load
      |=  old/fort
      ^+  ..^$
      ~&  %ames-reload
      ..^$(fox old)
    ::
    ++  scry
      |=  {fur/(unit (set monk)) ren/@tas who/ship syd/desk lot/coin tyl/path}
      ^-  (unit (unit cage))
      ?~  tyl  [~ ~]
      =+  hun=(slaw %p i.tyl)
      ?~  hun  [~ ~]
      ?.  =(`@`0 ren)  ~
      ?+    lot  ~
          {$$ $ud @}
        (perm who u.hun q.p.lot [syd t.tyl])
      ::
          {$$ $da @}
        ?.  =(now q.p.lot)  ~
        (temp who u.hun [syd t.tyl])
      ==
    ::
    ++  stay  fox
    ++  take                                            ::  accept response
      |=  {tea/wire hen/duct hin/(hypo sign)}
      ^-  {p/(list move) q/_..^$}
      =^  duy  ..knap
        (knap tea hen q.hin)
      [duy ..^$]
    --
  |%
  ++  claw  |=(our/ship ^-(duct hen:(need (~(get by zac.fox) our))))
  ++  clop
    |=  {now/@da hen/duct bon/boon}
    ^-  {(list move) fort}
    ?-    -.bon
        $beer
      :_  fox(zac (~(put by zac.fox) p.bon `corn`[hen ~ ~ ~]))
      :*  [hen [%slip %c %init p.bon]]
          [hen [%give %init p.bon]]
          [hen [%slip %a %kick now]]
          [hen [%slip %e %init p.bon]]
          [hen [%slip %g %init p.bon]]
          [hen [%slip %d %init p.bon]]                  ::  must be after gall
          ~
      ==
    ::
        $cake
      :_  fox
      :~  [s.bon %give %woot q.p.bon r.bon]
      ==
    ::
        $coke
      ::  ~&  [%tz p.bon q.bon r.bon]
      :_  fox
      :~  [s.bon %give %went q.p.bon r.bon]
      ==
    ::
        $mead  :_(fox [[hen [%give %hear p.bon q.bon]] ~])
        $milk
      ::  ~&  [%rx p.bon q.bon]
      ?>  ?=({@ *} q.q.bon)
      ?:  ?=($e i.q.q.bon)
        :_(fox [hen [%slip %e %wart p.bon %$ t.q.q.bon r.bon]]~)
      ?:  ?=($r i.q.q.bon)
        ?:  ?=({$ta *} t.q.q.bon)
          =+  wil=((hard (unit will)) r.bon)
          :_  fox
          =+  ^=  pax
              :+  (scot %p p.p.bon)
                (scot %p q.p.bon)
              q.q.bon
          [hen %pass pax %g %west p.bon /ge/hood 0 %m %will wil]~
        ?>  ?=({@ @ *} t.q.q.bon)
        :_  fox
        =+  [cak=i.t.q.q.bon ven=i.t.t.q.q.bon]
        :_  ~
        =+  neh=(claw p.p.bon)
        ?>  ?=(^ neh)
        ?:  ?=($e ven)
          ?+  cak  !!                 ::  XX  fix eyre
            $pr  :_  [%sick %waft p.bon r.bon]
                 [[`path`t.t.t.q.q.bon] hen] 
            $pc  :_  [%sick %wart p.bon cak `path`t.t.t.q.q.bon r.bon]
                 [[%e `path`t.t.t.q.q.bon] hen]
          ==
        =+  ton=[%waft p.bon r.bon]
        ::  ~&  [%milk-waft [[ven `path`t.t.t.q.q.bon] t.neh]]
        :_  [%sick ton]
        ?:  =(%c ven)
          ?>  =(%re cak)
          [[%c `path`t.t.t.q.q.bon] hen]
        [[ven `path`t.t.t.q.q.bon] t.neh]
      ?>  ?=($q i.q.q.bon)
      ?>  ?=({@ *} t.q.q.bon)
      ?+    i.t.q.q.bon
        :_  fox
        :~  :-  (claw p.p.bon)
            [%sick %wart p.bon i.t.q.q.bon t.t.q.q.bon r.bon]
        ==
      ::
          $pi                                           ::  ping
        [~ fox]
        ::  $(bon [%wine p.bon " sent a ping at {(scow %da now)}"])
      ::
          ?($pr $pc)                                    ::    %pr, %pc
        :_  fox
        :~  [hen [%slip %e %wart p.bon i.t.q.q.bon t.t.q.q.bon r.bon]]
        ==
      ::
          $re                                           ::    %re
        :_  fox
        :~  [hen [%slip %c %wart p.bon i.t.q.q.bon t.t.q.q.bon r.bon]]
        ==
      ==
    ::
        $mulk
      ::  ~&  [%mulk p.bon q.bon]
      ?>  ?=({@ @ *} q.q.bon)
      ?>  ?=(?($a $c $e $g) i.q.q.bon)
      =+  pax=[(scot %p p.p.bon) (scot %p q.p.bon) q.q.bon]
      :_  fox  [hen %pass pax i.q.q.bon %west p.bon t.q.q.bon r.bon]~
    ::
        $ouzo
      ::  ~&  [%send now p.bon `@p`(mug (shaf %flap q.bon))] 
      :_  fox
      [[gad.fox [%give %send p.bon q.bon]] ~]
    ::
        $wine
      :_  fox
      =+  fom=~(rend co %$ %p q.p.bon)
      :~  :-  hen
          :+  %slip  %d
          :+  %flog  %text
          ;:  weld
            "; "
            fom
            q.bon
          ==
      ==
    ==
  ::
  ++  knap
    |=  {tea/wire hen/duct sih/sign}
    ^-  {(list move) _+>}
    ?-  +<.sih
        $crud  [[[hen [%slip %d %flog +.sih]] ~] +>]
        $went  [~ +>]
        $mack  ?~  +>.sih  $(sih [%g %nice ~])          ::  XX using old code
               $(sih [%g %mean `[%mack +>+.sih]])
        $unto  [~ +>]
        ?($mean $nice)                                  ::  XX obsolete
      ?:  ?=({$ye $~} tea)
        [~ +>.$]
      ?>  ?=({@ @ @ *} tea)
      =+  soq=[(slav %p i.tea) (slav %p i.t.tea)]
      =+  pax=t.t.tea
      =+  ^=  fuy
          =<  zork  =<  zank
          %^  ~(rack am [now fox])  soq  pax
          ::  ~&  [%knap-ack ?-(+<.sih %mean `p.+.sih, %nice ~)]
          ?-(+<.sih $mean `p.+.sih, $nice ~)
      =>  %_(. fox q.fuy)
      =|  out/(list move)
      |-  ^-  {p/(list move) q/_+>.^$}
      ?~  p.fuy
        [(flop out) +>.^$]
      =^  toe  fox  (clop now hen i.p.fuy)
      $(p.fuy t.p.fuy, out (weld (flop toe) out))
    ==
  ::
  ++  knob
    |=  {hen/duct kyz/kiss-ames}
    ^-  {(list move) _+>}
    ?:  ?=($crud -.kyz)
      [[[hen [%slip %d %flog kyz]] ~] +>]
    ?:  ?=($west -.kyz)
      ?>  ?=({$ta $~} q.kyz)
      =+  gox=((hard {p/@p q/@pG r/gens s/pass}) r.kyz)
      =+  gus=(need (~(us go ton.fox) p.p.kyz))
      =^  wyl  gus  (born:gus now gox)
      =.  ton.fox  (~(su go ton.fox) gus)
      :_  +>.$
      =+  ^=  pax
          :+  (scot %p p.p.kyz)
            (scot %p q.p.kyz)
          q.kyz
      [hen %pass pax %g %deal p.kyz %hood %poke %will !>(wyl)]~
    ?:  ?=($wegh -.kyz)
      ~&  %ames-weighing
      [[hen %give %mass wegh]~ +>]
    =+  ^=  fuy  
        ^-  {p/(list boon) q/fort}
        ?-    -.kyz
            $barn
          [~ fox(gad hen)]
            $cash
          (~(have am [now fox]) p.kyz q.kyz)
        ::
            $hear
          (~(gnaw am [now fox]) %good p.kyz q.kyz)
        ::
            $hole
          (~(gnaw am [now fox]) %dead p.kyz q.kyz)
        ::
            $junk
          [~ fox(any.ton (shax (mix any.ton.fox p.kyz)))]
        ::
            $kick
          (~(kick am [now fox(hop p.kyz)]) hen)
        ::
            $make
          =+  vun=(~(come am [now fox]) p.kyz (bex q.kyz) r.kyz s.kyz)
          [[[%beer p.vun] ~] q.vun]
        ::
            $sith
          (~(czar am [now fox]) p.kyz q.kyz r.kyz)
        ::
            $wake
          (~(wake am [now fox]) hen)
        ::
            $want
          (~(wise am [now fox]) p.kyz hen q.kyz r.kyz |)
        ::
            $wont
          (~(wise am [now fox]) p.kyz hen q.kyz r.kyz &)
        ==
    =>  %_(. fox q.fuy)
    =|  out/(list move)
    |-  ^-  {p/(list move) q/_+>.^$}
    ?~  p.fuy
      [(flop out) +>.^$]
    =^  toe  fox  (clop now hen i.p.fuy)
    $(p.fuy t.p.fuy, out (weld (flop toe) out))
  ::
  ++  perm
    |=  {our/ship his/ship mar/@ud tyl/path}
    ^-  (unit (unit cage))
    ?~  tyl  ~
    ?:  ?=({$name $~} tyl)
      =+  wul=$(tyl [%will ~])
      ``[%noun !>(?~(wul (scot %p his) (gnow his q.q.q:((hard deed) -.u.wul))))]
    ?:  ?=({$gcos $~} tyl)
      =+  wul=$(tyl [%will ~])
      ?~(wul ~ ``[%noun !>(`gcos`q.q.q:((hard deed) -.u.wul))])
    =+  gys=(~(us go ton.fox) our)
    ?~  gys  ~
    ?.  =(our his)
      ?:  ?=({$will $~} tyl)
        =+  fod=(~(get by hoc.saf.u.gys) his)
        ?~  fod  ~
        %+  bind  (rick mar his lew.wod.u.fod)
        |=(a/will `[%noun !>(a)])
      ?:  ?=({$tick $~} tyl)
        ?.  =(our (sein his))  ~
        ``[%noun !>((end 6 1 (shaf %tick (mix his (shax sec:ex:q:sen:u.gys)))))]
      ~
    ?:  ?=({$buck $~} tyl)
      =+  muc=(rice mar sex:u.gys)
      =+  luw=(rick mar our law.saf.u.gys)
      ?.  &(?=(^ muc) ?=(^ luw))  ~
      ``[%noun !>(`buck`[u.muc u.luw])]
    ?:  ?=({$code $~} tyl)
      ``[%noun !>((end 6 1 (shaf %pass (shax sec:ex:q:sen:u.gys))))]
    ?:  ?=({$will $~} tyl)
      (bind (rick mar our law.saf.u.gys) |=(a/will `[%noun !>(a)]))
    ~
  ::
  ++  temp
    |=  {our/ship his/ship tyl/path}
    ^-  (unit (unit cage))
    ?:  ?=({?($show $tell) *} tyl)
      ?^  t.tyl  [~ ~]
      =+  gys=(~(us go ton.fox) our)
      ?~  gys  [~ ~]
      =+  zet=zest:(ho:(~(um am [now fox]) our) his)
      ``[%noun ?:(=(%show i.tyl) !>(>zet<) !>(zet))]
    ?:  ?=({$pals $~} tyl)
      ?.  =(our his)
        ~
      ``[%noun !>(pals:(~(um am [now fox]) our))]
    ?.  ?=({$life $~} tyl)
      =+  muc=$(tyl [%life ~])
      (perm our his ?~(muc 0 (@ud u.muc)) tyl)
    =+  gys=(~(us go ton.fox) our)
    ?~  gys  ~
    ?.  =(our his)
      =+  fod=(~(get by hoc.saf.u.gys) his)
      ?~  fod  ~
      ?~  lew.wod.u.fod  ~
      ``[%noun !>(`@ud`p.p.q.i.lew.wod.u.fod)]
    ?~  val.saf.u.gys  ~
    ``[%noun !>(`@ud`p.i.val.saf.u.gys)]
  ::
  ++  wegh
    ^-  mass
    :-  %ames
    :-  %|
    :~  fox+[%& fox]
    ==
  --
