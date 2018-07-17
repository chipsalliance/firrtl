circuit AddLiterals :
  module AddLiterals :
    input uin : UInt<5>
    input sin : SInt<5>
    output uout : UInt<6>
    output sout : SInt<6>

    node uconst = add(UInt<5>("h1"), UInt<5>("h2"))
    uout <= add(uconst, uin)

    node sconst = add(SInt<5>("h1"), SInt<5>("h-1"))
    sout <= add(sconst, sin)
