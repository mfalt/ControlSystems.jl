module TestAbstractTF
using CustomTest
using ControlSystems

# CONTINUOUS
C_011 = tfa("(s+2)")
C_111 = tfa("(s+1)/(s+2)")
@test C_111*C_011 == tfa("(s+2)*((s+1)/(s+2))")

#We might want to make evalfr scalar
@test C_011(1im) == reshape([2+1im;],1,1)
@test (C_111*C_011)(im) == reshape([1.0+1im],1,1)

@test bode(C_111*C_011, logspace(-1,1)) == bode(tfa("(s+2)*((s+1)/(s+2))"), logspace(-1,1))
end
