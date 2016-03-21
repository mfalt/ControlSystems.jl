ExprLike = Union{Expr,Number,Symbol}

## User should just use TransferFunction
immutable SisoAbstract <: SisoTf
    expr::ExprLike
    function SisoAbstract(expr::ExprLike)
        if isa(expr, Expr) && length(expr.args) == 3 && expr.args[1] == :(+) && expr.args[2] == 0
            #Get rid of the zero
            expr = expr.args[3]
        end
        new(expr)
    end
end

SisoAbstract(str::AbstractString) = SisoAbstract(parse(str))

function minreal(sys::SisoAbstract, eps::Real=sqrt(eps()))
    error("minreal is not implemented for abstract transferfunctions")
end

function print_siso(io::IO, t::SisoAbstract, var=:s)
    println(io, t.expr)
end

Base.promote_rule{T<:Real}(::Type{SisoAbstract}, ::Type{T}) = SisoAbstract
Base.convert(::Type{SisoAbstract}, b::Real) = SisoAbstract(b)

Base.zero(::Type{SisoAbstract}) = SisoAbstract(0)
Base.zero(::SisoAbstract) = Base.zero(SisoAbstract)

Base.length(t::SisoAbstract) = error("length is not implemented for abstract transferfunctions")
Base.num(t::SisoAbstract) = error("num is not implemented for abstract transferfunctions")
Base.den(t::SisoAbstract) = error("den is not implemented for abstract transferfunctions")
pole(t::SisoAbstract) = error("pole is not implemented for abstract transferfunctions")
tzero(t::SisoAbstract) = error("tzero is not implemented for abstract transferfunctions")

#This makes sure that the function can compile once
function _preprocess_for_freqresp(sys::SisoAbstract)
    _f = eval(:(s -> $(sys.expr)))
end

evalfr(f::Function, freq) = f(freq)
evalfr(sys::SisoAbstract, freq) = _preprocess_for_freqresp(sys)(freq)

function lsimabstract(sys::SisoAbstract, uin, dt, Tend)
    #TODO make sure U and P are of equal length, fix input arguments, Tend results in half time, make sure u interp is using Tend
    N = round(Int, Tend/dt) + 1
    #T=N*dt
    T = Tend
    dw = pi/T
    omega = linspace(-pi/dt, pi/dt, 2N+1)
    u = [uin; zeros(N)]
    U = fft(u)
    Pf = _preprocess_for_freqresp(sys)
    P = Complex{Float64}[evalfr(Pf, omega[i]*im) for i in 1:2N]
    y = real(ifft(fftshift(P).*U))
    t = dt*(0:N-1)
    y[1:N]
end

==(t1::SisoAbstract, t2::SisoAbstract) = (t1.expr == t2.expr)

#isapprox(t1::SisoRational, t2::SisoRational) = (t1.num ≈ t2.num && t1.den ≈ t2.den)

+(t1::SisoAbstract, t2::SisoAbstract) = SisoAbstract(:($(t1.expr) + $(t2.expr)))
+(t::SisoAbstract, n::Real) = SisoAbstract(:($(t.expr) + $n))
+(n::Real, t::SisoAbstract) = SisoAbstract(:($n + $(t.expr)))

-(t1::SisoAbstract, t2::SisoAbstract) = SisoAbstract(:($(t1.expr) - $(t2.expr)))
-(n::Real, t::SisoAbstract) = SisoAbstract(:($n - $(t.expr)))
-(t::SisoAbstract, n::Real) = SisoAbstract(:($(t.expr) - $n))

-(t::SisoAbstract) = SisoAbstract(:(- $(t.expr)))

*(t1::SisoAbstract, t2::SisoAbstract) = SisoAbstract(:($(t1.expr) * $(t2.expr)))
*(t::SisoAbstract, n::Real) = SisoAbstract(:($(t.expr) * $n))
*(n::Real, t::SisoAbstract) = SisoAbstract(:($n * $(t.expr)))

/(n::Real, t::SisoAbstract) = SisoAbstract(:($n / $(t.expr)))
/(t::SisoAbstract, n::Real) = SisoAbstract(:($(t.expr) / $n))
/(t1::SisoAbstract, t2::SisoAbstract) = SisoAbstract(:($(t1.expr) / $(t2.expr)))
