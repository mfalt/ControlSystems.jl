## User should just use TransferFunction
immutable SisoAbstract <: SisoTf
    expr::Expr
    function SisoAbstract(expr::Expr)
        new(expr)
    end
end

function SisoAbstract(str::AbstractString)
    expr = parse(str)
    SisoAbstract(forceExpr(expr))
end

forceExpr(expr::Expr) = expr
forceExpr(n::Number) = :($n + 0)

function minreal(sys::SisoAbstract, eps::Real=sqrt(eps()))
    error("minreal is not implemented for abstract transferfunctions")
end

function print_siso(io::IO, t::SisoAbstract, var=:s)
    println(io, t.expr)
end

Base.promote_rule{T<:Real}(::Type{SisoAbstract}, ::Type{T}) = SisoAbstract
Base.convert(::Type{SisoAbstract}, b::Real) = SisoAbstract("$b")

Base.zero(::Type{SisoAbstract}) = SisoAbstract("0")
Base.zero(::SisoAbstract) = Base.zero(SisoAbstract)

Base.length(t::SisoAbstract) = error("length is not implemented for abstract transferfunctions")

Base.num(t::SisoAbstract) = error("num is not implemented for abstract transferfunctions")

Base.den(t::SisoAbstract) = error("den is not implemented for abstract transferfunctions")

#This makes sure that the function can compile once
function _preprocess_for_freqresp(sys::SisoAbstract)
    _f = eval(:(s -> $(sys.expr)))
end

evalfr(f::Function, freq) = f(freq)
evalfr(f::SisoAbstract, freq) = _preprocess_for_freqresp(sys)(freq)

==(t1::SisoAbstract, t2::SisoAbstract) = (t1.expr == t2.expr)

#isapprox(t1::SisoRational, t2::SisoRational) = (t1.num ≈ t2.num && t1.den ≈ t2.den)

+(t1::SisoAbstract, t2::SisoAbstract) = SisoAbstract(:($(t1.expr) + $(t2.expr)))
+(t::SisoAbstract, n::Real) = SisoAbstract(:($(t.expr) + $n))
+(n::Real, t::SisoAbstract) = t + n

-(t1::SisoAbstract, t2::SisoAbstract) = SisoAbstract(:($(t1.expr) - $(t2.expr)))
-(n::Real, t::SisoAbstract) = SisoAbstract(:($n - $(t.expr)))
-(t::SisoAbstract, n::Real) = +(t, -n)

-(t::SisoAbstract) = SisoAbstract(:(- $(t.expr)))

*(t1::SisoAbstract, t2::SisoAbstract) = SisoAbstract(:($(t1.expr) * $(t2.expr)))
*(t::SisoAbstract, n::Real) = SisoAbstract(:($(t.expr) * $n))
*(n::Real, t::SisoAbstract) = *(t, n)

/(n::Real, t::SisoAbstract) = SisoAbstract(:($n / $(t.expr)))
/(t::SisoAbstract, n::Real) = t*(1/n)
/(t1::SisoAbstract, t2::SisoAbstract) = SisoAbstract(:($(t1.expr) / $(t2.expr)))
