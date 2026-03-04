module StreamReals
include("small_reals.jl")

export StreamReal
mutable struct StreamReal <: Real
    exponent::BigInt
    significand::SmallReal
end
const bits_to_show = 20
function Base.show(io::IO, r::StreamReal)
    print(io, "StreamReal( ")
    print(io, toString(head(r.significand)))
    print(io, ".")
    for bit in Lazy.take(bits_to_show, tail(r.significand))
        print(io, toString(bit))
    end 
    print(io, "... * 2^", r.exponent - 1 , " )") 
end 

Base.zero(::Type{<:StreamReal}) = StreamReal(0, zeroes)
Base.zero(::StreamReal) = zero(StreamReal)
Base.one(::Type{<:StreamReal}) = StreamReal(1, One() : zeroes)
Base.one(::StreamReal) = one(StreamReal)

function StreamReal(x::Number)
    x == 0 && return zero(StreamReal) 
    function bits_from_place_n(abs_x::Number, two_to_the_n::Rational)
        if(abs_x >= two_to_the_n)
            Lazy.@lazy One() : bits_from_place_n(abs_x - two_to_the_n, two_to_the_n/2)
        else
            Lazy.@lazy Zero() : bits_from_place_n(abs_x, two_to_the_n/2)
        end
    end
    power_of_2 = exponent(x) + 1
    two_to_the_n = Rational{BigInt}(2)^power_of_2
    abs_significand = bits_from_place_n(abs(x), two_to_the_n)
    significand = x >= 0 ? abs_significand : -abs_significand
    normalize!(StreamReal(power_of_2 + 1, significand))

end

StreamReal(x::StreamReal) = x

Base.:-(r::StreamReal) = StreamReal(r.exponent, -r.significand)

Base.:abs(r::StreamReal) = StreamReal(r.exponent, abs(r.significand))

function normalize!(r::StreamReal)
    normalize_step(first::SignedBit, ::SignedBit) = (first, false)
    normalize_step(::Zero, b::SignedBit) = (b, true)
    normalize_step(::NegOne, b::One) = (NegOne(), true)
    normalize_step(::One, ::NegOne) = (One(), true)
    
    while true
        r.exponent <= 0 && break
        new_head, changed = normalize_step(head(r.significand), head(tail(r.significand)))
        !changed && break
        r.exponent -= 1
        r.significand = new_head:tail(tail(r.significand))
    end
    r
end

Base.:*(r1::StreamReal, r2::StreamReal) = normalize!(StreamReal(r1.exponent + r2.exponent, times(r1.significand, r2.significand)))

function Base.:+(r1::StreamReal, r2::StreamReal)
    (fixed, to_shift) = r1.exponent > r2.exponent ? (r1, r2) : (r2, r1)
    shift_amount = fixed.exponent - to_shift.exponent
    shifted = Lazy.concat(Lazy.take(shift_amount, zeroes), to_shift.significand)
    normalize!(StreamReal(fixed.exponent + 1 , average(fixed.significand, shifted)))
end

Base.:-(r1::StreamReal, r2::StreamReal) = r1 + (-r2)


function concat_all(xs::Lazy.List)
    Lazy.@lazy Lazy.isempty(xs) ? Lazy.list() : 
    Lazy.first(xs) : concat_all(Lazy.tail(xs))
end

end