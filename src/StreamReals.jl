module Reals
include("smallReals.jl")

mutable struct StreamReal <: Real
    exponent::BigInt
    significand::SmallReal
end

Base.zero(::Type{<:StreamReal}) = StreamReal(0, zeroes)
Base.zero(::StreamReal) = zero(StreamReal)
Base.one(::Type{<:StreamReal}) = StreamReal(1, One() : zeroes)
Base.one(::StreamReal) = one(StreamReal)

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
    shifted = Lazy.prepend(Lazy.repeatedly(Zero(), shift_amount), to_shift.significand)
    normalize!(StreamReal(fixed.exponent, fixed.significand + shifted))
end

Base.:-(r1::StreamReal, r2::StreamReal) = r1 + (-r2)


function concat_all(xs::Lazy.List)
    Lazy.@lazy Lazy.isempty(xs) ? Lazy.list() : 
    Lazy.first(xs) : concat_all(Lazy.tail(xs))
end

end